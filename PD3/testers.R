Rcpp::sourceCpp("./knn.cpp")

# pieciokrotna kroswalidacja

# Funkcja get_partition zwraca k-elementowa partycje zbioru {1,...,n}
# tj. liste k-wektorow, w przyblizeniu tej samej dlugosci
# n - liczba elementow zbioru, dla ktorego bedziemy obliczac partycje
# k - na ile pozbiorow dzielimy zbior k-elementowy
get_partition <- function(n, k) {
  perm <- sample(1:n, n)
  
  part_size <- n %/% k
  modulo <- n %% k
  
  lapply(1:k, 
         function(i) {
           from <- (i - 1) * part_size + 1
           s <- part_size + ifelse(i <= modulo, 1, 0)
           perm[from:(from + s - 1)]
        })
}


# proporcja blednej klasyfikacji
ERR <- function(computed_result, expected_result) {
  sum(computed_result != expected_result) / length(expected_result)
}

# blad bezwzgledny
MAD <- function(computed_result, expected_result) {
  sum(abs(computed_result - expected_result)) / length(expected_result)
}

# blad sredniokwadratowy
MSE <- function(computed_result, expected_result) {
  sum(abs(computed_result - expected_result) ^ 2) / length(expected_result)
}

# obliczanie wartosci bledow dla krosswalidacji
# funkcja zwraca wektor z obliczonymi bledami
# funkcja przyjmuje nastepujace argumenty
# computed_list - lista wektorow z obliczonymi wynikami klasyfikacji dla kazdej z czesci kroswalidacji
# expected_list - lista wektorow z prawidlowymi wynikami klasyfikacji dla kazdej z czesci kroswalidacji
# error_functions - lista funkcji do obliczania bledow (np. ERR, MAD, MSE)
get_error_vector_for_crossvalidation <- function(computed_list, expected_list, error_functions) {
  
  get_error_result <- function(error_fun, c, e) {
    mean(mapply(function(x, y) error_fun(x, y), c, e))
  }
  
  unlist(lapply(error_functions,
                function(error_fun) {
                  get_error_result(error_fun, computed_list, expected_list)
                }))
}

# funkcja wykonujaca 5-cio krotna kroswalidacje
# funckja zwraca wektor z bledami (ERR, MAD, MSE)
# X - macierz z cechami
# y - wektor z etykietami
# classification_func - funckja klasyfikacji, przyjmujaca nastepujace argumenty (X, y, Z)
#   gdzie X - macierz z cechami (zbior uczacy),
#         y - wektor z etykietami (do zbioru uczacego),
#         Z - macierz z cechami (do zbioru testowego)
crossvalidation5_test <- function(X, y, classification_func) {
  partition <- get_partition(length(y), 5)
  
  computed_list <- lapply(partition,
                          function(t) {
                            testing_indexes <- c(t)
                            c(classification_func(
                              X[-testing_indexes, ],
                              y[-testing_indexes],
                              X[testing_indexes, ]))
                          })
  
  expected_list <- lapply(partition,
                          function(testing_indexes) {
                            c(y[testing_indexes])
                          })
  
  get_error_vector_for_crossvalidation(computed_list, expected_list, list(ERR, MAD, MSE))
}


# uruchamia dany algorytm na zbiorze danych zapisanych w dataName.csv
# wyniki benchmarku i otrzymanych bledow w 5-cio krotnej kroswalidacji
# zapisuje w osobnych plikach
# dataName - nazwa pliku, gdzie jest zapisany zbior danych (bez rozszerzenia csv)
# functionName - nazwa funkcji klasyfikacji, ktora bedzie testowana
#   owa nazwa jest potrzebna do odpowiedniego nazwania plikow z odpowiedziami
# classificationFunc - testowana funkcja klasyfikacji
#   przyjmuje 3 argumenty (X cechy zbioru uczacego, y etykiety do zbioru uczacego, z cechy zbioru testowego)
launch_algorithm <- function(dataName, functionName, classification_func) {
  givenDataFrame <- read.csv(paste(c(dataName, ".csv"), sep="", collapse=""))
  
  X <- as.matrix(givenDataFrame[, 2:ncol(givenDataFrame)])
  y <- as.factor(givenDataFrame[, 1])
  
  # zapisanie csv dla benchmarka
  B <- microbenchmark::microbenchmark(
    alg=crossvalidation5_test(X, y, classification_func))
  write.csv(B, paste(c(dataName, "_", functionName, "_benchmark.csv"), sep="", collapse=""))
  
  # zapisanie csv dla bledow
  r <- crossvalidation5_test(X, y, classification_func)
  names(r) <- c("ERR", "MAD", "MSE")
  write.csv(r, paste(c(dataName, "_", functionName, "_errors.csv"), sep="", collapse=""))
}

# uruchomienie testow dla algorytmu knn z roznymi parametrami
# parametry to wszystkie pary nalezace do iloczynu kartezjanskiego
# zbiorow {1, 3, ..., 19} i {1, 2, Inf}
# dataName - zbiorDanych, dla ktorych bedziemy uruchamiac algorytm
launch_knn_for <- function(dataName) {
  m <- expand.grid(seq(1, 20, 2), c(1, 2, Inf)) # zapisane wszystkie mozliwe pary wierszami

  apply(m, 1, function(r) {
    launch_algorithm(dataName,
                     paste(c("knn", "k", r[1], "_p", r[2]), sep="", collapse=""),
                     function(x, y, z) {
                       k <- r[1]
                       p <- r[2]
                       knn(x, y, z, 2, 3)})
  })
}

# uruchomienie algorytmu lasow losowych
launch_random_forest_alg_for <- function(dataName) {
  launch_algorithm(dataName,
                   "randomForest",
                   function(x, y, z) {
                     m <- randomForest::randomForest(
                       data.matrix(x),
                       factor(unlist(y)))
                     
                     predict(m, z)
                   })
}

# uruchomienie testow
dataNames <- c("affairs", "auto_riskness",
               "bostonhousing", "bostonhousing_ord",
               "glass", "kinematics",
               "machine_ord", "skill",
               "stock_ord")

lapply(dataNames,
       function(x) launch_knn_for(x))

lapply(dataNames,
       function(x) launch_random_forest_alg_for(x))

