// [[Rcpp::plugins("cpp11")]]

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <utility>
#include <bits/stdc++.h>
#include <cstdio>
using namespace Rcpp;
using namespace std;


// Funkcja zwracajaca obiekt typu factor
// length - dlugosc zwracanego wektora
// levels - mozliwe wartosci, jakie moze przyjmowac zwracany obiekt factor
IntegerVector get_factor_object(int length,
                                const CharacterVector& levels)
{
  IntegerVector res(length);
  res.attr("levels") = levels;
  res.attr("class") = "factor";
  
  return res;
}

// Funkcja zwracajaca wartosc metryki Minkowskiego dla skonczonych p
// w celu optymalizacji obliczen, nie liczymy pierwiastka z wyniku
// x_ind - numer wiersza z macierzy X, czyli "wskaznik na pierwszy z wektorow", dla ktorego liczymy metryke
// z_ind - numer wiersza z macierzy Z, czyli "wskaznik na drugi z wektorow", dla ktorego liczymy metryke
double get_minkowsky_distance_finite(double p,
                                     const NumericMatrix& X, int x_ind,
                                     const NumericMatrix& Z, int z_ind)
{
  double dist = 0.0;
  int m = X.ncol();
  
  for(int i = 0; i < m; ++i)
  {
    dist += pow(abs(X(x_ind, i) - Z(z_ind, i)), p);
  }
  
  return dist;
}

// Funkcja oblicza metryke dla p=Inf
// x_ind - numer wiersza z macierzy X, czyli "wskaznik na pierwszy z wektorow", dla ktorego liczymy metryke
// z_ind - numer wiersza z macierzy Z, czyli "wskaznik na drugi z wektorow", dla ktorego liczymy metryke
double get_chessboard_distance(const NumericMatrix& X, int x_ind,
                               const NumericMatrix& Z, int z_ind)
{
  double dist = 0.0;
  int m = X.ncol();
  
  for(int i = 0; i < m; ++i)
  {
    dist = std::max(dist, abs(X(x_ind, i) - Z(z_ind, i)));
  }
  
  return dist;
}

// Funkcja oblicza metryke dla danego p pomiedzy
// dwoma wektorami (pierwszy z macierzy X (o indexie x_ind)
//  drugi z macierzy Z o indexie z_ind)
double get_distance(double p,
                    const NumericMatrix& X, int x_ind,
                    const NumericMatrix& Z, int z_ind)
{
  return (p == INFINITY) ?
    get_chessboard_distance(X, x_ind, Z, z_ind) :
    get_minkowsky_distance_finite(p, X, x_ind, Z, z_ind);
}

// funkcja zwraca wektor par (odleglosc, etykieta)
// sa to odleglosci od wszystkich punktow ze zbioru uczacego
// wraz z przypisanymi im etykietami
// X - macierz cech ze zbioru uczacego
// Z - macierz cech ze zbioru testowego
// z_ind - index wiersza z macierzy Z, dla ktorego liczymy odleglosci do wszystkich punktow ze zbioru uczacego
// labels - etykiety przypisane kolejnym punktom ze zbioru uczacego
std::vector<std::pair<double, int> > get_distances(const NumericMatrix& X,
                                                   const NumericMatrix& Z,
                                                   int z_ind,
                                                   double p,
                                                   const IntegerVector& labels)
{
  int teaching_vectors_num = X.nrow();
  std::vector<std::pair<double, int> > dist(teaching_vectors_num);
  
  for(int i = 0; i < teaching_vectors_num; ++i)
  {
    dist[i].first = get_distance(p, X, i, Z, z_ind);
    dist[i].second = labels[i];
  }
  
  return dist;
}

// funkcja zwraca slownik haszowalny, gdzie klucz jest etykieta, a wartosc: ile wystepuje etykiet o tej wartosci
// funkcja przyjmuje dwa argumenty:
// odpowiednio iteratory na poczatek i koniec wektora par (odleglosc, etykieta)
// z wyznaczonej tak czesci wektora zliczamy licznosci wystapien etykiet
unordered_map<int,int> get_frequencies(const std::vector<std::pair<double, int> >::iterator& beg_it,
                                       const std::vector<std::pair<double, int> >::iterator& end_it)
{
  unordered_map<int, int> m;
  for(std::vector<std::pair<double, int> >::iterator it = beg_it; it != end_it; ++it)
  {
    if(m.find(it -> second) == m.end())
    {
      m.insert(std::make_pair(it -> second, 1));
    }
    else
    {
      ++m[it -> second];
    }
  }
  
  return m;
}

// funkcja uaktualnia wektor obecnych wartosci maksymalnych
// new_pair - nowa wartosc do rozwazenia
// current_max_cnt - aktualna wartosc maksymalna
// current_maximums - wektor etykiet, ktore wystepuja najczesciej
void update_current_maximums(const pair<int,int>& new_pair,
                             int& current_max_cnt,
                             vector<int>& current_maximums)
{
  if(new_pair.second > current_max_cnt)
  {
    current_maximums.clear();
    current_maximums.push_back(new_pair.first);
    
    current_max_cnt = new_pair.second;
  }
  else if(new_pair.second == current_max_cnt)
  {
    current_maximums.push_back(new_pair.first);
  }
}

// funkcja zwraca najczesciej wystapujaca etykiete z czesci wektora
// beg_it - iterator na poczatek rozwazanej czesci wektora
// end_it - iterator na koniec rozwazanej czesci wektora
int get_the_most_frequent(const std::vector<std::pair<double, int> >::iterator& beg_it,
                          const std::vector<std::pair<double, int> >::iterator& end_it)
{
  unordered_map<int, int> frequencies = get_frequencies(beg_it, end_it);
  
  int current_max_cnt = -1;
  vector<int> current_maximums;
  
  for(const std::pair<int, int>& item: frequencies)
  {
    update_current_maximums(item, current_max_cnt, current_maximums);
  }
  
  return current_maximums[rand() % current_maximums.size()];
}

// funkcja zwraca etykiete dla (z_ind)-tego wektora (wiersza) z macierzy Z (testowej)
// labels - wektor etykiet dla punktow z macierzy uczacej
// X - macierz cech dla zbioru uczacego
// p - wartosc charakteryzujaca metryke
// k - ile najblizszych sasiadow bierzemy pod uwage
int get_label(const IntegerVector& labels,
              const NumericMatrix& X,
              const NumericMatrix& Z, int z_ind,
              double p,
              int k)
{
  std::vector<std::pair<double, int> > distances = get_distances(X, Z, z_ind, p, labels);
  std::sort(distances.begin(), distances.end());
  int considered_elements_num = std::min(k, (int)distances.size());
  
  return get_the_most_frequent(distances.begin(),
                               distances.begin() + considered_elements_num);
}

// [[Rcpp::export]]
IntegerVector knn(NumericMatrix X,
                  IntegerVector y,
                  NumericMatrix Z,
                  int k,
                  double p)
{
  int m = Z.nrow();
  IntegerVector res = get_factor_object(m, y.attr("levels"));
  
  for(int i = 0; i < m; ++i)
  {
    res[i] = get_label(y, X, Z, i, p, k);
  }
  
  return res;
}

