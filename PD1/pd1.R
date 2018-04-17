# zad1

numberOfConcordantPairs <- function(x,y)
{
  a <- lapply(x, function(k) which(x > k))
  b <- lapply(y, function(k) which(y > k))
  
  sum(unlist(lapply(mapply(intersect, a, b), length)))
}

numberOfConcordantPairs_ver2 <- function(x,y)
{
  sum(outer(x, x, "<") & outer(y, y, "<"))
}

kendall <- function(x, y)
{
  stopifnot(length(x) == length(y))
  stopifnot(anyDuplicated(x) == 0)
  stopifnot(anyDuplicated(y) == 0)
  
  n <- length(x)
  c <- numberOfConcordantPairs(x, y)
  
  ((2 * c) / (n * (n - 1) / 2)) - 1
}

# zad2
moda <- function(x)
{
  counters <- rle(sort(x))
  most_freq <- counters$values[which(counters$lengths == max(counters$lengths))]
  
  ifelse(length(most_freq) == 1, most_freq,
        sample(most_freq, 1))
}

# zad3

gendyskr <- function(n, x, p)
{
  stopifnot(length(n) == 1 & n == as.integer(n) & n > 0)
  stopifnot(length(x) == length(p))
  stopifnot(anyDuplicated(x) == 0)
  
  x[1 + findInterval(runif(n, 0, 1),
                     cumsum(p / sum(p)),
                     left.open = TRUE,
                     rightmost.closed = TRUE
                     )]
}

# zad4
lead <- function(x, n)
{
    res <- rep(c(NA), length(x))
    
    if(length(x) > n)
    {
      res[1:(length(x)-n)] <- x[(n + 1):length(x)]
    }
    
    res
}

lag <- function(x, n)
{
  res <- rep(c(NA), length(x))
  
  if(length(x) > n)
  {
    res[(n + 1):length(x)] <- x[1:(length(x)-n)]
  }
  
  res
}

# zad5
Fn_roz1 <- function(x, y)
{
  unlist(lapply(y, function(a) {
    length(x[x <= a])
  })) / length(x)
}

Fn <- function(x, y)
{
  x <- sort(x)
  x_unique <- unique(x)
  counters <- rle(x)
  
  sum_pref <- c(0, cumsum(counters$lengths))
  
  indexes <- findInterval(y, c(-Inf, x_unique), rightmost.closed = FALSE, left.open = FALSE)
  sum_pref[indexes] / length(x)
}

