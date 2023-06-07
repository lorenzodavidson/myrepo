# Functions ---------------------------------------------------------------

# Creating rescaling function (with for loop)
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale <- function(x) {
  rng <- range(x, na.rm = TRUE)
  x <- (x - rng[1])/(rng[2]-rng[1])
}

df_rescaled <- tibble::tibble(
  a = 1:10,
  b = 1:10,
  c = 1:10,
  d = 1:10
)

for (i in seq_along(df)) {
  df_rescaled[[i]] <- rescale(df[[i]])
}

# Writing function to find variance of a vector
v <- sample(-10:10, 20, replace=TRUE)

variance_function <- function(x) {
  n <- length(x)
  mean <- mean(x, na.rm = TRUE)
  diff <- rep(0, n)
  for (i in seq_along(x)) {
    diff[[i]] <- (x[[i]]-mean)^2
  }
  1/(n-1)*sum(diff)
} # Correct!