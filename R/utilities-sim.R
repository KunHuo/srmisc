#' Generate Random Samples from Normal Distribution
#'
#' Generate random samples from a normal distribution.
#'
#' @param n The number of samples to generate.
#' @param mean The mean of the normal distribution. Default is 0.
#' @param sd The standard deviation of the normal distribution. Default is 1.
#' @param digits The number of digits to round the generated samples to. Default is 2.
#' @param seed The seed for random number generation. Default is 123.
#' @param miss The number of missing values to introduce into the generated samples.
#' Default is 0.
#'
#' @return A vector of random samples from the normal distribution.
#'
#' @details This function generates random samples from a normal distribution
#' with the specified mean and standard deviation. The number of samples to
#' generate is given by the parameter \code{n}.
#'
#' The generated samples are rounded to the specified number of digits using the
#' \code{round} function. If the parameter \code{miss} is greater than 0 and
#' less than \code{n}, missing values are introduced into the generated samples
#' at random positions.
#'
#' @export
#'
#' @examples
#' # Generate random samples from a normal distribution with default parameters
#' sim_norm(n = 10)
#'
#' # Generate random samples with missing values
#' sim_norm(n = 10, miss = 2)
sim_norm <- function(n, mean = 0, sd = 1, digits = 2, seed = 123, miss = 0){
  set.seed(seed = seed)
  x <- round(stats::rnorm(n = n, mean = mean, sd = sd), digits = digits)

  if(miss != 0 & miss < n){
    x[sample(1:n, size = miss)] <- NA
  }

  x
}


#' Generate Random Samples from Categorical Distribution
#'
#' Generate random samples from a categorical distribution.
#'
#' @param ... An unnamed argument taking category frequencies as input.
#'            Each category frequency should be specified as a numeric value.
#'            For example, sim_category(10, 20, 30) generates a vector with 10
#'            occurrences of the first category, 20 occurrences of the second
#'            category, and 30 occurrences of the third category.
#' @param seed The seed for random number generation. Default is 123.
#' @param miss The number of missing values to introduce into the generated
#' samples. Default is 0.
#' @param factor Logical indicating whether to return the generated samples as
#' factors. Default is TRUE.
#'
#' @return A vector of random samples from the categorical distribution.
#'
#' @details This function generates random samples from a categorical distribution
#' with specified category frequencies. The category frequencies are specified as
#' unnamed arguments to the function, where each argument corresponds to the
#' frequency of a category.
#'
#' If the parameter \code{miss} is greater than 0 and less than the total number
#' of samples, missing values are introduced into the generated samples at random
#' positions.
#'
#' If \code{factor} is TRUE (default), the generated samples are returned as
#' factors.
#'
#' @export
#'
#' @examples
#' # Generate random samples with frequencies of categories A and B as 2 and 3 respectively
#' sim_category(A = 2, B = 3)
#'
#' # Generate random samples with frequencies of categories specified as numeric values
#' sim_category(3, 4)
#'
#' # Generate random samples with frequencies of categories A and B as 3 and 4 respectively,
#' # and introduce 2 missing values
#' sim_category(A = 3, B = 4, miss = 2)
sim_category <- function(..., seed = 123, miss = 0, factor = TRUE) {
  set.seed(seed = seed)

  # Create a list of category frequencies
  d <- list(...)

  # Generate samples based on category frequencies
  if (is.null(names(d))) {
    x <- rep(1:length(d), as.vector(d))
  } else {
    x <- rep(names(d), as.vector(d))
  }

  # Shuffle the samples
  x <- sample(x)

  # Introduce missing values if specified
  if (miss != 0 & miss < length(x)) {
    x[sample(1:length(x), size = miss)] <- NA
  }

  # Convert samples to factors if specified
  if (factor) {
    x <- factor(x)
  }

  x
}


#' Expand Categorical Data
#'
#' Expand categorical data based on category frequencies to generate original data.
#'
#' @param ... An unnamed argument taking category frequencies as input.
#'            Each category frequency should be specified as a numeric value.
#' @param names An optional argument to specify the names of the resulting variables.
#'If provided, it should be a character vector with two elements, where the first
#'element represents the name of the first variable and the second element
#'represents the name of the second variable.
#' @param factor Logical indicating whether to return the generated variables as
#' factors. Default is TRUE.
#'
#' @return A data frame representing the expanded categorical data.
#'
#' @details This function expands categorical data based on category frequencies
#' to generate original data. The category frequencies are specified as unnamed
#' arguments to the function, where each argument corresponds to the frequency
#' of a category.
#'
#' If \code{factor} is TRUE (default), the generated variables are returned as
#' factors; otherwise, they are returned as numeric values.
#'
#' @export
#'
#' @examples
#' expand_category(High = c(Male = 94,  Female = 13),
#'                 Low  = c(Male = 141, Female = 25),
#'                 names = c("HLA", "Sex"))
expand_category <- function(..., names = NULL, factor = TRUE) {

  # Create a list of category frequencies
  d <- list(...)

  # Extract category levels
  levels1 <- names(d)
  levels2 <- names(d[[1]])

  # Generate expanded variables
  d <- lapply(d, \(x) {
    rep(names(x), x)
  })

  # Create variables for data frame
  var1 <- rep(names(d), sapply(d, length))
  var2 <- unlist(d)
  names(var2) <- NULL

  # Convert variables to factors if specified
  if (factor) {
    var1 <- factor(var1, levels = levels1)
    var2 <- factor(var2, levels = levels2)
  }

  # Create data frame
  out <- data.frame(var1, var2)

  # Set variable names if specified
  if (!is.null(names)) {
    names(out) <- names
  }

  out
}