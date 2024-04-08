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
#' @param min Minimum value to constrain the generated values.
#' @param max Maximum value to constrain the generated values.
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
#' at random positions, and constrain the generated values within a specified
#' range using the \code{min} and \code{max} arguments.
#'
#' @seealso [sim_snorm()]
#'
#' @export
#'
#' @examples
#' # Generate random samples from a normal distribution with default parameters
#' sim_norm(n = 10)
#'
#' # Generate random samples with missing values
#' sim_norm(n = 10, miss = 2)
sim_norm <- function(n, mean = 0, sd = 1, digits = 2, seed = 123, miss = 0, min = NULL, max = NULL){
  set.seed(seed = seed)

  # Generate data from normal distribution
  x <- round(stats::rnorm(n = n, mean = mean, sd = sd), digits = digits)

  # Constrain values to specified minimum and maximum
  if(!is.null(min)){
    x[x < min] <- min
  }
  if(!is.null(max)){
    x[x > max] <- max
  }

  # Introduce missing values
  if(miss != 0 & miss < n){
    x[sample(1:n, size = miss)] <- NA
  }

  x
}


#' Simulate Data with Grouped Normal Distribution
#'
#' @param ... List of named arguments specifying the groups and their parameters
#' from [sim_norm()].
#' @param signif Logical indicating whether to simulate until the group difference
#' is significant (TRUE) or not (FALSE).
#' @param names Vector of names.
#'
#' @return A data frame containing simulated data with grouped normal distribution.
#'
#' @details This function generates simulated data with grouped normal distribution.
#' It takes multiple arguments where each argument represents a group with its
#' parameters (e.g., mean, standard deviation, sample size). The function simulates
#' data for each group until the group difference becomes statistically significant
#' or not based on the \code{signif} argument.
#'
#' @seealso [sim_norm()]
#'
#' @examples
#' # Simulate data with grouped normal distribution
#' sim_gourp_norm(A = list(n = 10, mean = 30, sd = 1, digits = 2),
#'                B = list(n = 8, mean = 35, sd = 2, digits = 2),
#'                C = list(n = 8, mean = 32, sd = 3, digits = 2))
#'
#' @export
sim_gourp_norm <- function(..., signif = TRUE, names = NULL) {
  # Store the arguments in a list
  d <- list(...)

  # Initialize output
  out <- NULL

  seed <- 0

  while (TRUE) {
    seed <- seed + 1

    # Simulate data for each group
    res <- lapply(d, \(x) {
      x$seed <- seed
      do.call(sim_norm, x)
    })

    # Combine results into a data frame
    group <- rep(names(res), sapply(res, length))
    value <- unlist(res)
    res <- data.frame(group, value)
    row.names(res) <- NULL

    # Check variance equality
    var.equal <- tryCatch(exec_levene(value, group)[[2]], error = function(e) 1.0)
    var.equal <- ifelse(var.equal>=0.05, TRUE, FALSE)

    # Perform group difference test
    if (length(d) == 2L) {
      fit <- stats::t.test(value ~ group, var.equal = var.equal)
    } else{
      fit <- stats::oneway.test(value ~ group, var.equal = var.equal)
    }

    if(seed == 5000){
      stop("Can not find.", call. = FALSE)
    }

    # Check significance and break loop if condition is met
    if (signif) {
      if (fit$p.value < 0.05) {
        out <- res
        break
      }
    } else{
      if (fit$p.value > 0.05) {
        out <- res
        break
      }
    }
  }

  if(!is.null(names)){
    names(out) <- names
  }
  out
}

exec_levene <- function(x, g){
  if (!is.factor(g)) {
    g <- as.factor(g)
  }
  valid <- stats::complete.cases(x, g)
  meds <- tapply(x[valid], g[valid], mean)
  resp <- abs(x - meds[g])
  res <-  stats::anova(stats::lm(resp ~ g))[, c(1, 4, 5)]
  rownames(res)[2] <- " "
  data.frame(statistic = res[1, 2], p.value = res[1, 3], stringsAsFactors = FALSE)
}


#' Generate Random Samples from a Skew-Normal Distribution
#'
#' @param n Number of observations to generate.
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#' @param xi Shape parameter controlling skewness.
#' @param digits Number of digits to round the generated data to.
#' @param seed Seed for random number generation.
#' @param miss Number of missing values to introduce.
#' @param min Minimum value to constrain the generated values.
#' @param max Maximum value to constrain the generated values.
#'
#' @return A numeric vector of simulated data.
#'
#' @details This function generates data from a skew-normal distribution.
#' The skew-normal distribution is a generalization of the normal distribution
#' that introduces skewness through a shape parameter \code{xi}. And constrain
#' the generated values within a specified range using the \code{min} and
#' \code{max} arguments.
#'
#' @examples
#' # Simulate 100 observations from a skew-normal distribution
#' sim_snorm(n = 100, mean = 0, sd = 1, xi = 1.5)
#' sim_snorm(n = 100, mean = 0, sd = 1, xi = -1.5)
#'
#' # Simulate 100 observations with 10% missing values
#' sim_snorm(n = 100, mean = 0, sd = 1, xi = 1.5, miss = 10)
#'
#' @seealso [sim_norm()]
#'
#' @export
sim_snorm <- function(n, mean = 0, sd = 1, xi = 1.5, digits = 2, seed = 123, miss = 0, min = NULL, max = NULL){

  set.seed(seed = seed)

  exec <- function (n, xi) {
    weight = xi / (xi + 1 / xi)
    z = stats::runif(n, -weight, 1 - weight)
    Xi = xi ^ sign(z)
    Random = -abs(stats::rnorm(n)) / Xi * sign(z)
    m1 = 2 / sqrt(2 * pi)
    mu = m1 * (xi - 1 / xi)
    sigma = sqrt((1 - m1 ^ 2) * (xi ^ 2 + 1 / xi ^ 2) + 2 * m1 ^ 2 - 1)
    Random = (Random - mu) / sigma
    Random
  }

  x <- round(exec(n = n, xi = xi) * sd + mean, digits = digits)

  # Constrain values to specified minimum and maximum
  if(!is.null(min)){
    x[x < min] <- min
  }
  if(!is.null(max)){
    x[x > max] <- max
  }

  # Introduce missing values
  if(miss != 0 & miss < n){
    x[sample(1:n, size = miss)] <- NA
  }

  x
}


#' Simulate Grouped Data from Skewed Normal Distribution
#'
#' @param ... List of arguments, each specifying parameters for a group from
#' [sim_snorm()].
#' @param signif Logical indicating whether to generate data until a significant
#' difference among groups is found.
#' @param names Optional names for the groups.
#'
#' @return A data frame containing simulated grouped data from a skewed normal
#' distribution.
#'
#' @details This function generates simulated grouped data from a skewed normal
#' distribution. You can specify parameters for each group using the ellipsis
#' (...) argument. If the \code{signif} parameter is set to \code{TRUE}, the
#' function continues generating data until a significant difference among groups
#' is found. The names of the groups can be provided using the \code{names} argument.
#'
#' @seealso [sim_snorm()]
#'
#' @examples
#' # Simulate grouped data with three groups
#' sim_gourp_snorm(A = list(n = 10, mean = 30, sd = 1, digits = 2),
#'                B = list(n = 8, mean = 35, sd = 2, digits = 2),
#'                C = list(n = 8, mean = 32, sd = 3, digits = 2))
#'
#' @export
sim_gourp_snorm <- function(..., signif = TRUE, names = NULL) {
  # Store the arguments in a list
  d <- list(...)

  # Initialize output
  out <- NULL

  seed <- 0

  while (TRUE) {
    seed <- seed + 1

    # Simulate data for each group
    res <- lapply(d, \(x) {
      x$seed <- seed
      do.call(sim_snorm, x)
    })

    # Combine results into a data frame
    group <- rep(names(res), sapply(res, length))
    value <- unlist(res)
    res <- data.frame(group, value)
    row.names(res) <- NULL

    # Perform group difference test
    if (length(d) == 2L) {
      fit <- stats::wilcox.test(value ~ group, exact = FALSE, correct = FALSE)
    } else{
      fit <- stats::kruskal.test(value ~ group)
    }

    if(seed == 5000){
      stop("Can not find.", call. = FALSE)
    }

    # Check significance and break loop if condition is met
    if (signif) {
      if (fit$p.value < 0.05) {
        out <- res
        break
      }
    } else{
      if (fit$p.value > 0.05) {
        out <- res
        break
      }
    }
  }

  if(!is.null(names)){
    names(out) <- names
  }
  out
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


#' Simulate Missing Values in a Vector
#'
#' @param x A vector containing the data.
#' @param n Number of missing values to introduce.
#' @param seed Seed for random number generation.
#'
#' @return A vector with missing values introduced.
#'
#' @details This function introduces missing values into a vector by randomly
#' selecting \code{n} indices and replacing the corresponding values with NA.
#'
#' @examples
#' # Simulate missing values in a numeric vector
#' sim_missing(x = c(1, 2, 3, 4, 5), n = 2)
#'
#' @export
sim_missing <- function(x, n = 0, seed = 123){
  set.seed(seed)
  x[sample(1:length(x), n)] <- NA
  x
}
