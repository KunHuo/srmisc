#' Simple impute missing values
#'
#' @param data a data frame.
#' @param method.numeric Impute method for continuous variables, default "median".
#' @param method.category Impute method for categorical variables, default "mode".
#' @param digits digits for continuous impute method,If it is NULL (default),
#' it will be automatically obtained from the original data.
#'
#' @return a data frame after imputation.
#' @export
#'
#' @examples
#' md <- data.frame(
#'   x = c(3, 4, 5, 3, NA, 5, NA, 4, 5, 3, 5),
#'   y = c("a", "b", "c", "a", "b", "c", "c", NA, "c", "a", NA),
#'   z = factor(c("a", "b", "c", NA, "b", "c", "c", "a", "c", NA, "a")))
#' md
#' impute(md)
impute <- function(data, method.numeric = c("median", "mean"), method.category = "mode", digits = NULL){
  method.numeric <- match.arg(method.numeric)
  method.category <- match.arg(method.category)
  data[, ] <- lapply(data[, ], function(x){
    if(any(is.na(x))){
      if(is.numeric(x)){
        if(method.numeric == "median"){
          x <- impute_median(x, digits = digits)
        }else if(method.numeric == "mean"){
          x <- impute_mean(x, digits = digits)
        }
      }else if(is.factor(x) | is.character(x)){
        if(method.category == "mode"){
          x <- impute_mode(x)
        }
      }
    }
    x
  })
  data
}


#' Impute with median value
#'
#' @param x a numeric vector.
#' @param digits digits for median,If it is NULL (default), it will be
#' automatically obtained from the original data.
#'
#' @return a numeric vector after impute with median value.
#' @export
#'
#' @seealso [impute_mean()], [impute_mode()], [impute()]
#'
#' @examples
#' md <- c(4.0, 5.9, 3.2, 6.1, 7.6, NA, 9.1, 7.2, NA, 8.3)
#' md
#'
#' impute_median(md)
impute_median <- function(x, digits = NULL){
  m <- stats::median(x, na.rm = TRUE)
  if(!is.null(digits)){
    m <- round(m, digits = digits)
  }else{
    digits <- max(n_digits(x))
    m <- round(m, digits = digits)
  }
  x[is.na(x)] <- m
  x
}


#' Impute with mean value
#'
#' @param x a numeric vector.
#' @param digits digits for mean,If it is NULL (default), it will be
#' automatically obtained from the original data.
#'
#' @return a numeric vector after impute with mean value.
#' @export
#'
#' @seealso [impute_median()], [impute_mode()], [impute()]
#'
#' @examples
#' md <- c(4.0, 5.9, 3.2, 6.1, 7.6, NA, 9.1, 7.2, NA, 8.3)
#' md
#'
#' impute_mean(md)
impute_mean <- function(x, digits = NULL){
  m <- mean(x, na.rm = TRUE)
  if(!is.null(digits)){
    m <- round(m, digits = digits)
  }else{
    digits <- max(n_digits(x))
    m <- round(m, digits = digits)
  }
  x[is.na(x)] <- m
  x
}


#' Impute with mode value
#'
#' @param x a vector.
#'
#' @return a numeric vector after impute.
#' @export
#'
#' @seealso [impute_median()], [impute_mean()], [impute()]
#'
#' @examples
#' # for numeric
#' md1 <- c(3, 4, 5, 3, 4, 5, 5, NA, 5, 3, NA)
#' md1
#' impute_mode(md1)
#'
#' # for character
#' md2 <- c("a", "b", "c", "a", "b", "c", "c", NA, "c", "a", NA)
#' md2
#' impute_mode(md2)
#'
#' # for factor
#' md3 <- as.factor(md2)
#' md3
#' impute_mode(md3)
impute_mode <- function(x){
  u <- unique(x)
  u <- u[which.max(tabulate(match(x, u)))]
  x[is.na(x)] <- u
  x
}


#' Multivariate Imputation by Chained Equations
#'
#' @param data A data frame or a matrix containing the incomplete data. Missing
#' values are coded as NA.
#' @param varnames Variable names
#' @param m Number of multiple imputations. The default is m=1.
#' @param method Can be either a single string, or a vector of strings with
#' length length(blocks), specifying the imputation method to be used for each
#' column in data. If specified as a single string, the same method will be used
#' for all blocks. The default imputation method (when no argument is specified)
#' depends on the measurement level of the target column, as regulated by the
#' defaultMethod argument. Columns that need not be imputed have the empty
#' method "". See details.
#' @param seed An integer that is used as argument by the set.seed() for
#' offsetting the random number generator. Default is to leave the random number
#' generator alone.
#' @param printFlag If TRUE, mice will print history on console.
#' @param ... Named arguments that are passed down to the univariate imputation functions.
#'
#' @seealso [mice::mice()]
#'
#' @return a data frame.
#' @export
impute_mice <- function(data, varnames = NULL, m = 1, method = NULL, seed = 123, printFlag = FALSE, ...){
  varnames <- srmisc::select_variable(data, varnames)

  if(srmisc::is_empty(varnames)){
    varnames <- names(data)
  }
  mdata <- data[varnames]
  mdata <- mice::mice(data = mdata, m = 5, method = method, seed = seed, printFlag = printFlag, ...)
  mdata <- mice::complete(mdata, 1)

  for(i in 1:length(varnames)){
    data[varnames[i]] <- mdata[varnames[i]]
  }
  data
}
