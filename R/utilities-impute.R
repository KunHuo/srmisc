#' Impute with median value
#'
#' @param x a numeric vector.
#' @param digits digits for median,If it is NULL (default), it will be
#' automatically obtained from the original data.
#'
#' @return a numeric vector after impute with median value.
#' @export
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
impute_mode <- function(x){
  u <- unique(x)
  u <- u[which.max(tabulate(match(x, u)))]
  x[is.na(x)] <- u
  x
}


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
  attr(data, "impute.method") <- "Because missing data were less than 5%, simple imputation procedures were used to impute all missing values. Continuous variables were imputed to their median values and categorical variables were imputed to the highest frequency category."
  data
}
