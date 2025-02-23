#' Impute missing values
#'
#' This function allows you to impute missing values in a data frame using simple
#' imputation methods. It provides flexibility to specify different imputation
#' methods for numeric and categorical variables.
#'
#' @param data A data frame containing the dataset with missing values.
#' @param method.numeric The imputation method for numeric variables. Options
#' include "median" or "mean" (default: "median").
#' @param method.category The imputation method for categorical variables. Options
#' include "mode" (default: "mode").
#' @param digits The number of digits to use for imputing numeric variables.
#'
#' @return A data frame with missing values imputed based on the specified methods.
#'
#' @details
#' The `impute_simple` function provides simple imputation methods for missing values
#' in both numeric and categorical variables. For numeric variables, it allows you to
#' choose between imputing the missing values with the median or mean of the non-missing
#' values. For categorical variables, the function imputes missing values with the mode
#' (most frequent value) of the variable.
#'
#' @examples
#' # Create a dataset with missing values
#' data <- data.frame(
#'   numeric_var = c(1, 2, NA, 4, 5),
#'   categorical_var = c("A", "B", NA, "A", "C")
#' )
#'
#' # Impute missing values using median for numeric and mode for categorical variables
#' impute_simple(data,
#'               method.numeric = "median",
#'               method.category = "mode",
#'               digits = NULL)
#'
#' @export
impute_simple <- function(data, method.numeric = c("median", "mean"), method.category = "mode", digits = NULL){
  method.numeric <- match.arg(method.numeric)
  method.category <- match.arg(method.category)

  # Apply imputation method to each column of the data frame
  data[, ] <- lapply(data[, ], function(x){
    if(any(is.na(x))){
      if(is.numeric(x)){
        # Impute missing values for numeric variables
        if(method.numeric == "median"){
          x <- impute_median(x, digits = digits)
        }else if(method.numeric == "mean"){
          x <- impute_mean(x, digits = digits)
        }
      }else if(is.factor(x) | is.character(x)){
        # Impute missing values for categorical variables
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
#' @seealso [impute_mean()], [impute_mode()], [impute_simple()]
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
#'
#'
#' @seealso [impute_median()], [impute_mode()], [impute_simple()]
#'
#' @examples
#' md <- c(4.0, 5.9, 3.2, 6.1, 7.6, NA, 9.1, 7.2, NA, 8.3)
#' md
#'
#' impute_mean(md)
#'
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
#' This function replaces missing values in a vector with the mode (most frequent
#'  value) of that vector.
#'
#' @param x A vector containing numeric or categorical data with missing values.
#'
#' @return A vector with missing values replaced by the mode of the variable.
#'
#' @seealso [impute_median()], [impute_mean()], [impute_simple()]
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
#'
#' @export
impute_mode <- function(x){
  u <- unique(x)
  u <- u[which.max(tabulate(match(x, u)))]
  x[is.na(x)] <- u
  x
}


#' Multivariate Imputation by Chained Equations
#'
#'
#' This function allows you to impute missing values in a dataframe using the
#' multiple imputation by chained equations (MICE) method. It provides
#' flexibility to specify the variables to be imputed, the number of imputations,
#' and the imputation method for each variable.
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
#' @return a data frame.
#'
#' @details
#' The `impute_mice` function uses the multiple imputation by chained equations
#' (MICE) method to impute missing values in a dataset. It allows you to specify
#' the variables to be imputed (`varnames`), the number of imputations (`m`),
#' and the imputation method for each variable (`method`). If `method` is NULL,
#' the [mice::mice()] function will choose appropriate imputation methods automatically.
#'
#' @seealso [mice::mice()]
#'
#' @export
impute_mice <- function(data, m = 1, varnames = NULL, method = NULL, seed = 123, printFlag = FALSE, ...){

  varnames <- srmisc::select_variable(data, varnames)

  if(srmisc::is_empty(varnames)){
    varnames <- names(data)
  }
  mdata <- data[varnames]
  mdata <- mice::mice(data = mdata, m = m, method = method, seed = seed, printFlag = printFlag, ...)

  out <- lapply(1:m, \(j){
    res <- data
    for(i in 1:length(varnames)){
      res[varnames[i]] <- mice::complete(mdata, j)[varnames[i]]
    }
    res
  })

  if(m == 1){
    out <- out[[1]]
  }else{
    class(out) <- c("mlist", "list")
  }

  out
}
