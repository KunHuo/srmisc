check_index <- function(data, index){
  tmp <- index >=1 & index <= ncol(data)
  if(!all(tmp)){
    meaasge <- sprintf("Index must be between 1 and %d.", ncol(data))
    stop(meaasge, call. = FALSE)
  }
}

#' Check if variable is contained in data frame
#'
#' Check whether the variable is included in the data frame.
#'
#' @param data a data frame.
#' @param varnames variable names.
#' @param stop a logical indicating whether or not to stop if the variable is
#' not included in the data frame.
#'
#' @return Logical vectors if stop == FALSE. Otherwise, the program execution
#' terminates without return value.
#' @export
#'
#' @examples
#' check_name(mtcars, varnames = c("am", "vs"))
check_name <- function(data, varnames, stop = TRUE){
  tmp <- varnames %in% names(data)

  if(!all(tmp)){
    tmpname <- varnames[!tmp]
    tmpname <- paste(tmpname, collapse = ", ")
    message <- sprintf("%s are (is) not included in the data frame.", tmpname)
    if(stop){
      stop(message, call. = FALSE)
    }else{
      warning(message, call. = FALSE)
      tmp
    }
  }else{
    tmp
  }
}


#' Check whether the package is installed
#'
#' @param pkg the names of a package.
#' @param message a logical indicating whether or not show message when the
#' package is not installed.
#'
#' @return Logical vectors indicating whether the package is installed.
#' @export
#'
#' @examples
#' check_installed("ggplot2")
#' check_installed("dplyr", "tidyr")
check_installed <- function (pkg, message = FALSE) {
  res <- all(sapply(pkg, function(x) isTRUE(requireNamespace(x, quietly = TRUE))))
  if(res){
    return(res)
  }else{
    if(message){
      stop(sprintf("The '%s' package needs to be installed. Run the following
                   code to install: install.packages('%s').", pkg, pkg), call. = FALSE)
    }else{
      return(res)
    }
  }
}


#' Check Missing Values
#'
#' Check missing values in each variable of a data frame.
#'
#' @param data The input data frame.
#' @param digits The number of digits to display for percentages. Default is 1.
#' @param label Logical indicating whether to include variable labels. Default
#' is FALSE.
#' @param language Specify language, 'en' is English, 'cn' or 'zh' is Chinese.
#' Default is 'en'.
#'
#' @return A data frame summarizing missing values in each variable.
#'
#' @details
#' This function checks missing values in each variable of a data frame. It
#' calculates the number and percentage of missing values, as well as the number
#' of valid (non-missing) values for each variable. The
#' function returns a data frame with columns for variable name, number of
#' missing values, percentage of missing values, and number of valid values.
#'
#' @export
check_missing <- function(data, digits = 1, label = FALSE, language = "en"){

  out <- lapply(data, \(x){
    data.frame(Missing = sum(is.na(x)),
               Missing.p = sprintf("%s%%", fmt_digits(sum(is.na(x)) / length(x) * 100, digits = digits)),
               valid = length(x) - sum(is.na(x)))
  })

  if(label){
    names(out) <- get_var_label(data, names(data), default = ".name")
  }else{
    names(out) <- names(data)
  }

  out <- list_rbind(out)
  out$NO <- 1:nrow(out)
  out <- relocate(out, "NO")

  if(language == "en"){
    names(out) <- c("No.", "Variable", "Missing (n)", "Missing (%)", "Valid (n)")
    attr(out, "title") <- "Data missingness"
    attr(out, "note") <- sprintf("Note: %d variables and %d cases have missing values.",
                                 sum(out[[3]] != 0),
                                 nrow(data) - nrow(stats::na.omit(data)))
  }else{
    names(out) <- c("\u5e8f\u53f7", "\u53d8\u91cf", "\u7f3a\u5931\u4f8b\u6570", "\u7f3a\u5931\u6bd4\u4f8b", "\u6709\u6548\u4f8b\u6570")
    attr(out, "title") <- "\u6570\u636e\u7f3a\u5931\u60c5\u51b5"
    attr(out, "note") <- sprintf("\u6ce8\uff1a%d\u4e2a\u53d8\u91cf\u4e2d\u6709%d\u4e2a\u5b58\u5728\u7f3a\u5931\u503c\u3002", ncol(data), sum(out[[3]] != 0))
  }

  class(out) <- c("check", "data.frame")
  out
}


#' Check Variable Types and Values
#'
#' Check variable types and unique values in a data frame.
#'
#' @param data The input data frame.
#' @param label Logical indicating whether to include variable labels. Default is FALSE.
#'
#' @return A data frame summarizing variable types and unique values.
#'
#' @details This function checks the variable types and unique values in each
#' variable of a data frame. The function returns a data frame with columns for
#' variable name, class, number of missing values, number of unique values,
#' and sample values. If the number of unique values for a variable exceeds 5,
#' only the first 5 values are displayed followed by "...".
#'
#' @export
check_type <- function(data, label = FALSE){
  out <- lapply(names(data), \(x){
    if(label){
      Variable <- get_var_label(data, x, default = ".name")
    }else{
      Variable <- x
    }

    if(unique_length(data[[x]]) <= 5L){
      Value <- paste(unique(data[[x]]), collapse = ", ")
    }else{
      Value <- paste0(paste(utils::head(data[[x]]), collapse = ", "), ", ...")
    }

    data.frame(Variable = Variable,
               Type = class(data[[x]]),
               Missing = sum(is.na(data[[x]])),
               Unique = unique_length(data[[x]]),
               Value = Value)
  })

  out <- list_rbind(out, varname = "No.")

  attr(out, "title") <- "Data structure"
  class(out) <- c("check", "data.frame")
  out
}


#' Check for Outliers
#'
#' Identify univariate outliers in numeric variables using boxplot methods.
#'
#' @param data The input data frame.
#' @param group The grouping variable(s). Default is NULL.
#' @param varnames The numeric variable(s) to check for outliers. Default is NULL
#' (checks all numeric variables).
#' @param ... unused.
#'
#' @return A data frame summarizing the identified outliers.
#'
#' @details This function identifies univariate outliers in numeric variables
#' using boxplot methods. It calculates the outliers based on the interquartile
#' range (IQR) rule, where values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are
#' considered outliers, and extreme points based on the same rule but with a
#' multiplier of 3.
#'
#' If no numeric variables are provided or identified in the data, the function
#' prints a message and returns NULL. The function returns a data frame with
#' columns for variable name, grouping variable (if provided), row number,
#' variable value, outlier status, and extreme status.
#'
#' @examples
#' check_outlier(iris)
#' check_outlier(iris, group = "Species")
#'
#' @export
check_outlier <-  function(data, group = NULL, varnames = NULL, ...){

  group <- select_variable(data, group, type = "name")
  varnames <- select_numeric(data, varnames, type = "name")
  varnames <- setdiff(varnames, group)

  if(length(varnames) == 0L){
    cat("\n No numeric variables to identify outliers. \n\n")
    return(invisible(NULL))
  }

  names(varnames) <- varnames

  data$.row.number <- 1:nrow(data)

  exec <- function(data, varname){
    data$outlier <- is_outlier(data[[varname]])
    data$extreme <- is_extreme(data[[varname]])
    data <- data[, c(".row.number", varname, "outlier", "extreme")]
    names(data)[2] <- "value"
    data
  }

  out <- lapply(varnames, function(varname){
    res <- group_exec(data, group = group, \(d){
      exec(data, varname)
    })
  })

  out <- list_rbind(out, collapse.names = FALSE)
  out <- out[!is.na(out$outlier) & out$outlier, ]

  if(nrow(out) < 1L){
    cat("\nNo outliers.\n\n")
    return(invisible(NULL))
  }

  out[[1]] <- sapply(out[[1]], \(x) get_var_label(data, x, default = ".name"))
  out[[1]] <- delete_duplicate_values(out[[1]])
  out <- as.data.frame(out)

  if(is_empty(group)){
    names(out) <- c("Variable", "Row", "Value", "Outlier", "Extreme")
  }else{
    names(out) <- c("Variable", get_var_label(data, group, default = ".name"), "Row", "Value", "Outlier", "Extreme")
  }

  out <- add_title(out, "Identify univariate outliers using boxplot methods")
  out <- add_note(out, "Note: Values above Q3 + 1.5\u00d7IQR or below Q1 - 1.5\u00d7IQR are considered as outliers.")
  out <- add_note(out, "Values above Q3 + 3\u00d7IQR or below Q1 - 3\u00d7IQR are considered as extreme points (or extreme outliers).")
  out <- add_note(out, "Q1 and Q3 are the first and third quartile, respectively. IQR is the interquartile range (IQR = Q3 - Q1).")

  class(out) <- c("check", "data.frame")

  out
}

#' Determine outliers
#'
#' This function calculates outliers in a numeric vector using the interquartile
#' range (IQR) method.
#'
#' @param x Numeric vector containing the values to be checked for outliers.
#' @param coef Coefficient to determine the threshold for defining outliers.
#' Default is 1.5.
#'
#' @return Logical vector indicating outliers (TRUE) and non-outliers (FALSE).
#'
#' @details
#' The interquartile range (IQR) method identifies outliers based on the difference
#' between the first quartile (Q1) and third quartile (Q3). Outliers are defined
#' as values outside the range (Q1 - coef*IQR, Q3 + coef*IQR),
#' where \code{coef} is the coefficient.
#'
#' @seealso \code{\link{is_extreme}}
#'
#' @examples
#' # Define a numeric vector
#' x <- c(1, 2, 3, 4, 5, 20)
#'
#' # Determine outliers
#' is_outlier(x)
#'
#' @export
is_outlier <- function (x, coef = 1.5) {
  res  <- x
  Q1   <- stats::quantile(x, 0.25, na.rm = TRUE)
  Q3   <- stats::quantile(x, 0.75, na.rm = TRUE)
  .IQR <- stats::IQR(x, na.rm = TRUE)

  upper.limit <- Q3 + (coef * .IQR)
  lower.limit <- Q1 - (coef * .IQR)

  outlier <- ifelse(x < lower.limit | x > upper.limit, TRUE, FALSE)
  outlier
}


#' Determine extreme values
#'
#' This function calculates extreme values in a numeric vector using the
#' interquartile range (IQR) method.
#'
#' @param x Numeric vector containing the values to be checked for extreme values.
#'
#' @return Logical vector indicating extreme values (TRUE) and non-extreme values (FALSE).
#'
#' @details
#' This function internally calls \code{\link{is_outlier}} with a coefficient of
#' 3 to determine extreme values.
#'
#' @seealso \code{\link{is_outlier}}
#'
#' @examples
#' # Define a numeric vector
#' x <- c(1, 2, 3, 4, 5, 100)
#'
#' # Determine outliers
#' is_extreme(x)
#'
#' @export
is_extreme <- function (x) {
  is_outlier(x, coef = 3)
}


#' Delete Extreme Values from Data
#'
#' This function removes extreme values from a specified variable in a dataset.
#'
#' @param data A data frame containing the dataset.
#' @param varname The name of the variable from which extreme values should be
#' removed.
#'
#' @return A data frame with extreme values removed from the specified variable.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3, 100, 5))
#' clean_data <- delete_extreme(data, "x")
#'
#' @export
delete_extreme <- function(data, varname = NULL){

  varname <- select_variable(data, varname)

  index <- is_extreme(data[[varname]])

  if(!any(index, na.rm = TRUE)){
    data
  }else{
    index[is.na(index)] <- FALSE
    data[!index, , drop = FALSE]
  }
}


#' Print check object
#'
#' @param x a object of check.
#' @param ... more.
#'
#' @keywords internal
#' @export
print.check <- function(x, ...){
  print_booktabs(x, ...)
}

