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



#' Check missing
#'
#' @param data a data frame.
#' @param digits digits for percent. Default 1.
#' @param label Whether to replace variable names with variable labels. Default is FALSE.
#'
#' @return a data frame.
#' @export
check_missing <- function(data, digits = 1, label = FALSE){

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

  names(out) <- c("No.", "Variable", "Missing (n)", "Missing (%)", "Valid (n)")
  attr(out, "title") <- "Data missingness"
  attr(out, "note") <- sprintf("Note: %d of the %d variables had missing values.", sum(out[[3]] != 0), ncol(data))
  class(out) <- c("check", "data.frame")
  out
}


#' Check data structure
#'
#' @param data a data frame.
#' @param label Whether to replace variable names with variable labels. Default is FALSE.
#'
#' @return a data frame.
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
      Value <- paste0(paste(head(data[[x]]), collapse = ", "), ", ...")
    }

    data.frame(Variable = Variable,
               class = class(data[[x]]),
               Missing = sum(is.na(data[[x]])),
               Unique = unique_length(data[[x]]),
               Value = Value)
  })

  out <- list_rbind(out, varname = "No.")

  attr(out, "title") <- "Data structure"
  class(out) <- c("check", "data.frame")
  out
}


#' Identify univariate outliers
#'
#' Detect outliers using boxplot methods. Boxplots are a popular and an easy
#' method for identifying outliers. There are two categories of outlier: (1)
#' outliers and (2) extreme points.Values above Q3 + 1.5xIQR or below Q1 -
#' 1.5xIQR are considered as outliers. Values above Q3 + 3xIQR or below Q1 -
#' 3xIQR are considered as extreme points (or extreme outliers). Q1 and Q3 are
#' the first and third quartile, respectively. IQR is the interquartile range
#' (IQR = Q3 - Q1).Generally speaking, data points that are labelled outliers in
#' boxplots are not considered as troublesome as those considered extreme points
#' and might even be ignored. Note that, any NA and NaN are automatically
#' removed before the quantiles are computed.
#'
#' @param data a data frame.
#' @param group group variable name.
#' @param varnames numeric variable names.
#' @param ... unused arguments.
#'
#' @return a data frame.
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
  out <- tibble::as_tibble(out)

  if(is_empty(group)){
    names(out) <- c("Variable", "Row", "Value", "Outlier", "Extreme")
  }else{
    names(out) <- c("Variable", get_var_label(data, group, default = ".name"), "Row", "Value", "Outlier", "Extreme")
  }

  out <- add_title(out, "Identify univariate outliers using boxplot methods")
  out <- add_note(out, "Note: Values above Q3 + 1.5×IQR or below Q1 - 1.5×IQR are considered as outliers.")
  out <- add_note(out, "Values above Q3 + 3×IQR or below Q1 - 3×IQR are considered as extreme points (or extreme outliers).")
  out <- add_note(out, "Q1 and Q3 are the first and third quartile, respectively. IQR is the interquartile range (IQR = Q3 - Q1).")

  class(out) <- c("check", "data.frame")

  out
}


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


is_extreme <- function (x) {
  is_outlier(x, coef = 3)
}


#' Print 'check' object
#'
#' @param data a object of 'check'.
#' @param ... more.
#'
#' @keywords internal
#' @export
print.check <- function(data, ...){
  print_booktabs(data, ...)
}

