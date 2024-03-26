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

