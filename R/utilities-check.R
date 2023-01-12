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
#' check_name(mtcars, varnames = c("am", "vs))
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
