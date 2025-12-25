#' Complete Data
#'
#' This function removes rows with missing values from the input data frame. It
#' can be applied to the entire data frame or to specific variables.
#'
#' @param data The input data frame.
#' @param varnames A character vector specifying the names of variables to
#' consider. If NULL, all variables in the data frame are considered.
#' @param ... Additional arguments passed to other functions.
#'
#' @details
#' This function removes rows with missing values either from the entire data
#' frame or from specific variables specified by 'varnames'. If 'varnames' is
#' NULL, all variables in the data frame are considered for the removal of rows
#' with missing values. If 'varnames' is specified, only the specified variables
#' are considered.
#'
#' @return A data frame with complete cases, i.e., rows with no missing values.
#'
#' @examples
#' data <- data.frame(
#'   x1 = c(1, 2, NA, 4),
#'   x2 = c(NA, 2, 3, 4),
#'   x3 = c(1, NA, 3, 4)
#' )
#' complete_data(data)
#'
#' complete_data(data, varnames = "x1")
#'
#' complete_data(data, varnames = c("x1", "x2"))
#'
#' @export
complete_data <- function(data, varnames = NULL, ...){


  var_labels <- srmisc::get_var_label(data)

  varnames <- srmisc::select_variable(data, varnames)

  if(srmisc::is_empty(varnames)){
    return(stats::na.omit(data))
  }

  index <- sapply(varnames, \(x){
    !stats::complete.cases(data[[x]])
  })

  index <- apply(index, 1, any)

  data <- data[!index, ]

  data <- srmisc::set_var_label(data, var_labels)
  return(data)
}

