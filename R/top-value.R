#' Top values
#'
#' This function returns the top n largest and smallest values for each numeric
#' variable in the input data frame.
#'
#' @param data The input data frame.
#' @param varnames A character vector specifying the names of variables to
#' consider. If NULL, all numeric variables in the data frame are considered.
#' @param n The number of top values to return.
#' @param unique Logical indicating whether to consider unique values only.
#' Default is FALSE.
#'
#' @details
#' This function returns the top n largest and smallest values for each numeric
#' variable in the input data frame. The 'varnames' argument specifies the
#' variables to consider. If 'varnames' is NULL, all numeric variables in the
#' data frame are considered. The 'unique' argument, if set to TRUE, considers
#' only unique values when determining the top values.
#'
#' @return A list of data frames, where each data frame contains the top n
#' largest and smallest values for each numeric variable.
#'
#' @export
top <- function(data, varnames = NULL, n = 20, unique = FALSE){

  if(is_empty(varnames)){
    varnames <- names(data)
  }

  varnames <- select_numeric(data, varnames)

  lapply(varnames, \(x){
    if(unique){
      d <- unique(data[[x]])
    }else{
      d <- data[[x]]
    }
    data.frame(Largest   = utils::head(sort(d, decreasing = TRUE),  n = n),
               Smallest  = utils::head(sort(d, decreasing = FALSE), n = n))
  })
}
