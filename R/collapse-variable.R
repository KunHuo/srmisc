#' Collapse a Data Frame by a Specific Variable
#'
#'
#' @param data A data frame to be collapsed.
#' @param which A character string specifying the variable by which to collapse
#' the data frame.
#' @param drop logical indicating if levels that do not occur should be dropped
#' (if f is a factor or a list).
#'
#' @return A data frame collapsed by the specified variable.
#'
#' @export
collapse_variable <- function(data, which = NULL, drop = FALSE){
  which <- select_variable(data, which)
  listdata <- split.data.frame(data, data[[which]], drop = drop)
  listdata <- lapply(listdata, \(d){
    delete_col(d, which)
  })
  list_rbind(listdata, collapse.names = TRUE, varname = which)
}
