#' Index Column
#'
#' Selects the index column(s) from a given data frame.
#'
#' @param data A data frame from which the index column(s) will be selected.
#' @param which A character vector specifying the name(s) of the
#' column(s) to be used as index.
#'
#' @return A vector of index column(s).
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' index_col(df, which = "a")
index_col <- function(data, which = NULL){
  select_variable(data, which, type = "index")
}



#' Delete Column
#'
#' Deletes the specified column(s) from a data frame.
#'
#' @param data A data frame from which the column(s) will be deleted.
#' @param which A character vector specifying the name(s) of the column(s)
#' to be deleted.
#'
#' @return A data frame with the specified column(s) removed.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' delete_col(df, which = "a")
delete_col <- function(data, which = NULL){
  data[-index_col(data, which = which)]
}
