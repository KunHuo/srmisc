#' Add a title to a specific row in a data frame
#'
#' This function adds a title to a specific row in a data frame,
#' by modifying the column names.
#'
#' @param data A data frame.
#' @param which The row number to add the title to.
#' @param title The title to add to the row.
#'
#' @return The modified data frame with the title added to the specified row.
#'
#' @export
add_rowtitle <- function(data, which, title){
  which <- select_variable(data, which, type = "index")
  names(data)[which] <- paste(title, names(data)[which], sep = "__")
  data
}
