#' Pad Strings in a Column with Leading Spaces
#'
#' This function pads the values in a specified column of a data frame with leading spaces.
#' It also pads the column name itself with leading spaces.
#'
#' @param data A data frame containing the column to be padded.
#' @param col The name or index of the column to pad. If a name is provided, it must be a character string.
#' @param n.space The number of leading spaces to add (default is 5).
#'
#' @return A modified data frame with the specified column and its values padded with leading spaces.
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob", NA), age = c(25, 30, NA))
#' pad_left(df, "name", n.space = 3)
#' pad_left(df, 1, n.space = 2)
#'
#' @export
pad_left <- function(data, col, n.space = 5){

  # Select the column index based on the provided column name or index
  col <- select_variable(data = data, col, type = "index")

  # Pad the column name with leading spaces
  names(data)[col] <- paste0(strrep(" ", n.space), names(data)[col])

  # Pad each value in the column with leading spaces
  data[col] <- lapply(data[col], function(x){
    sapply(x, function(i){
      ifelse(is.na(i), NA, paste0(strrep(" ", n.space), i))
    })
  })

  # Return the modified data frame
  data

}
