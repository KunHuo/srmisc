#' Get the file path of the Desktop directory
#'
#' This function takes an optional argument file and returns the file path
#' of the Desktop directory. If no file argument is provided, it returns
#' the path of the Desktop directory. If a file argument is provided,
#' it returns the path of the Desktop directory with the file appended.
#'
#' @param file An optional character string representing the file name.
#'
#' @return A character string representing the file path.
#'
#' @examples
#' desktop()
#'
#' desktop(file = "hello.txt")
#'
#' @export
desktop <- function(file = ""){
  if(file == ""){
    file.path(Sys.getenv("USERPROFILE"), "Desktop")
  }else{
    paste(file.path(Sys.getenv("USERPROFILE"), "Desktop"), file, sep = "/")
  }
}
