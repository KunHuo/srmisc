#' First N Elements of a Vector
#'
#' This function returns the first `n` elements of a vector `x`. If `n` is
#' greater than or equal to the length of `x`, it returns the entire vector.
#'
#' @param x A vector from which the first `n` elements are to be extracted.
#' @param n An integer specifying the number of elements to extract. The default
#' is the length of `x`.
#'
#' @return A vector containing the first `n` elements of `x`.
#'
#' @examples
#' # Get the first 3 elements of a vector
#' first_n(1:10, 3)
#'
#' # Get all elements if n is greater than the length of the vector
#' first_n(1:5, 10)
#'
#' @export
first_n <- function(x, n = length(x)){
  if(n >= length(x) | n < 0){
    x
  }else{
    x[1:n]
  }
}


#' Last N Elements of a Vector
#'
#' This function returns the last `n` elements of a vector `x`. If `n` is
#' greater than or equal to the length of `x`, it returns the entire vector.
#'
#' @param x A vector from which the last `n` elements are to be extracted.
#' @param n An integer specifying the number of elements to extract. The default
#' is the length of `x`.
#'
#' @return A vector containing the last `n` elements of `x`.
#'
#' @examples
#' # Get the last 3 elements of a vector
#' last_n(1:10, 3)
#'
#' # Get all elements if n is greater than the length of the vector
#' last_n(1:5, 10)
#'
#' @export
last_n <- function(x, n = length(x)){
  if(n >= length(x) | n < 0){
    x
  }else{
    x[(length(x) - n + 1):length(x)]
  }
}
