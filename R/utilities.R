#' Is object an empty vector or NULL and NA?
#'
#' @param x object to test.
#'
#' @return a logical.
#' @export
#'
#' @examples
#' is_empty(NULL)
#' is_empty(list())
#' is_empty(list(NULL))
is_empty <- function(x){
  length(x) == 0L
}


delete_duplicate_values <- function(x){
  for(i in rev(seq_along(x))){
    if(i != 1){
      if(x[i] == x[i - 1]){
        x[i] <- NA
      }
    }
  }
  return(x)
}
