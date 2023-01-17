#' Append elements to objects
#'
#'
#' Append elements to a number of various objects as vectors, matrices, data.frames
#' and lists. In a matrix either rows or columns can be inserted at any position.
#' In data frames any vectors can be inserted. values will be recycled to the
#' necessary length.
#'
#'
#' @param x
#' object for the elements to be inserted.
#' @param values
#' the elements to be inserted.
#' @param after
#' a subscript, after which the values are to be appended. If it's missing the
#' values will be appended after the last element (or column/row).
#' @param ...
#' further arguments (not used here).
#'
#' @details
#' The vector x will be recycled to a length of the next multiple of the number
#' of rows (or columns) of the matrix m and will be inserted such that the first
#' inserted row (column) has the index i. If the dimnames are given, they will
#' be used no matter if the matrix m has already dimnames defined or not.
#'
#' @return
#' An object containing the values in x with the elements of values appended
#' after the specified element of x.
#'
#' @seealso [rbind], [cbind], [append]
#'
#' @export
#'
#' @examples
#' # the same as append
#' append2(1:5, 0:1, after = 3)
#'
#' # Insert columns and rows
#' x <- matrix(runif(25), 5)
#'
#' append2(x, values=1:10, after=2, names = c("X","Y"))
#' append2(x, values=1:10, after=2)
#'
#' # append to a data.frame
#' d.frm <- data.frame("id"   = c(1,2,3),
#'                     "code" = c("AAA", "BBB", "CCC"),
#'                     "val"  = c(111, 222, 333))
#' z <- c(10, 20, 30)
#'
#' append2(d.frm, z, after=2, names="ZZZ")
append2 <- function(x, values, after = NULL, ...){
  UseMethod("append2")
}


#' @rdname append2
#' @export
append2.default <- function(x, values, after = NULL, ...){
  if(is.character(after)){
    after <- which(names(x) == after)
  }
  if (is.null(after)) {
    after <- length(x)
  }
  append(x, values, after)
}

#' @rdname append2
#'
#' @param rows
#' logical, defining if vector should be added as row or as column. Default is
#' column (rows=FALSE).
#' @param names
#' the dimension names for the inserted elements(s).
#'
#' @export
append2.matrix <- function (x, values, after = NULL, rows = FALSE, names = NULL, ...) {
  if(is.character(after)){
    after <- which(names(x) == after)
  }
  if (rows) {
    nr <- dim(x)[1]
    if (is.null(after))
      after <- nr
    values <- matrix(values, ncol = ncol(x))
    if (!is.null(names)) {
      err <- try(row.names(x) <- names, silent = TRUE)
      if (inherits(err, "try-error"))
        warning("Could not set rownames.")
    }
    if (!after)
      res <- rbind(values, x)
    else if (after >= nr)
      res <- rbind(x, values)
    else res <- rbind(x[1L:after, , drop = FALSE], values,
                      x[(after + 1L):nr, , drop = FALSE])
    colnames(res) <- colnames(x)
  }
  else {
    nc <- dim(x)[2]
    if (missing(after))
      after <- nc
    values <- matrix(values, nrow = nrow(x))
    if (!is.null(names))
      colnames(values) <- names
    if (!after)
      res <- cbind(values, x)
    else if (after >= nc)
      res <- cbind(x, values)
    else res <- cbind(x[, 1L:after, drop = FALSE], values,
                      x[, (after + 1L):nc, drop = FALSE])
    rownames(res) <- rownames(x)
  }
  return(res)
}


#' @rdname append2
#' @export
append2.data.frame <- function (x, values, after = NULL, rows = FALSE, names = NULL, ...) {

  if(is.character(after)){
    after <- which(names(x) == after)
  }

  .InsertRow <- function(x, val, after = nrow(x)) {
    x[seq(after + 1, nrow(x) + 1), ] <- x[seq(after, nrow(x)), ]
    x[after, ] <- val
    x
  }
  if (rows) {
    output <- .InsertRow(x, values, after = after)
  }else {
    output <- as.data.frame(append(x, set_names(list(values), names = names),
                                   after = after),
                            check.names = FALSE)
  }

  attr.old <- attributes(x)
  attr.new <- attributes(output)
  attr.out <- attr.old
  attr.out$names <- attr.new$names
  attr.out$row.names <- attr.new$row.names
  attributes(output) <- attr.out
  output
}
