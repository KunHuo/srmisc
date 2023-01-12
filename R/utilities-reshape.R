#' Change column order
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param variables Columns to move. It can be a variable name or an index in the data frame.
#' @param before Destination of columns selected by variables. Supplying neither
#' will move columns to the left-hand side.
#' @param after Destination of columns selected by variables. Supplying neither
#' will move columns to the right-hand side.
#'
#' @return a data frame.
#' @export
relocate <- function(data, variables, before = NULL, after = NULL) {
  if (is.numeric(variables)) {
    check_index(data, variables)
    to_move <- variables
  } else{
    check_name(data, variables)
    to_move <- sapply(variables, function(x) { which(names(data) == x) })
    names(to_move) <- NULL
  }

  if (!is.null(before) && !is.null(after)) {
    stop("Must supply only one of `.before` and `.after`.")
  } else if (!is.null(before)) {
    if (is.numeric(before)) {
      check_index(data, before)
      where <- before
    } else{
      check_name(data, before)
      where <- which(names(data) == before)
    }
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  } else if (!is.null(after)) {
    if (is.numeric(after)) {
      check_index(data, after)
      where <- after
    } else{
      check_name(data, after)
      where <- which(names(data) == after)
    }
    if (!where %in% to_move) {
      to_move <- c(where, to_move)
    }
  } else {
    where <- 1L
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  }

  lhs <- setdiff(seq2(1, where - 1), to_move)
  rhs <- setdiff(seq2(where + 1, ncol(data)), to_move)

  pos <- unique(c(lhs, to_move, rhs))
  out <- data[pos]
  out
}


#' Increasing sequence of integers in an interval
#'
#'
#' These helpers take two endpoints and return the sequence of all integers within
#' that interval. Unlike base::seq(), they return an empty vector if the starting
#' point is a larger integer than the end point.
#'
#' @param from The starting point of the sequence.
#' @param to The end point.
#'
#' @return An integer vector containing a strictly increasing sequence.
#' @export
#'
#' @examples
#' seq2(2, 10)
#' seq2(10, 2)
#' seq(10, 2)
seq2 <- function(from, to) {
  if (length(from) != 1) {
    stop("`from` must be length one")
  }
  if (length(to) != 1) {
    stop("`to` must be length one")
  }
  if (from > to) {
    integer()
  }
  else {
    seq.int(from, to)
  }
}


#' Merge two data frames by left
#'
#' @param x a data frame.
#' @param y a data frame.
#' @param by specifications of the columns used for merging.
#'
#' @return a data frame.
#' @export
merge_left <- function(x, y, by){
  x$.id <- 1:nrow(x)
  res <- merge(x, y, sort = FALSE, by = by, all.x = TRUE)
  res <- res[order(res$.id), ]
  res[, -which(names(res) == ".id")]
}


#' Merge two tables  by the first column
#'
#' @param x a data frame.
#' @param y a data frame.
#' @param name.x name of x.
#' @param name.y name of y.
#' @param name.x.index Start naming from which column of x.
#' @param name.y.index Start naming from which column of y.
#' @param sep a character string to separate the terms.
#'
#' @return a data frame.
#' @export
merge_table <- function(x, y, name.x = NULL, name.y = NULL, name.x.index = 2, name.y.index = 2, sep = "__"){
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")

  if(!is.null(name.x)){
    names(x) <- c(names(x)[1:(name.x.index - 1)],
                  paste(name.x, names(x)[c(name.x.index:length(names(x)))], sep = sep))
  }

  if(!is.null(name.y)){
    names(y) <- c(names(y)[1:(name.y.index - 1)],
                  paste(name.y, names(y)[c(name.y.index:length(names(y)))], sep = sep))
  }

  x <- add_terms_column(x, which = 1)
  y <- add_terms_column(y, which = 1)
  y <- y[, -2, drop = FALSE]
  out <- merge_left(x, y, by = ".term")
  out <- out[, -1, drop = FALSE]

  class(out) <- x.class
  attr(out, "title") <- x.title
  attr(out, "note") <- x.note
  out
}


#' Data frame transpose
#'
#' @param x a data frame.
#' @param row.names.col specifies which column is the transposed row name,
#' default 1. If row.names.col=0, only row and column names are transposed.
#' @param varname variable name.
#'
#' @return a data frame.
#' @export
transpose <- function(x, row.names.col = 1, varname = NULL){

  title <- attr(x, "title")
  note  <- attr(x, "note")
  args  <- attr(x, "args")

  if(row.names.col == 0){
    x <- rownames_to_column(x, varname = "variable")
    row.names.col <- 1
  }

  if(is.null(varname)){
    varname <- names(x)[row.names.col]
  }

  o.class <- class(x)
  row.names <- x[[row.names.col]]
  x <- x[-row.names.col]
  col.names <- names(x)
  x <- t(x)
  x <- as.data.frame(x)
  names(x) <- row.names
  x <- append2(x, col.names, after = 0)
  names(x)[1] <- varname

  class(x) <- o.class
  attr(x, "title") <- title
  attr(x, "note")  <- note
  attr(x, "args")  <- args
  x
}

#' Separate a character column into multiple columns
#'
#' @param data a data frame.
#' @param varname column name or position to separate.
#' @param into names of new variables to create as character vector.
#' @param sep separator between columns.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param keep logical. Indicates whether to keep the column of 'varname'.
#' @param ... unused.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' df <- data.frame(x = c(NA, "x.y", "x.z", "z"))
#'
#' separate2cols(data = df, varname = "x")
#' separate2cols(data = df, varname = "x", keep = TRUE)
#' separate2cols(data = df, varname = "x", keep = TRUE, into = c("A", "B"))
separate2cols <- function(data, varname = NULL, into = NULL, sep = ".", fixed = TRUE, keep = FALSE, ...){

  varname <- select_variable(data, varname, type = "name")
  res <- regex_split(data[[varname]], pattern = sep, fixed = fixed)
  max <- max(sapply(res, length))

  res <- lapply(res, \(x){ c(x, rep(NA, max - length(x))) })
  res <- do.call(rbind, res)

  if(is.null(into)){
    new.names <- sprintf("%s.%d", varname, 1:ncol(res))
  }else{
    new.names <- into
  }

  colnames(res) <- new.names
  res <- as.data.frame(res)

  data <- append2(data, res, after = varname)

  if(keep){
    data
  }else{
    data[, -which(names(data) == varname), drop = FALSE]
  }
}

#' Reshape data from wide to long
#'
#' @param data a data frame to reshape.
#' @param cols columns to reshape into longer format.
#' @param names.to a character vector specifying the new column or columns to
#' create from the information stored in the column names of data specified by cols.
#' @param values.to a string specifying the name of the column to create from
#' the data stored in cell values.
#' @param add.id.col a logical, indicate whether add a id column, which dentify
#' multiple records from the same group/individual.
#' @param id.name names of one or more variables in long format that identify
#' multiple records from the same group/individual.
#' @param ... additional arguments passed on to methods.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' # Basic example
#' reshape_long(mtcars)
#'
#' # Add id column.
#' reshape_long(mtcars, add.id.col = TRUE)
#'
#' # Reshape the specified variable.
#' reshape_long(mtcars, cols = c("mpg", "cyl", "disp"))
reshape_long <- function(data,
                         cols = names(data),
                         names.to = ".name",
                         values.to = ".value",
                         add.id.col = FALSE,
                         id.name = ".id",
                         ...){

  class(data) <- "data.frame"

  if(is.numeric(cols)){
    cols <- names(data)[cols]
  }

  cols <- select_variable(data, cols)

  res <- stats::reshape(data,
                        direction = "long",
                        idvar = id.name,
                        ids = as.character(1:nrow(data)),
                        times   = cols,
                        timevar = names.to,
                        v.names = values.to,
                        varying = list(cols))

  if(add.id.col){
    res <- relocate(res, variables = id.name, before = 1)
  }else{
    res <- res[, -which(names(res) == id.name), drop = FALSE]
  }
  tibble::as_tibble(res)
}