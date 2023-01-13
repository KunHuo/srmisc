#' Change column order
#'
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param variables columns to move. It can be a variable name or an index in the data frame.
#' @param before destination of columns selected by variables. Supplying neither
#' will move columns to the left-hand side.
#' @param after destination of columns selected by variables. Supplying neither
#' will move columns to the right-hand side.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' head(mtcars)
#'
#' relocate(mtcars, variables = "am", before = "cyl")
#'
#' relocate(mtcars, variables = "am", before = 2)
#'
#' relocate(mtcars, variables = "am", after = "mpg")
#'
#' relocate(mtcars, variables = "am", after = 1)
#'
#' relocate(mtcars, variables = c("am", "vs"), before = 1)
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
#' that interval. Unlike [base::seq()], they return an empty vector if the starting
#' point is a larger integer than the end point.
#'
#' @param from the starting point of the sequence.
#' @param to the end point.
#'
#' @seealso [seq]
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
#' @description
#' Merge two data frames by left, includes all rows in x.
#'
#' @param x a data frame.
#' @param y a data frame.
#' @param by specifications of the columns used for merging.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' A <- data.frame(term = c("t1", "t2", "t3"), x = 1:3)
#' B <- data.frame(term = c("t1", "t3"), y = 4:5)
#'
#' merge_left(A, B, by = "term")
#' merge_left(B, A, by = "term")
merge_left <- function(x, y, by){
  x$.id <- 1:nrow(x)
  res <- merge(x, y, sort = FALSE, by = by, all.x = TRUE)
  res <- res[order(res$.id), ]
  res[, -which(names(res) == ".id")]
}


#' Merge two tables by the first column
#'
#' @description
#' Merge two tables by the first column, includes all rows in x.
#'
#' @param x a data frame.
#' @param y a data frame.
#' @param name.x name of x.
#' @param name.y name of y.
#' @param name.x.index start naming from which column of x.
#' @param name.y.index start naming from which column of y.
#' @param sep a character string to separate the terms.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' A <- data.frame(term = c("t1", "t2", "t3"), x = 1:3, z = 8:10)
#' B <- data.frame(term = c("t1", "t3"), y = 4:5, d = 20:21)
#'
#' merge_table(A, B)
#'
#' res <- merge_table(A, B, name.x = "A", name.y = "B")
#' res
#'
#' print_booktabs(res)
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
#'
#' @examples
#' dat <- data.frame(group = c("A", "B"), x = 1:2, y = 4:5)
#' dat
#' transpose(dat)
#' transpose(dat, varname = "variable")
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
#' @param include.id a logical, indicate whether add a id column, which dentify
#' multiple records from the same group/individual.
#' @param id.name names of one or more variables in long format that identify
#' multiple records from the same group/individual.
#' @param ... unused.
#'
#' @seealso [reshape_wide()] to reshape data from long to wide.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # Basic example
#' reshape_long(mtcars)
#'
#' # Do not add id column
#' reshape_long(mtcars, include.id = FALSE)
#'
#' # Reshape the specified variable.
#' reshape_long(mtcars, cols = c("mpg", "cyl", "disp"))
reshape_long <- function(data,
                         cols = names(data),
                         names.to = ".name",
                         values.to = ".value",
                         include.id = TRUE,
                         id.name = ".id",
                         ...){
  data <- as.data.frame(data)
  cols <- select_variable(data, cols)
  res <- stats::reshape(data,
                        direction = "long",
                        idvar = id.name,
                        ids = as.character(1:nrow(data)),
                        times   = cols,
                        timevar = names.to,
                        v.names = values.to,
                        varying = list(cols))

  if(include.id){
    res <- relocate(res, variables = id.name, before = 1)
  }else{
    res <- res[, -which(names(res) == id.name), drop = FALSE]
  }
  row.names(res) <- NULL
  res
}


#' Reshape data from long to wide
#'
#' @param data a data frame to reshape.
#' @param id names of one or more variables in long format that identify multiple
#' records from the same group/individual.
#' @param names.from an arguments describing which column (or columns) to get
#' the name of the output column.
#' @param values.from an arguments describing which column (or columns) to
#' get the cell values from.
#' @param include.id a logical, indicate whether add a id column, which dentify
#' multiple records from the same group/individual.
#' @param ... unused.
#'
#' @seealso [reshape_long()] to reshape data from wide to long.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' ldata <- reshape_long(iris, cols = 1:4)
#' ldata
#'
#' reshape_wide(ldata, id = ".id", names.from = ".name", values.from = ".value")
reshape_wide <- function(data, id, names.from, values.from, include.id = TRUE, ...){
  data <- as.data.frame(data)

  id <- select_variable(data, id)
  names.from <- select_variable(data, names.from)
  values.from <- select_variable(data, values.from)

  res <- stats::reshape(data,
                 direction = "wide",
                 idvar = id,
                 timevar = names.from,
                 v.names = values.from)

  row.names(res) <- NULL

  names(res) <- regex_replace(string = names(res),
                              pattern = paste0(values.from, "."),
                              replacement = "",
                              fixed = TRUE)
  if(!include.id){
    res <- res[, -which(names(res) == id), drop = FALSE]
  }
  res
}


