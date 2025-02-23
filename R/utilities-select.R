#' Select variables
#'
#' @description Functions select variables in a data frame.
#'
#' * select_numeric: select numeric variables.
#' * select_factor: select factor variables.
#' * select_character: select character variables.
#' * select_category: select factor and character variables.
#' * select_logical: select logical variables.
#' * select_variable: select variables.
#'
#' @param data a data frame.
#' @param varnames variable names, Indicates to select the corresponding data
#' type in the specified variable, if it is NULL, it means to select from all
#' variables.
#' @param type return type, 'name' for a vector of character, 'index' for a
#' vector of integer, and 'data' for a data frame.
#'
#' @return depends on the parameter type. 'name' for a vector of character,
#' 'index' for a vector of integer, and 'data' for a data frame.
#'
#' @export
#'
#' @examples
#' select_numeric(iris)
#' select_factor(iris)
select_numeric <- function(data, varnames = NULL, type = c("name", "data", "index")){
  .select_impl(data = data,
               varnames = varnames,
               var.type = "numeric",
               out.type = type)
}


#' @rdname select_numeric
#' @export
select_character <- function(data, varnames = NULL, type = c("name", "data", "index")){
  .select_impl(data = data,
               varnames = varnames,
               var.type = "character",
               out.type = type)
}


#' @rdname select_numeric
#' @export
select_factor <- function(data, varnames = NULL, type = c("name", "data", "index")){
  .select_impl(data = data,
               varnames = varnames,
               var.type = "factor",
               out.type = type)
}


#' @rdname select_numeric
#' @export
select_category <- function(data, varnames = NULL, type = c("name", "data", "index")){
  .select_impl(data = data,
               varnames = varnames,
               var.type = "category",
               out.type = type)
}


#' @rdname select_numeric
#' @export
select_logical <- function(data, varnames = NULL, type = c("name", "data", "index")){
  .select_impl(data = data,
               varnames = varnames,
               var.type = "logical",
               out.type = type)
}

#' @rdname select_numeric
#' @param ... variables to select.
#' @export
select_variable <- function(data, ..., type = c("name", "data", "index")){

  type  <- match.arg(type)

  if(length(c(...)) == 0L){
    return(NULL)
  }

  index <- .col_index(data, ...)

  if(length(index) == 0L){
    return(NULL)
  }

  if(type == "name"){
    .col_index2(data, ...)
  }else{

    switch(type,
           data  = data[index],
           name  = {
             varname <- names(data)[index]
             names(varname) <- varname
             varname
           },
           index = index)
  }
}


.col_index <- function(data, ...){
  varnames <- list(...)
  res <- lapply(varnames, function(x){
    if(is.numeric(x)){
      if(max(x) > ncol(data) | min(x) <= 0){
        stop("Out of range for column index.", call. = FALSE)
      }
      x
    }else{
      x <- cc(x)
      sapply(x, function(i){
        if(regex_detect(i, pattern = ":", fixed = TRUE)){
          st <- regex_split(i, pattern = ":", fixed = TRUE)[[1]]

          start <- st[1]
          end   <- st[2]

          if(start == ".first"){
            start <- names(data)[1]
          }

          if(end == ".first"){
            end <- names(data)[1]
          }

          if(start == ".last"){
            start <- names(data)[ncol(data)]
          }
          if(end == ".last"){
            end <- names(data)[ncol(data)]
          }

          check_name(data, start)
          check_name(data, end)

          start <- which(names(data) == start)
          end   <- which(names(data) == end)
          start:end
        }else if(regex_detect(i, pattern = "*", fixed = TRUE)){
          st <- regex_split(i, pattern = "*", fixed = TRUE)[[1]]
          check_name(data, st[1])
          check_name(data, st[2])
          c(which(names(data) == st[1]), which(names(data) == st[2]))
        } else{

          if(i == ".first"){
            i <- names(data)[1]
          }

          if(i == ".last"){
            i <- names(data)[ncol(data)]
          }
          check_name(data, i)
          which(names(data) == i)
        }
      })
    }
  })
  res <- unique(unlist(res))
  names(res) <- names(data)[res]
  res
}


.col_index2 <- function(data, ...){
  varnames <- list(...)
  res <- lapply(varnames, function(x){
    if(is.numeric(x)){
      if(max(x) > ncol(data) | min(x) <= 0){
        stop("Out of range for column index.", call. = FALSE)
      }
      names(data)[x]
    }else{
      x <- cc(x)
      sapply(x, function(i){
        if(regex_detect(i, pattern = ":", fixed = TRUE)){
          st <- regex_split(i, pattern = ":", fixed = TRUE)[[1]]

          start <- st[1]
          end   <- st[2]

          if(start == ".last"){
            start <- names(data)[ncol(data)]
          }

          if(end == ".last"){
            end <- names(data)[ncol(data)]
          }

          if(start == ".first"){
            start <- names(data)[1]
          }

          if(end == ".first"){
            end <- names(data)[1]
          }

          check_name(data, start)
          check_name(data, end)
          start <- which(names(data) == start)
          end   <- which(names(data) == end)
          names(data)[start:end]
        }else if(regex_detect(i, pattern = "*", fixed = TRUE)){
          st <- regex_split(i, pattern = "*", fixed = TRUE)[[1]]
          check_name(data, st[1])
          check_name(data, st[2])
          c(st[1], st[2], paste(st[1], st[2], sep = ":"))
        } else{

          if(i == ".first"){
            i <- names(data)[1]
          }

          if(i == ".last"){
            i <- names(data)[ncol(data)]
          }
          check_name(data, i)
          i
        }
      })
    }
  })
  res <- unique(unlist(res))
  # names(res) <- names(data)[res]
  res
}

.select_impl <- function(data,
                         varnames = NULL,
                         var.type = c("numeric", "factor", "character", "logical", "category"),
                         out.type = c("name", "data", "index"),
                         ...){

  var.type <- match.arg(var.type)
  out.type <- match.arg(out.type)

  if(is.null(varnames)){
    varnames <- names(data)
  }else{
    varnames <- select_variable(data, varnames, type = "name")
  }

  varnames <- switch(var.type,
                     numeric   = varnames[sapply(data[varnames], is.numeric)],
                     factor    = varnames[sapply(data[varnames], is.factor) ],
                     character = varnames[sapply(data[varnames], is.character) ],
                     logical   = varnames[sapply(data[varnames], is.logical)],
                     category  = varnames[sapply(data[varnames], \(x){ is.factor(x) | is.character(x) })])

  if(length(varnames) == 0L){
    return(switch(out.type,
                  data  = data.frame(),
                  name  = character(),
                  index = integer()))
  }

  names(varnames) <- varnames

  switch(out.type,
         data  = data[varnames],
         name  = varnames,
         index = sapply(varnames, \(x) {which(names(data) == x)}))
}

#' Capture Variables from an Expression
#'
#' This function processes an input expression and returns its components as
#' characters. It handles different types of inputs including numeric,
#' character, symbol, and language (R expressions), and converts them into
#' a character representation.
#'
#' @param x An R expression (numeric, character, symbol, or language).
#'
#' @return A character vector or string representing the input expression.
#' If the input is a numeric or character vector, it returns the input as is.
#' If the input is a symbol, it returns the symbol's name as a string.
#' If the input is a language object (R expression), it deparses the expression
#' into a character string or extracts the elements if the expression is a call
#' to `c()`.
#'
#' @examples
#' capture_vars(42)  # Returns "42"
#' capture_vars("abc")  # Returns "abc"
#' capture_vars(x)  # Returns "x" (if x is a symbol)
#' capture_vars(c(1, 2, 3))  # Returns "1" "2" "3"
#'
#' @export
capture_vars <- function(x) {
  x_expr <- x

  if (is.numeric(x_expr)) {
    x <- x_expr
  }

  if (is.character(x_expr)) {
    x <- x_expr
  }

  if (is.symbol(x_expr)) {

    x_name <- deparse(x)
    if (exists(x_name, envir = parent.frame())) {
      x <- eval(get(x_name), envir = parent.frame())
      return(x)
    } else {
      x <- as.character(x_expr)
    }
  } else if (is.language(x_expr)) {
    if (is.call(x_expr) && x_expr[[1]] == quote(c)) {
      x <- as.character(x_expr)[-1]
    } else {
      x <- deparse(x_expr)
    }
  }

  convert_to_numeric(x)
}


convert_to_numeric <- function(x) {
  result <- c()  # Initialize an empty vector to store the result

  for (i in x) {
    # Check if the element is in the format "start:end"
    if (grepl("^\\d+:\\d+$", i)) {
      # Split the range and convert to a numeric vector
      range_vals <- as.numeric(strsplit(i, ":")[[1]])
      result <- c(result, seq(range_vals[1], range_vals[2]))
    } else if (grepl("^\\d+$", i)) {
      # If it's a simple number, convert to numeric and add to the result
      result <- c(result, as.numeric(i))
    } else {
      result <- c(result, i)
    }
  }
  return(result)
}


#' Select Variables from a Data Frame
#'
#' This function selects variable names from a data frame based on provided column indices or character strings.
#' The selection can be based on individual column names, ranges, or pairs of column names using a specific format.
#'
#' @param data A data frame from which variables are to be selected.
#' @param varnames A vector of column names or indices, or a vector of ranges in the format `"start:end"` or pairs of column names `"col1*col2"`.
#'
#' @return A vector of selected variable names from the data frame.
#'
#' @details
#' - If the input `varnames` contains column indices (numeric values), the function checks whether they are within the valid range of columns in the data frame.
#' - If the input is in the form of column name ranges (e.g., `"col1:col5"`), the function selects variables from the specified range.
#' - If the input is a pair of column names (e.g., `"col1*col2"`), both columns and their concatenation (e.g., `"col1:col2"`) are selected.
#' - Special cases include `.first` and `.last`, which refer to the first and last columns of the data frame, respectively.
#'
#' @export
select_vars <- function(data, varnames){

  varnames <- capture_vars(varnames)

  res <- lapply(varnames, function(x){
    if(is.numeric(x)){
      if(max(x) > ncol(data) | min(x) <= 0){
        stop("Out of range for column index.", call. = FALSE)
      }
      names(data)[x]
    }else{
      x <- cc(x)
      sapply(x, function(i){
        if(regex_detect(i, pattern = ":", fixed = TRUE)){
          st <- regex_split(i, pattern = ":", fixed = TRUE)[[1]]

          start <- st[1]
          end   <- st[2]

          if(start == ".last"){
            start <- names(data)[ncol(data)]
          }

          if(end == ".last"){
            end <- names(data)[ncol(data)]
          }

          if(start == ".first"){
            start <- names(data)[1]
          }

          if(end == ".first"){
            end <- names(data)[1]
          }

          check_name(data, start)
          check_name(data, end)
          start <- which(names(data) == start)
          end   <- which(names(data) == end)
          names(data)[start:end]
        }else if(regex_detect(i, pattern = "*", fixed = TRUE)){
          st <- regex_split(i, pattern = "*", fixed = TRUE)[[1]]

          st1 <- trimws(st[1])
          st2 <- trimws(st[2])

          check_name(data, st1)
          check_name(data, st2)
          c(st1, st2, paste(st1, st2, sep = ":"))
        } else{

          if(i == ".first"){
            i <- names(data)[1]
          }

          if(i == ".last"){
            i <- names(data)[ncol(data)]
          }
          check_name(data, i)
          i
        }
      })
    }
  })
  res <- unique(unlist(res))
  # names(res) <- names(data)[res]
  res

}
