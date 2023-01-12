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
#' @param varnames variable names，Indicates to select the corresponding data
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
#' # return variable names
#' select_character(lung)
#' select_numeric(lung)
#' select_factor(lung)
#' select_category(lung)
#'
#' # return col index
#' select_numeric(lung, type = "index")
#'
#' # return data frame
#' select_numeric(lung, type = "data")
#'
#' # select in varnames
#' select_numeric(lung, varnames = 1:4)
#' select_numeric(lung, varnames = "time:ph.karno")
#' select_numeric(lung, varnames = c("time:ph.karno", "wt.loss"))
#'
#' select_variable(lung, c("time:age", "wt.loss"), 5)
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

  switch(type,
         data  = data[index],
         name  = {
                  varname <- names(data)[index]
                  names(varname) <- varname
                  varname
           },
         index = index)
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
      sapply(x, function(i){
        if(regex_detect(i, pattern = ":", fixed = TRUE)){
          st <- regex_split(i, pattern = ":", fixed = TRUE)[[1]]
          check_name(data, st[1])
          check_name(data, st[2])
          start <- which(names(data) == st[1])
          end   <- which(names(data) == st[2])
          start:end
        }else{
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
