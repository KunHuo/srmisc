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
#'
#' select_variable(iris, 1:4)
#' select_variable(iris, "Species")
#' select_variable(iris, "Species", type = "index")
#' select_variable(iris, "Species", type = "data")
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
