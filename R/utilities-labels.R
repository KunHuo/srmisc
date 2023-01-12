extract_terms <- function(x, which = 1){
  if(is.character(which)){
    which(names(x) == which)
  }
  variables <- x[[which]]
  term <- vector(length = length(variables))
  varname <- vector(length = length(variables))
  code <- rep(NA, length(variables))

  for(i in seq_along(variables)){

    if(regex_detect(variables[i], pattern = "^\\s")){
      tmp <- rev(variables[1:i])
      tmp <- regex_extract(tmp, pattern = "^\\S.*")
      tmp <- tmp[tmp != ""][1]
      term[i] <- paste0(tmp, trimws(variables[i]))
      code[i] <- trimws(variables[i])
      varname[i] <- tmp

    }else{
      term[i] <- variables[i]
      varname[i] <- variables[i]
    }
  }

  data.frame(.term = term, .code = code,  .varname = varname)
}


add_terms_column <- function(x, which = 1){
  terms <- extract_terms(x, which = which)
  terms <- terms[, 1, drop = FALSE]
  tibble::add_column(x, terms, .after = 0)
}


add_varnames_column <- function(x, which = 1){
  terms <- extract_terms(x, which = which)
  terms <- terms[, 3, drop = FALSE]
  tibble::add_column(x, terms, .after = 0)
}


#' Add title attribute to a data frame
#'
#' @param x a data frame.
#' @param value a character string.
#'
#' @return a data frame.
#' @export
add_title <- function(x, value = NULL){
  attr(x, "title") <- value
  x
}



#' Add note attribute to a data frame
#'
#' @param x a data frame.
#' @param value a character string.
#' @param append a logical.
#'
#' @return a data frame.
#' @export
add_note <- function(x, value = NULL, append = TRUE){
  if(is_empty(value)){
    attr(x, "note") <- NULL
  }else{
    if(append){
      note <- attr(x, "note")
      if(is_empty(note)){
        attr(x, "note") <- value
      }else{
        attr(x, "note") <- paste(note, value, sep = "\n")
      }
    }else{
      attr(x, "note") <- value
    }
  }
  x
}


#' Add labels to  the first column of a data frame
#'
#' @param data a data frame.
#' @param ldata a data frame contain the column of term, code, and label.
#' @param col col index.
#'
#' @return a data frame.
#' @export
add_lables <- function(data, ldata, col = 1){
  tdata <- extract_terms(data, which = col)
  for(i in 1:nrow(data)){
    label <- find_labels(ldata, varname = tdata$.term[i])
    if(!is_empty(label)){
      data[[col]][i] <- regex_replace(string = data[[col]][i],
                                      pattern = trimws(data[[col]][i]),
                                      replacement = label)
    }
  }
  data
}

tidy_labels <- function(data = NULL){

  if(is_empty(data)){
    return(NULL)
  }
  term0 <- data[[1]]

  data[, 1] <- lapply(data[, 1], function(v) {
    for (i in seq_along(v)) {
      if (i != 1) {
        if (is.na(v[i])) {
          v[i] <- v[i - 1]
        }
      }
    }
    v
  })
  names(data) <- c(".varname", ".code", ".label")
  data <- tibble::add_column(data, .term = data[[1]], .after = 0)

  for(i in 1:nrow(data)){
    if(is.na(term0[i])){
      if(!is.na(data$.code[i])){
        data$.term[i] <- paste0(data$.term[i], data$.code[i])
      }
    }
  }
  data
}


# If can not find return NULL.
find_labels <- function(data, varname, code = NA, defalut = NULL){

  data <- tidy_labels(data)

  if(is_empty(data)){
    return(defalut)
  }
  if(is.na(code) | code == ""){
    x <- varname
  }else{
    x <- paste(varname, code, sep = "")
  }

  if(is_empty(which(as.character(data$.term) == as.character(x)))){
    return(defalut)
  }

  data$.label[which(as.character(data$.term )== as.character(x))]
}
