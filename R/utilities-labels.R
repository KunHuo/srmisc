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


#' Get label of variable in a data frame
#'
#' @description The function gets the label of the variable in a data frame,
#'  and returns the variable name if the label has no label.
#'
#' @param data a data frame.
#' @param varname variable name.
#'
#' @return a string.
#' @export
#'
#' @examples
#' get_label(iris, varname = "Species")
#'
#' attr(iris$Species, "label") <- "Species category"
#' get_label(iris, varname = "Species")
get_label <- function(data, varname) {
  varname <- select_variable(data, varname)
  label <- attr(data[[varname]], "label")
  if (is.null(label)) {
    varname
  } else {
    label
  }
}


#' Extract terms from a variable
#'
#' @param data a data frame.
#' @param which terms form which column, default the first column.
#'
#' @return
#' A data frame contains the columns of .term, .varname, .code.
#' * .term: identify unique rows.
#' * .varname: variable names.
#' * .code: variblae codes.
#' @export
#'
#' @examples
#' dat <- data.frame(variblae = c("age",
#'                                "sex", "  1", "  2",
#'                                "race", "  1", "  2", " 3",
#'                                "size",
#'                                "metastasis", "  0", "  1",
#'                                "status", "  0", "  1",
#'                                "time"),
#'                   meansd = sprintf("%.2f (%.2f)", rnorm(16) * 10, rnorm(16) * 2 ))
#' dat
#'
#' extract_terms(dat)
extract_terms <- function(data, which = 1){
  which <- select_variable(data, which)
  variables <- data[[which]]
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
  data.frame(.term = term, .varname = varname, .code = code)
}


#' Add term columns to a data frame
#'
#' @param data a data frame.
#' @param which terms form which column, default the first column.
#' @param columns which columns will be added, one or more of 'term', 'varname',
#' and 'code'.
#' @param .before one-based column index or column name where to add the new columns,
#' default: before first column.
#' @param .after one-based column index or column name where to add the new columns,
#' default: before first column.
#'
#' @return A data frame contains the terms.
#' @export
#'
#' @examples
#' dat <- data.frame(variblae = c("age",
#'                                "sex", "  1", "  2",
#'                                "race", "  1", "  2", " 3",
#'                                "size",
#'                                "metastasis", "  0", "  1",
#'                                "status", "  0", "  1",
#'                                "time"),
#'                   meansd = sprintf("%.2f (%.2f)", rnorm(16) * 10, rnorm(16) * 2 ))
#' dat
#'
#' add_terms_columns(dat)
add_terms_columns <- function(data,
                              which = 1,
                              columns = c("term", "varname", "code"),
                              .before = NULL,
                              .after = NULL){
  terms <- extract_terms(data, which = which)

  columns <- paste0(".", columns)

  terms <- terms[, columns, drop = FALSE]

  if(is.null(.before) & is.null(.after)){
    .before <- 1
  }
  tibble::add_column(data, terms, .after = .after, .before = .before)
}


#' Add labels to the first column of a data frame
#'
#' @param data a data frame.
#' @param codes a data frame contain the column of variable, code, and label.
#' @param col col index.
#'
#' @return a data frame.
#' @export
add_lables <- function(data, codes, col = 1){
  tdata <- extract_terms(data, which = col)
  for(i in 1:nrow(data)){
    label <- find_labels(codes, varname = tdata$.term[i])
    if(!is_empty(label)){
      data[[col]][i] <- regex_replace(string = data[[col]][i],
                                      pattern = trimws(data[[col]][i]),
                                      replacement = label)
    }
  }
  data
}


#' Tidy codes
#'
#' @param data a data frame contains the 3 columns of variable, code, and label.
#' Note, The order of columns cannot be changed.
#'
#' @return A data frame contains the columns of .term, .varname, .code, and .label.
#' @export
tidy_codes <- function(data){
  term0 <- data[, 1, drop = TRUE]
  term1 <- term0

  for (i in seq_along(term1)) {
    if (i != 1) {
      if (is.na(term1[i])) {
        term1[i] <- term1[i - 1]
      }
    }
  }
  data[1] <- term1

  names(data) <- c(".varname", ".code", ".label")
  data <- tibble::add_column(data, .term = data[, 1, drop = TRUE], .after = 0)

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

  data <- tidy_codes(data)

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


set_codes <- function(data, codes, as.factor = TRUE, exclude = "") {
  codes <- tidy_codes(codes)

  varnames <- names(data)

  for (i in seq_along(varnames)) {
    # code for each variable
    code <- codes[codes$.varname == varnames[i], ]

    # exclude
    if (!(varnames[[i]] %in% exclude)) {
      # exec if find code
      if (nrow(code) != 0L) {
        if (nrow(code) >= 2L) {

        }

        if (!is.na(code[1, 4][[1]])) {
            attr(data[[varnames[i]]], "label") <- code[1, 4][[1]]
        }
      }
    }
  }

  data

}

