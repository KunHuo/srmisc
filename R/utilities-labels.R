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
#'                                "meta", "  0", "  1",
#'                                "status", "  0", "  1",
#'                                "time"),
#'                   meansd = sprintf("%.2f (%.2f)", rnorm(16) * 10, rnorm(16) * 2))
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
#' @param after one-based column index or column name where to add the new columns,
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
#'                                "meta", "  0", "  1",
#'                                "status", "  0", "  1",
#'                                "time"),
#'                   meansd = sprintf("%.2f (%.2f)", rnorm(16) * 10, rnorm(16) * 2))
#' dat
#'
#' add_terms_columns(dat)
add_terms_columns <- function(data,
                              which = 1,
                              columns = c("term", "varname", "code"),
                              after = NULL){
  terms <- extract_terms(data, which = which)

  columns <- paste0(".", columns)

  terms <- terms[, columns, drop = FALSE]

  if(is.null(after)){
    after <- 0
  }

  append2(data, terms, after = after)
}


#' Tidy codes
#'
#' @param data a data frame contains the 3 columns of variable, code, and label.
#' Note, The order of columns cannot be changed.
#'
#' @return A data frame contains the columns of .term, .varname, .code, and .label.
#' @keywords internal
#' @export
#'
#' @examples
#' data(cancer.codes)
#' cancer.codes
#' tidy_codes(cancer.codes)
tidy_codes <- function(data = NULL){

  if(is.null(data)){
    return(NULL)
  }

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

  if(ncol(data == 3L)){
    names(data) <- c(".varname", ".code", ".label")
  }else{
    names(data) <- c(".varname", ".code", ".label", ".abbr")
  }

  data <- append2(data, data[, 1, drop = TRUE], names = ".term", after = 0)

  for(i in 1:nrow(data)){
    if(is.na(term0[i])){
      if(!is.na(data$.code[i])){
        data$.term[i] <- paste0(data$.term[i], data$.code[i])
      }
    }
  }
  data
}


#' Switch a column to labels
#'
#' @param data a data frame.
#' @param codes a data frame contain the column of variable, code, and label.
#' @param which set labels to which column, default the first column.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' dat <- data.frame(variblae = c("age",
#'                                "sex", "  1", "  2",
#'                                "race", "  1", "  2", " 3",
#'                                "size",
#'                                "meta", "  0", "  1",
#'                                "status", "  0", "  1",
#'                                "time"),
#'                   meansd = sprintf("%.2f (%.2f)", rnorm(16) * 10, rnorm(16) * 2))
#' dat
#'
#' print(cancer.codes)
#' switch2labels(dat, cancer.codes)
switch2labels <- function(data, codes, which = 1){
  tdata <- extract_terms(data, which = which)
  for(i in 1:nrow(data)){
    label <- find_labels(codes, varname = tdata$.term[i])
    if(!is_empty(label)){
      data[[which]][i] <- regex_replace(string = data[[which]][i],
                                      pattern = trimws(data[[which]][i]),
                                      replacement = label)
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


#' Set codes to a data frame
#'
#' @param data a data frame.
#' @param codes a data frame contains the 3 columns of variable, code, and label.
#' Note, The order of columns cannot be changed.
#' @param as.factor a logical indicating whether to convert codes to factors,
#' default TRUE.
#' @param exclude excluded variables when as.factor == TRUE.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' data("cancer")
#' data("cancer.codes")
#'
#' head(cancer)
#' print(cancer.codes)
#'
#' str(codes2labels(data = cancer, codes = cancer.codes))
#' str(codes2labels(data = cancer, codes = cancer.codes, exclude = "status"))
#' str(codes2labels(data = cancer, codes = cancer.codes, as.factor = FALSE))
codes2labels <- function(data, codes, as.factor = TRUE, exclude = "") {
  codes <- tidy_codes(codes)

  varnames <- names(data)

  if(exclude != ""){
    exclude <- select_variable(data, exclude)
  }

  for (i in seq_along(varnames)) {
    # code for each variable
    code <- codes[codes$.varname == varnames[i], ]

    # exec if find code
    if (nrow(code) != 0L) {

      if (nrow(code) >= 2L) {

        values <- code$.code[-1]
        labels <- code$.label[-1]

        if (as.factor & !(varnames[[i]] %in% exclude)) {
          data[[varnames[[i]]]] <- labels[match(data[[varnames[i]]], values)]
          data[[varnames[[i]]]] <- factor(data[[varnames[[i]]]], levels = labels)
        }else{
          names(values) <- labels
          attr(data[[varnames[i]]], "labels") <- values
          class(data[[varnames[i]]]) <- class(data[[varnames[i]]]) # "haven_labelled", "vctrs_vctr",
        }

      } # End for 'nrow(code) >= 2L'

      # set label
      if (!is.na(code[1, 4, drop = TRUE])) {
        attr(data[[varnames[i]]], "label") <- code[1, 4, drop = TRUE]
      }

      if(ncol(code) == 5L){
        if (!is.na(code[1, 5, drop = TRUE])) {
          attr(data[[varnames[i]]], "abbr") <- code[1, 5, drop = TRUE]
        }

      }

    } # End for 'nrow(code) != 0L'

  } # End for loop

  data
}


#' Data coding information
#'
#' This function generates coding information for each variable in the data frame.
#'
#' @param data A data frame containing the variables.
#' @param language Specify language, 'en' is English, 'cn' or 'zh' is Chinese.
#' Default is 'en'.
#' @param ... unused.
#'
#' @return A data frame containing the coding information.
#'
#' @seealso [codes2()], [codes_t()].
#'
#' @examples
#' codes(iris)
#'
#' @export
codes <- function(data, language = "en", ...){
  exec <- function(x){
    var   <- x
    code  <- NA
    label <- get_var_label(data, x, default = ".name")

    d <- data[[x]]

    if(is.factor(d)){
      code <- c(NA, 1:nlevels(d))
      label <- c(label, levels(d))
    }

    var   <- c(x, rep(NA, length(code) - 1))
    data.frame(Variable = var, Code = code, Label = label)
  }

  out <- lapply(names(data), exec)
  out <- do.call(rbind, out)

  row.names(out) <- NULL

  if(language != "en"){
    names(out) <- c("\u53d8\u91cf", "\u7f16\u7801", "\u6807\u7b7e")
  }

  attr(out, "title") <- ifelse(language == "en", "Data coding information", "\u6570\u636e\u7f16\u7801\u4fe1\u606f")
  class(out) <- c("codes", "data.frame")
  out
}


#' Data coding information
#'
#' This function generates coding information for each variable in the data frame.
#'
#' @param data A data frame containing the variables.
#' @param language Specify language, 'en' is English, 'cn' or 'zh' is Chinese.
#' Default is 'en'.
#' @param ... unused.
#'
#' @return A data frame containing the coding information.
#'
#' @seealso [codes()], [codes_t()].
#'
#' @examples
#' codes2(iris)
#'
#' @export
codes2 <- function(data, language = "en", ...){

  out <- lapply(names(data), \(x){
    if(is.factor(data[[x]])){
      # If it is a factor, create coding information
      code <- paste(sprintf("%d: %s", 1:nlevels(data[[x]]), levels(data[[x]])), collapse = "; ")
    }else{
      # If not a factor, leave coding information empty
      code <- ""
    }

    # Create a data frame with Variable, Label, Type, and Code columns
    data.frame(Variable = x,
               Label  = get_var_label(data, x, default = ".name"),
               Type = data_type(class(data[[x]]), language),
               Code = code)
  })
  # Combine the list of data frames into a single data frame
  out <- list_rbind(out, names.as.column = FALSE)

  if(language != "en"){
    names(out) <- c("\u53d8\u91cf",
                    "\u6807\u7b7e",
                    "\u6570\u636e\u7c7b\u578b",
                    "\u7f16\u7801")
  }

  attr(out, "title") <- ifelse(language == "en", "Data coding information", "\u6570\u636e\u7f16\u7801\u4fe1\u606f")
  class(out) <- c("codes", "data.frame")

  out
}


data_type <- function(type, language = "en"){

  type <- type[length(type)]

  if(language == "en"){
    switch(type,
           integer = "numeric",
           type)
  }else{
    switch(type,
           integer = "\u8fde\u7eed\u53d8\u91cf",
           numeric = "\u8fde\u7eed\u53d8\u91cf",
           characte = "\u5b57\u7b26\u53d8\u91cf",
           factor = "\u5206\u7c7b\u53d8\u91cf",
           logical = "\u903b\u8f91\u53d8\u91cf",
           type)
  }
}


#' Transcoding
#'
#' @param data an object of 'codes'
#'
#' @return a data frame.
#' @export
codes_t <- function(data){
  if(ncol(data) == 3L){
    data <- tidy_codes(data)
    data <- split.data.frame(data, data$.varname)

    res <- lapply(data, \(d){
      if(nrow(d) == 1L){
        d <- d[c(1, 4)]
        names(d) <- c("Variable", "Label")
        d$Type <- ""
        d$Code <- ""
        d
      }else{
        data.frame(Variable = d[1, 2],
                   Label = d[1, 4],
                   Type = "factor",
                   Code = paste( sprintf("%d: %s", d[[3]][-1], d[[4]][-1]), collapse = "; "))
      }
    })
    res <- do.call(rbind, res)
  }else{

    res <- lapply(1:nrow(data), \(i){
      if(data[i, 4] != ""){
        r <- regex_split(data[i, 4], pattern = "[;|,]\\s*")[[1]]
        r <- regex_split(r, pattern = "[:|=]\\s*")
        r <- do.call(rbind, r)
        r <- as.data.frame(r)
        names(r) <- c("Code", "Label")
        r$Variable <- NA
        r <- relocate(r, "Variable")
        rbind(data.frame(Variable = data[i, 1], Code = "", Label = data[i, 2]), r)
      }else{
        data.frame(Variable = data[i, 1], Code = "", Label = data[i, 2])
      }
    })
    res <- do.call(rbind, res)
  }
  res <- add_title(res, "Data coding")
  class(res) <- c("codes", "data.frame")
  res
}


#' Print object of 'codes'
#'
#' @param x a object of 'codes'.
#' @param ... further arguments.
#'
#' @return No return value.
#' @keywords internal
#' @export
print.codes <- function(x, ...){
  print_booktabs(x, ...)
}


#' Get / Set a variable label
#'
#' @param x a vector or a data frame.
#' @param value a character string or `NULL` to remove the label
#'  For data frames, it could also be a named list or a character vector
#'  of same length as the number of columns in `x`.
#' @param unlist for data frames, return a named vector instead of a list.
#' @param null.action for data frames, by default `NULL` will be returned for
#'  columns with no variable label. Use `"fill"` to populate with the column name
#'  instead, or `"skip"` to remove such values from the returned list.
#' @param ... further arguments.
#'
#' @details
#'   For data frames, if `value` is a named list, only elements whose name will
#'   match a column of the data frame will be taken into account. If `value`
#'   is a character vector, labels should in the same order as the columns of
#'   the data.frame.
#'
#' @export
#'
#' @examples
#' var_label(iris$Sepal.Length)
#' var_label(iris$Sepal.Length) <- 'Length of the sepal'
#' var_label(iris$Sepal.Length)
#'
#' # To remove a variable label
#' var_label(iris$Sepal.Length) <- NULL
#' var_label(iris$Sepal.Length)
#'
#' # To change several variable labels at once
#' var_label(iris) <- c(
#'   "sepal length", "sepal width", "petal length",
#'   "petal width", "species"
#' )
#' var_label(iris)
#'
#' var_label(iris) <- list(
#'   Petal.Width = "width of the petal",
#'   Petal.Length = "length of the petal",
#'   Sepal.Width = NULL,
#'   Sepal.Length = NULL
#' )
#' var_label(iris)
#' var_label(iris, null_action = "fill")
#' var_label(iris, null_action = "skip")
#' var_label(iris, unlist = TRUE)
var_label <- function(x, ...) {
  UseMethod("var_label")
}


#' @export
var_label.default <- function(x, ...) {
  attr(x, "label", exact = TRUE)
}


#' @rdname var_label
#' @export
var_label.data.frame <- function(x,
                                 unlist = FALSE,
                                 null.action = c("keep", "fill", "skip"), ...) {
  r <- lapply(x, var_label)

  null.action <- match.arg(null.action)

  if (null.action == "fill") {
    r <- mapply(
      function(l, n) {
        if (is.null(l)) n else l
      },
      r,
      names(r),
      SIMPLIFY = FALSE
    )
  }

  if (null.action == "skip") {
    r <- r[!sapply(r, is.null)]
  }

  if (unlist) {
    r <- lapply(
      r,
      function(x) {
        if (is.null(x)) "" else x
      }
    )
    r <- base::unlist(r, use.names = TRUE)
  }

  r
}


#' @rdname var_label
#' @export
`var_label<-` <- function(x, value) {
  UseMethod("var_label<-")
}


#' @export
`var_label<-.default` <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) || length(value) > 1)
    stop("`value` should be a single character string or NULL",
         call. = FALSE, domain = "R-labelled")
  attr(x, "label") <- value
  x
}


#' @export
`var_label<-.data.frame` <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) && !is.list(value) ||
      (is.character(value) && length(value) > 1 && length(value) != ncol(x)))
    stop(
      paste0(
        "`value` should be a named list, NULL, a single character string or a ",
        "character vector of same length than the number of columns in `x`"
      ),
      call. = FALSE, domain = "R-labelled")
  if (is.character(value) && length(value) == 1) {
    value <- as.list(rep(value, ncol(x)))
    names(value) <- names(x)
  }
  if (is.character(value) && length(value) == ncol(x)) {
    value <- as.list(value)
    names(value) <- names(x)
  }
  if (is.null(value)) {
    value <- as.list(rep(1, ncol(x)))
    names(value) <- names(x)
    value <- lapply(value, function(x) {
      x <- NULL
    })
  }

  if (!all(names(value) %in% names(x))) {
    missing_names <- paste0(
      setdiff(names(value), names(x)),
      collapse = ", "
    )
    stop("some variables not found in x:", missing_names)
  }

  value <- value[names(value) %in% names(x)]
  for (var in names(value)) var_label(x[[var]]) <- value[[var]]
  x
}


#' Set variable labels to a data frame
#'
#' @param data a data frame.
#' @param ... name-value pairs of variable labels.
#'
#' @return Return an updated copy of data.
#' @export
#'
#' @seealso [get_var_label()] gets varaible label from a data frame.
#'
#' @examples
#' data("mtcars")
#'
#' mtcars.copy <-  mtcars |>
#'   set_var_label(am = "Transmission")  |>
#'   set_var_label(vs = "Engine") |>
#'   set_var_label(hp = "Gross horsepower", cyl = "Number of cylinders")
#' str(mtcars)
#' str(mtcars.copy)
#'
#' mtcars.copy <- set_var_label(mtcars.copy, am = NULL)
#' str(mtcars.copy)
set_var_label <- function(data, ...){
  ldata <- list_flatten(list(...))
  for(i in seq_along(ldata)){
    name <- names(ldata)[i]
    name <- select_variable(data, name)
    var_label(data[[name]]) <- ldata[[i]]
  }
  data
}


#' Get variable labels from a data frame
#'
#' @param data a data frame.
#' @param ... variale names.
#' @param default the returm value if can not find label, default == ".name" will
#'  return the variable name.
#' @param units Whether to display the units.
#' @param unlist if TRUE (default), return a named vector instead of a list.
#'
#' @return a named vector when unlist == TRUE, othewise a named list.
#' @export
#'
#' @seealso [set_var_label()] sets varaible labels to a data frame
#'
#' @examples
#' data("mtcars")
#'
#' mtcars.copy <-  mtcars |>
#'   set_var_label(am = "Transmission")  |>
#'   set_var_label(vs = "Engine") |>
#'   set_var_label(hp = "Gross horsepower", cyl = "Number of cylinders")
#'
#' get_var_label(mtcars.copy, "am")
#' get_var_label(mtcars.copy, "am", "vs", "wt")
#' get_var_label(mtcars.copy, "am", "vs", "wt", default = ".name")
#' get_var_label(mtcars.copy, "am", "vs", "wt", unlist = FALSE)
get_var_label <- function(data, ..., default = NULL, units = TRUE, unlist = TRUE){
  names <- select_variable(data, ...)

  out <-  lapply(names, \(nm){
      if(is.null(var_label(data[[nm]]))){
        if(!is.null(default)){
          if(is.na(default)){
            NA
          }else{
            if(default == ".name"){
              nm
            }else{
              default
            }
          }
        }else{
          if(unlist){
            ""
          }
        }
      }else{
        lb <- var_label(data[[nm]])
        if(!units){
          if(regex_detect(lb, pattern = ",|\\s+\\(")){
            lb <- regex_split(lb, pattern = ",|\\s+\\(")[[1]][1]
          }
        }
        lb
      }
    })

  if(unlist){
    unlist(out)
  }else{
    out
  }
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

