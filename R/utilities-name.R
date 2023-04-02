#' Set the names in an object
#'
#'
#' This is a convenience function that sets the names of an object and returns
#' it including the new names. It is most useful at the end of a function
#' definition where one is creating the object to be returned and would prefer
#' not to store it under a name just that the names can be assigned. In addition
#' to the function setNames in base R the user can decide, whether rownames,
#' colnames or simply the names are to be set. Names are recyled.
#'
#'
#' @param x
#' an object for which a names attribute will be meaningful
#' @param ...
#' the names to be assigned to the object. This should be a character vector of
#' names named dimnames, rownames, colnames or names. Setting rownames=NULL would
#' remove existing rownames. All kind of names can be changed at the same time.
#' Default would be names. Abbreviations are supported.
#'
#' @return
#' An object of the same sort as object with the new names assigned.
#' @seealso [setNames]
#' @export
#'
#' @examples
#' set_names(1:5, names=letters[1:5])
#'
#' # the default, if no argument names are provided, is "names"
#'
#' set_names(1:5, letters[1:5])
set_names <- function (x, ...) {
  args <- list(...)
  if (is.null(names(args)))
    names(args) <- "names"
  names(args) <- lapply(names(args), match.arg, c("names",
                                                  "rownames", "colnames", "dimnames"))
  if ("dimnames" %in% names(args)) {
    if (is.null(args[["dimnames"]]))
      dimnames(x) <- NULL
    else dimnames(x) <- args[["dimnames"]]
  }
  if ("rownames" %in% names(args)) {
    if (is.null(args[["rownames"]]))
      rownames(x) <- NULL
    else rownames(x) <- rep_len(args[["rownames"]],
                                dim(x)[1])
  }
  if ("colnames" %in% names(args)) {
    if (is.null(args[["colnames"]]))
      colnames(x) <- NULL
    else colnames(x) <- rep_len(args[["colnames"]],
                                dim(x)[2])
  }
  if ("names" %in% names(args)) {
    if (is.null(args[["names"]]))
      names(x) <- NULL
    else names(x) <- rep_len(args[["names"]], length(x))
  }
  x
}


#' Change names of a named object
#'
#' Rename changes the names of a named object.
#'
#' @param x
#' Any named object
#' @param ...
#' A sequence of named arguments, all of type character.
#' @param gsub
#' a logical value; if TRUE, [gsub] is used to change the row and column labels of
#' the resulting table. That is, instead of substituting whole names, substrings
#' of the names of the object can changed.
#' @param fixed
#' a logical value, passed to [gsub]. If TRUE, substitutions are by fixed strings
#' and not by regular expressions.
#' @param warn
#' a logical value; should a warning be issued if those names to change are not found?
#'
#' @details
#' This function changes the names of x according to the remaining arguments.
#' If gsub is FALSE, argument tags are the old names, the values are the new names.
#' If gsub is TRUE, arguments are substrings of the names that are substituted
#' by the argument values.
#'
#' @return
#' The object x with new names defined by the ... arguments.
#' @export
#'
#' @examples
#' x <- c(a=1, b=2)
#' rename(x, a="A", b="B")
#'
#' str(rename( iris,
#'             Sepal.Length="Sepal_Length",
#'             Sepal.Width ="Sepal_Width",
#'             Petal.Length="Petal_Length",
#'             Petal.Width ="Petal_Width"
#' ))
#'
#' str(rename(iris, .="_", gsub=TRUE))
rename <- function (x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE) {
  subst <- c(...)
  if (is.null(names(subst)))
    names(x)[1:length(subst)] <- subst
  if (gsub) {
    names.x <- names(x)
    for (i in 1:length(subst)) {
      names.x <- gsub(names(subst[i]), subst[i], names.x, fixed = fixed)
    }
    names(x) <- names.x
  }
  else {
    i <- match(names(subst), names(x))
    if (any(is.na(i))) {
      if (warn)
        warning("unused name(s) selected")
      if (any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if (length(i))
      names(x)[i] <- subst
  }
  return(x)
}


#' Change names of a data frame
#'
#' @param data a data frame.
#' @param columns which columns will be rename.
#' @param newnames new names.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' rename2(head(iris), 5, "species")
#' rename2(head(iris), "Species, "species")
#' rename2(head(iris), 1:2, c("A", "B"))
rename2 <- function(data, columns, newnames){
  columns <- select_variable(data, columns, type = "index")
  for(i in 1:length(columns)){
    names(data)[columns[i]] <- newnames[i]
  }
  data
}

#' Convert rownames to column
#'
#' @param data 	a data frame.
#' @param varname name of column to use for rownames
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' data("mtcars")
#' mtcars
#' rownames_to_column(mtcars)
rownames_to_column <- function(data, varname = "term"){
  if(!is.data.frame(data)){
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  data <- cbind(data.frame(term = row.names(data)), data)
  row.names(data) <- NULL
  names(data)[1] <- varname
  data
}
