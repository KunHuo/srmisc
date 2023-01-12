#' Discrete numeric to factor
#'
#' @param x a numeric vector or a data frame which is to be converted to a
#' factor by cutting.
#' @param breaks a numeric vector of  cut points.
#' @param labels labels for the levels of the resulting category.
#' @param sample a logical, indicating whether to display the sample size on
#' the label.
#' @param lower a logical, indicating if the intervals should be closed on the
#' left.
#' @param varname a variable name, specify the variable to be converted, it is
#' executed only when x is a data frame.
#' @param new.col.name the name for a new column, If it is NULL, the specified
#' variable will be overwritten.
#'
#' @return A factor or data frame is returned.
#' @export
#'
#' @examples
#' discrete(1:20, breaks = 10)
#' discrete(1:20, breaks = 10, sample = TRUE, lower = TRUE)
#' discrete(1:20, breaks = c(4, 12))
#' discrete(1:20, breaks = c(4, 12, 13, 16))
#'
#' discrete(iris, varname = "Sepal.Length", breaks = c(5, 7))
#'
#' discrete(iris,
#'          varname = "Sepal.Length",
#'          new.col.name = "g_Sepal.Length",
#'          breaks = c(5, 7))
discrete <- function(x, breaks, labels = NULL, sample = FALSE, lower = TRUE, varname = NULL, new.col.name = NULL){
  UseMethod("discrete")
}


#' @rdname discrete
#' @export
discrete.numeric <- function(x, breaks, labels = NULL, sample = FALSE, lower = TRUE, varname = NULL, new.col.name = NULL){
  if(length(breaks) == 1L){
    discrete2group(x, breaks, labels = labels, lower = lower, sample = sample)
  }else if(length(breaks) == 2L){
    discrete3group(x, breaks, labels = labels, sample = sample)
  }else if(length(breaks) == 4L){
    discrete4group(x, breaks, labels = labels, sample = sample)
  }
}


#' @rdname discrete
#' @export
discrete.data.frame <- function(x, breaks, labels = NULL, sample = FALSE, lower = TRUE, varname = NULL, new.col.name = NULL){
  label <- attr(x[[varname]], "label")
  res <- discrete.numeric(x = x[[varname]], breaks = breaks, labels = labels, lower = lower, sample = sample)
  if(is.null(new.col.name)){
    x[[varname]] <- res
    attr(x[[varname]], "label") <- label
  }else{
    x <- append2.data.frame(x, res, after = varname, names = "NAME_")
    names(x)[which(names(x) == "NAME_")] <- new.col.name
    attr(x[[new.col.name]], "label") <- label
  }
  x
}


discrete2group <- function(x, breaks, labels = NULL, lower = TRUE, sample = FALSE){
  out <- vector(length = length(x))
  if(lower){
    level1 <- paste0("\u2264", breaks)
    level2 <- paste0(">", breaks)
    out[x <= breaks] <- level1
    out[x > breaks] <- level2
  }else{
    level1 <- paste0("<", breaks)
    level2 <-  paste0("\u2265", breaks)
    out[x < breaks] <- level1
    out[x>= breaks] <- level2
  }

  levels <-  c(level1, level2)
  if(is.null(labels)){
    labels <- levels
  }
  if(sample){
    labels <- sprintf("%s (n=%d)", labels, sapply(levels, function(x) {sum(out == x)}))
  }
  factor(out, levels = levels, labels = labels)
}


discrete3group <- function(x, breaks, labels = NULL, sample = FALSE){
  out <- vector(length = length(x))
  level1 <- paste0("<", breaks[1])
  level2 <- paste0(breaks[1], "-", breaks[2])
  level3 <-  paste0(">", breaks[2])
  out[x < breaks[1]] <- level1
  out[x>=breaks[1] & x<=breaks[2]] <- level2
  out[x > breaks[2]] <- level3

  levels <-  c(level1, level2, level3)
  if(is.null(labels)){
    labels <- levels
  }
  if(sample){
    labels <- sprintf("%s (n=%d)", labels, sapply(levels, function(x) {sum(out == x)}))
  }
  factor(out, levels = levels, labels = labels)
}


discrete4group <- function(x, breaks, labels = NULL, sample = FALSE){
  out <- vector(length = length(x))
  level1 <- paste0("<", breaks[1])
  level2 <- paste0(breaks[1], "-", breaks[2])
  level3 <- paste0(breaks[3], "-", breaks[4])
  level4 <- paste0(">", breaks[4])
  out[x < breaks[1]] <- level1
  out[x>=breaks[1] & x<=breaks[2]] <- level2
  out[x>=breaks[3] & x<=breaks[4]] <- level3
  out[x > breaks[4]] <- level4

  levels <-  c(level1, level2, level3, level4)
  if(is.null(labels)){
    labels <- levels
  }
  if(sample){
    labels <- sprintf("%s (n=%d)", labels, sapply(levels, function(x) {sum(out == x)}))
  }
  factor(out, levels = levels, labels = labels)
}
