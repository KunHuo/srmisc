#' Dummy codes for a factor
#'
#' Generate a matrix of dummy codes (class indicators) for a given factor.
#'
#' @param x
#' factor or vector of classes for cases.
#' @param method
#' efines the method of the contrasts being formed. Can be one out of "treatment",
#' "sum", "helmert", "poly", "full", whereas "treatment" is the default one.
#' Abbreviations are accepted. The option "full" returns a full set of class
#' indicators, say a dummy factor for each level of x. Note that this would be
#' redundant for lm() and friends.
#' @param base
#' an integer specifying which group is considered the baseline group.
#' @param levels
#' an optional vector of the values (as character strings) that x might have taken.
#' The default is the unique set of values taken by as.character(x), sorted into
#' increasing order of x. This is directly passed on to factor. Not used when the
#' x is a data frame.
#' @param ... further arguments, no used here.
#'
#' @return
#' a matrix with the dummy codes. The number of rows correspond to the number of
#' elements in x and the number of columns to the number of its levels - 1,
#' respectively to the number of levels given as argument -1. When method = "full"
#' is chosen the number of columns will correspond to the number of levels.
#'
#' @references
#' Venables, W N and Ripley, B D (2002): Modern Applied Statistics with S. Fourth edition. Springer.
#' @seealso [model.frame], [contrasts]
#' @export
#'
#' @examples
#' x <- c("red","blue","green","blue","green","red","red","blue")
#' dummy(x)
#' dummy(x, base=2)
#'
#' dummy(x, method="sum")
#'
#'
#' y <- c("Max","Max","Max","Max","Max","Bill","Bill","Bill")
#'
#' dummy(y)
#' dummy(y, base="Max")
#'
#' dummy(y, base="Max", method="full")
#'
#'
#' # "Undummy" (revert the dummy coding)
#' m <- dummy(y, method="full")
#' m
#' z <- apply(m, 1, function(x) colnames(m)[x==1])
#' z
#' identical(y, as.vector(z))
#'
#' m <- dummy(y)
#' m
#' z <- apply(m, 1, function(x) ifelse(sum(x)==0, attr(m,"base"), colnames(m)[x==1]))
#' z
dummy <- function(x,
                  method = c("treatment", "sum", "helmert", "poly", "full"),
                  base = 1,
                  levels = NULL,
                  ...){
  UseMethod("dummy")
}


#' @rdname dummy
#' @export
dummy.default <- function (x,
                           method = c("treatment", "sum", "helmert", "poly", "full"),
                           base = 1,
                           levels = NULL,
                           ...) {

  if (is.null(levels))
    x <- factor(x)
  else
    x <- factor(x, levels = levels)
  if (!is.numeric(base))
    base <- match(base, levels(x))
  method <- match.arg(arg = method,
                      choices = c("treatment", "sum", "helmert", "poly", "full"))
  switch(
    method,
    treatment = {
      res <- stats::contr.treatment(n = nlevels(x), base = base)[x,, drop = FALSE]
    },
    sum = {
      res <- stats::contr.sum(n = nlevels(x))[x, , drop = FALSE]
    },
    helmert = {
      res <- stats::contr.helmert(n = nlevels(x))[x, , drop = FALSE]
    },
    poly = {
      res <- stats::contr.poly(n = nlevels(x))[x, , drop = FALSE]
    },
    full = {
      res <- diag(nlevels(x))[x, , drop = FALSE]
    }
  )
  res <- as.matrix(res)
  if (method == "full") {
    dimnames(res) <-
      list(if (is.null(names(x)))
        1L:length(x)
        else
          names(x),
        levels(x))
    attr(res, "base") <- NA
  }
  else {
    dimnames(res) <-
      list(if (is.null(names(x)))
        1L:length(x)
        else
          names(x),
        levels(x)[-base])
    attr(res, "base") <- levels(x)[base]
  }
  return(res)
}


#' @rdname dummy
#' @param varnames
#' Specify the name of the variable that needs to be processed
#' with dummy variables, if not specified, all factor variables and character
#' variables will be processed. Only used when the x is a data frame.
#' @param keep
#' Whether to keep the original column, the default is FALSE. Only used when the
#' x is a data frame.
#' @param sep a character string to separate the terms.
#'
#' @export
#'
#' @examples
#' dummy(iris)
#' dummy(iris, keep = TRUE)
#'
#' dummy(mtcars, varnames = c("vs", "am"))
#' dummy(mtcars, varnames = c("vs", "am"), sep = ".")
dummy.data.frame <- function(x,
                             method = c("treatment", "sum", "helmert", "poly", "full"),
                             base = 1,
                             levels = NULL,
                             varnames = NULL,
                             keep = FALSE,
                             sep = "_",
                             ...) {

  method <- match.arg(method)

  if(is.null(varnames)){
    varnames <- names(x)[sapply(x, function(i) is.factor(i) | is.character(i))]
  }else{
    check_name(x, varnames)
  }

  if(length(varnames) == 0L){
    return(x)
  }

  for(i in seq_along(varnames)){
    res <- dummy(x[[varnames[i]]], method = method, base = base)
    res <- as.data.frame(res)
    names(res) <- paste(varnames[i], names(res), sep = sep)
    x <- append2(x, res, after = varnames[i])
    if(!keep){
      x[which(names(x) == varnames[i])] <- NULL
    }
  }
  x
}
