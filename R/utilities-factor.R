# fct_recode --------------------------------------------------------------

#' Recode a factor
#'
#' Combining or rearranging a factor can be tedious if it has many levels.
#' fct_recode supports this step by accepting a direct definition of new levels
#' by enumerating old levelnames as argument and adding an "elselevel" option.
#' If new levels are given as integer values they will be translated in the
#' according levels.
#'
#'
#' @param x
#' a facotr (or character vector) or a data frame.
#' @param ...
#' the old levels (combined by c() if there are several) named with the new level.
#' See examples.
#' @param elselevel
#' the value for levels, which are not matched by newlevel list. If this is set
#' to NULL, the elselevels will be left unchanged. If set to NA (default) non
#' matched levels will be set to NA.
#' @param use.empty
#' logical. Defines how a new level, which can't be found in x, should be handled.
#' Should it be left in the level's list or be dropped? The default is FALSE,
#' which drops empty levels.
#' @param num
#' logical. If set to TRUE the result will be numeric
#' @param varname variable name contains in the data frame.
#'
#' @seealso [factor], [levels]
#'
#' @return Return a factor when 'x' is a factor or character, otherwise return a
#' data frame when 'x' is a data frame.
#' @export
#'
#' @examples
#' set.seed(1984)
#' x <- factor(sample(1:15, 20, replace=TRUE))
#' levels(x) <- paste("old", levels(x), sep="_")
#'
#' y <- fct_recode(x,
#'             "new_1"   = c("old_1","old_4","old_5"),
#'             "new_2"   = c("old_6","old_10","old_11"),
#'             "new_3"   = c("old_12","old_13"),
#'             elselevel = "other")
#' data.frame(x=x, y=y)
#'
#'
#' x <- factor(letters[1:6])
#'
#' z1 <- fct_recode(x, AB=c("a","b"), CD=c("c","d"), elselevel="none of these")
#' z2 <- fct_recode(x, AB=c("a","b"), CD=c("c","d"), elselevel=NA)
#' z3 <- fct_recode(x, AB=c("a","b"), CD=c("c","d"), elselevel=NULL)
#' z4 <- fct_recode(x, AB=c("a","b"), GH=c("g","h"), elselevel=NA, use.empty=TRUE)
#' z5 <- fct_recode(x, AB=c("a","b"), GH=c("g","h"), elselevel=NA, use.empty=FALSE)
#'
#' data.frame(z1, z2, z3, z4, z5)
#'
#' lapply(data.frame(z1, z2, z3, z4, z5), levels)
#'
#' # empty level GH exists in z4...
#' table(z4, useNA="ifany")
#' # and is dropped in z5
#' table(z5, useNA="ifany")
#'
#' # use integers to define the groups to collapse
#' set.seed(1972)
#' (likert <- factor(sample(1:10, size=15, replace=TRUE),
#'                   levels=1:10, labels=gettextf("(%s)", 1:10)))
#' fct_recode(likert, det=1:6, pas=7:8, pro=9:10)
#'
#' # or directly turned to numeric
#' fct_recode(likert, "1"=1:6, "2"=7:8, "5"=9:10, num=TRUE)
fct_recode <- function (x, ..., elselevel = NULL, use.empty = FALSE, num = FALSE){
  UseMethod("fct_recode")
}


#' @rdname fct_recode
#' @export
fct_recode.factor <- function (x, ..., elselevel = NULL, use.empty = FALSE, num = FALSE) {
  if (xchar <- is.character(x)) {
    x <- factor(x)
  }
  newlevels <- list(...)
  if (sum(duplicated(unlist(newlevels))) > 0)
    stop("newlevels contain non unique values!")
  if (all(is.numeric(unlist(newlevels))))
    newlevels <- lapply(newlevels, function(i) levels(x)[i])
  if (is.null(elselevel)) {
    elselevels <- setdiff(levels(x), unlist(newlevels))
    names(elselevels) <- elselevels
    newlevels <- c(newlevels, elselevels)
  }
  else {
    if (!is.na(elselevel)) {
      newlevels[[length(newlevels) + 1]] <- setdiff(levels(x),
                                                    unlist(newlevels))
      names(newlevels)[[length(newlevels)]] <- elselevel
    }
  }
  levels(x) <- newlevels
  if (!use.empty)
    x <- factor(x)
  if (xchar)
    x <- as.character
  if (num)
    x <- as.numeric(as.character(x))
  return(x)
}


#' @rdname fct_recode
#' @export
fct_recode.character <- function (x, ..., elselevel = NULL, use.empty = FALSE, num = FALSE) {
  x <- factor(x)
  fct_recode(x = x, ..., elselevel = elselevel, use.empty = use.empty, num = num)
}


#' @rdname fct_recode
#' @export
fct_recode.data.frame <- function (x, ..., elselevel = NULL, use.empty = FALSE, num = FALSE, varname = NULL) {
  varname <- select_variable(x, varname)
  x <- x[[varname]]
  fct_recode(x = x, ..., elselevel = elselevel, use.empty = use.empty, num = num)
}


# fct_reverse -------------------------------------------------------------

#' Reverse order of factor levels
#'
#' @param x a facotr (or character vector) or a data frame.
#' @param varname variable name contains in the data frame.
#' @param ... further arguments.
#'
#' @return Return a factor when 'x' is a factor or character, otherwise return a
#' data frame when 'x' is a data frame.
#' @export
#'
#' @examples
#' f <- factor(c("a", "b", "c"))
#' f
#' fct_reverse(f)

#' fd <- data.frame(x = factor(c("a", "b", "c")), y = 1:3)
#' fd$x
#' fd <- fct_reverse(fd, varname = "x")
#' fd$x
fct_reverse <- function(x, ...){
  UseMethod("fct_reverse")
}


#' @rdname fct_reverse
#' @export
fct_reverse.factor <- function(x, ...){
  factor(x, levels = rev(levels(x)))
}


#' @rdname fct_reverse
#' @export
fct_reverse.character <- function(x, ...){
  x <- factor(x)
  factor(x, levels = rev(levels(x)))
}


#' @rdname fct_reverse
#' @export
fct_reverse.data.frame <- function(x, varname = NULL, ...){
  varname <- select_variable(x, varname)
  x[[varname]] <- fct_reverse(x[[varname]])
  x
}


# fct_reorder -------------------------------------------------------------

#' Reorder factor levels
#'
#' @param x a facotr (or character vector) or a data frame.
#' @param ... the levels of factor.
#' @param varname a string of variable name.
#' @param exclude a vector of values to be excluded when forming the set of levels.
#' This may be factor with the same level set as x or should be a character.
#'
#' @return Return a factor when 'x' is a factor or character, otherwise return a
#' data frame when 'x' is a data frame.
#' @export
#'
#' @examples
#' # for character
#' f <- c("a", "b", "c", "d")
#' f
#' fct_reorder(f, "c", "b")
#'
#' # for factor
#' f <- factor(f)
#' f
#' fct_reorder(f, "d", "b")
#'
#' # for data.frame
#' fd <- data.frame(x = factor(c("a", "b", "c", "d")), y = 1:4)
#' fd$x
#' fd <- fct_reorder(fd, varname = "x", "b", "d")
#' fd$x
fct_reorder <- function(x, ...){
  UseMethod("fct_reorder")
}


#' @rdname fct_reorder
#' @export
fct_reorder.factor <- function(x, ..., exclude = NA){
  levels <- levels(x)
  order <- c(...)
  index <- order %in% levels
  if (!all(index)) {
    text <- paste(order[!index], collapse = ", ")
    text <- sprintf("The level of '%s' is not in the data.", text)
    stop(text, call. = FALSE)
  }
  levels <- c(order, setdiff(levels, order))
  x<- factor(x, levels = levels, exclude = exclude)
  x
}


#' @rdname fct_reorder
#' @export
fct_reorder.character <- function(x, ..., exclude = NA){
  x <- factor(x)
  fct_reorder(x, ..., exclude = exclude)
}


#' @rdname fct_reorder
#' @export
fct_reorder.data.frame <- function(x, ..., exclude = NA, varname = NULL) {
  varname <- select_variable(x, varname)
  x[[varname]] <- fct_reorder(x[[varname]], ..., exclude = exclude)
  x
}


# fct_auto ----------------------------------------------------------------

#' Automatic conversion to factor
#'
#' @param data a data frame.
#' @param include include variable names.
#' @param exclude exclude variable names.
#' @param min usually taking a min number of distinct values. Default 2.
#' @param max usually taking a max number of distinct values. Default 5.
#' @param na.include whether to include NA values in the table.
#' @param na.level level to use for missing values.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' data("cancer")
#'
#' str(cancer)
#' str(fct_auto(cancer))
#' str(fct_auto(cancer, max = 2))
#' str(fct_auto(cancer, exclude = "status"))
fct_auto <- function(data, include = NULL, exclude = NULL, min = 2, max = 5,
                     na.include = FALSE, na.level = "Missing"){
  if(is.null(include)){
    variables <- names(data)
  }else{
    variables <- include
  }
  variables <- setdiff(variables, exclude)


  index <- sapply(variables, function(x) {
    if(!is.null(attr(data[[x]], "labels"))){
      TRUE
    }else{
      if (!is.factor(x)) {
        l <- length(unique(data[[x]]))
        ifelse(l >= min & l <= max, TRUE, FALSE)
      }
      else {
        FALSE
      }
    }
  })
  if (!any(index)) {
    return(data)
  }
  variables <- variables[index]
  data[variables] <- lapply(data[variables], function(x) {

    labels <- attr(x, "labels")
    label <- attr(x, "label")

    if(!is.null(labels)){
      levels <- attr(x, "labels")
      x <- factor(x, levels = levels, labels = names(levels))
    }else{
      x <- factor(x)
    }

    if (na.include) {
      x <- fct_explicit_na(x, na_level = na.level)
    }
    attr(x, "label") <- label
    x
  })
  data
}



#' categorize
#'
#' @param data a data frame.
#' @param varnames variable names.
#' @param exclude exclude variable names.
#' @param min usually taking a min number of distinct values only when varnames = ".auto". Default 2.
#' @param max usually taking a max number of distinct values only when varnames = ".auto". Default 5.
#' @param na.include whether to include NA values in the table.
#' @param na.level level to use for missing values.
#' @param ... unused.
#'
#' @return a data frame.
#' @export
categorize <- function(data, varnames = ".auto", exclude = NULL, min = 2, max = 5, na.include = FALSE, na.level = "Missing", ...){

  if(varnames[1] == ".auto"){
    index <- sapply(data, \(x){
      unique_length(x, na.rm = TRUE) >= min & unique_length(x, na.rm = TRUE) <= max
    })
    varnames <- names(data)[index]
  }else{
    varnames <- select_variable(data, varnames)
  }

  exclude <- select_variable(data, exclude)
  varnames <- setdiff(varnames, exclude)

  if(is_empty(varnames)){
    return(data)
  }

  data[varnames] <- lapply(data[varnames], function(x) {

    labels <- attr(x, "labels")
    label <- attr(x, "label")

    if(!is.null(labels)){
      levels <- attr(x, "labels")
      x <- factor(x, levels = levels, labels = names(levels))
    }else{
      x <- factor(x)
    }

    if (na.include) {
      x <- fct_explicit_na(x, na_level = na.level)
    }
    attr(x, "label") <- label
    x
  })

  data
}


#' Make missing values explicit
#'
#' @param f A factor (or character vector).
#' @param na_level Level to use for missing values: this is what NAs will be changed to.
#'
#' @return a factor vector.
#' @export
fct_explicit_na <- function (f, na_level = "Missing") {
  f <- check_factor(f)
  is_missing <- is.na(f)
  is_missing_level <- is.na(levels(f))
  if (any(is_missing)) {
    f <- fct_expand(f, na_level)
    f[is_missing] <- na_level
    f
  }
  else if (any(is_missing_level)) {
    levs <- levels(f)
    levs[is.na(levs)] <- na_level
    lvls_revalue(f, levs)
  }
  else {
    f
  }
}

lvls_revalue <- function (f, new_levels) {
  f <- check_factor(f)
  if (!is.character(new_levels)) {
    stop("`new_levels` must be a character vector",
         call. = FALSE)
  }
  if (length(new_levels) != nlevels(f)) {
    stop("`new_levels` must be the same length as `levels(f)`: expected ",
         nlevels(f), " new levels, got ", length(new_levels),
         ".", call. = FALSE)
  }
  if (anyDuplicated(new_levels)) {
    u_levels <- unique(new_levels)
    index <- match(new_levels, u_levels)
    out <- index[f]
    attributes(out) <- attributes(f)
    attr(out, "levels") <- u_levels
    out
  }
  else {
    attr(f, "levels") <- new_levels
    f
  }
}


fct_expand <- function (f, ...) {
  f <- check_factor(f)
  new_levels <- c(...)
  lvls_expand(f, union(levels(f), new_levels))
}


lvls_expand <- function (f, new_levels) {
  f <- check_factor(f)
  missing <- setdiff(levels(f), new_levels)
  if (length(missing) > 0) {
    stop("Must include all existing levels. Missing: ",
         paste0(missing, collapse = ", "), call. = FALSE)
  }
  refactor(f, new_levels)
}

check_factor <- function(f) {
  if (is.character(f)) {
    factor(f)
  } else if (is.factor(f)) {
    f
  } else {
    stop("`f` must be a factor (or character vector).", call. = FALSE)
  }
}

refactor <- function(f, new_levels, ordered = NA) {
  if (is.na(ordered)) {
    ordered <- is.ordered(f)
  }

  new_f <- factor(f, levels = new_levels, exclude = NULL, ordered = ordered)
  # attributes(new_f) <- utils::modifyList(attributes(f), attributes(new_f))
  new_f
}


# Others ------------------------------------------------------------------

#' Count of factor levels
#'
#' @param data a data frame.
#' @param varname a string of variable name.
#'
#' @return a data frame.
#' @export
fct_count <- function(data, varname){
  table(data[[varname]])
}
