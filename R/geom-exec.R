#' Execute ggplot2 functions
#' @description A helper function used by ggpubr functions to execute any geom_*
#'   functions in ggplot2. Useful only when you want to call a geom_* function
#'   without carrying about the arguments to put in aes(). Basic users of ggpubr
#'   don't need this function.
#' @param geomfunc a ggplot2 function (e.g.: geom_point)
#' @param data a data frame to be used for mapping
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... arguments accepted by the function
#' @return return a plot if geomfunc!=Null or a list(option, mapping) if
#'   geomfunc = NULL.
#' @examples
#' \dontrun{
#' ggplot() + geom_exec(geom_point, data = mtcars,
#'     x = "mpg", y = "wt", size = "cyl", color = "cyl")
#' }
#' @export
geom_exec <- function (geomfunc = NULL,
                       data = NULL,
                       position = NULL,
                       ...) {
  params <- list(...)

  mapping <-
    list() # option to pass to mapping aes()
  option <- list() # option to the geom_*()

  allowed_options <- c(
    # general
    "x", "y", "color", "colour", "linetype", "fill", "size", "shape", "width",
    "alpha", "na.rm", "lwd", "pch", "cex", "position", "stat", "geom",
    "show.legend", "inherit.aes", "fun.args", "fontface", "linewidth",
    # point
    "stroke",
    # boxplot
    "outlier.colour", "outlier.shape", "outlier.size",
    "outlier.stroke", "notch", "notchwidth", "varwidth",
    # dot plot
    "binwidth", "binaxis", "method", "binpositions",
    "stackdir", "stackratio", "dotsize",
    # Violin
    "trim", "draw_quantiles", "scale",
    # error
    "ymin", "ymax", "xmin", "xmax",
    # text
    "label", "hjust", "vjust", "fontface", "angle", "family", "parse",
    # text.repel
    "segment.size", "force", "max.overlaps", "seed",
    # smooth
    "se", "level", "fullrange",
    "conf.int.level",
    # straightline
    "xintercept", "yintercept",
    # histograms
    "bins", "weight",
    # rug
    "sides",
    # segment
    "arrow", "xend", "yend",
    # stat_summary,
    "fun.data", "fun.y", "fun.ymin", "fun.ymax",
    # bracket
    "y.position", "tip.length", "label.size", "step.increase",
    "bracket.nudge.y", "bracket.shorten", "coord.flip"


  )

  columns <- colnames(data)
  for (key in names(params)) {
    value <- params[[key]]
    if (is.null(value)) {

    }
    else if (unlist(value)[1] %in% columns & key %in% allowed_options) {
      mapping[[key]] <- value

    }
    else if (key %in% allowed_options) {
      option[[key]] <- value
    }
    else if (key =="group") {
      mapping[[key]] <- value # for line plot
    }
    else if(key == "step.group.by"){
      # for geom_bracket, value are variable name.
      # but this parameter is an option not an aes
      option[[key]] <- value
    }
    # else warnings("Don't know '", key, "'")
  }
  if (!is.null(position))
    option[["position"]] <- position
  option[["data"]] <- data
  if(is.null(geomfunc)){
    res <- list(option = option, mapping = mapping)
  }
  else{
    option[["mapping"]] <- create_aes(mapping)
    res <- do.call(geomfunc, option)
  }
  res
}

create_aes <- function(.list, parse = TRUE){
  if(missing(parse)){
    parse <- base::getOption("ggpubr.parse_aes", default = TRUE)
  }
  if(parse){
    return(create_aes.parse(.list))
  } else{
    return(create_aes.name(.list))
  }
}


# Ref:  https://rpubs.com/hadley/97970
# Parse as name. If x_var is "a + b", as.name() will turn it into a variable called `a + b`.
create_aes.name <- function(.list){
  .list <- lapply(.list, function(x) to_name(x))
  do.call(ggplot2::aes, .list)
}

# Parse an expression. If x_var is "a + b", parse() will turn it into the function call a + b
create_aes.parse <- function(.list){
  .list <-  lapply(.list, function(x) parse_expression(x))
  do.call(ggplot2::aes, .list)
}

parse_expression <- function(x){
  if(is_parsable_aes(x)){
    # if contains space, just add backsticks using as.name()
    if(contains_space(x)){
      if(!is_math_string(x)) return(as.name(x))
    }
    x <- parse(text = x)[[1]]
  }
  x
}

to_name <- function(x){
  if(is_parsable_aes(x)){
    x <- as.name(x)
  }
  x
}

# Check if parsable aesthetic
is_parsable_aes <- function(x){
  is.character(x) & (!is_numeric_char(x)) & (length(x) == 1)
}

# Check if x is a numeric string "1", "2"
# return TRUE for "1", "2", etc
is_numeric_char <- function(x){
  if(is.character(x)) res <- grepl("^[[:digit:]]+$", x)
  else res <- FALSE
  res
}



# Fortify variable name----------------------------
fortify_variable_name <- function(x){
  if(contains_space(x)){
    if(!is_math_string(x)){

    }
  }
}

# Check if string contains space
contains_space <- function(x){
  grepl("\\s", x)
}

extract_text_inside_parenthesis <- function(x){
  regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=TRUE))[[1]]
}

# Check if text contains mathematical operators
is_math_string <- function(x){
  # operators <- unlist(lapply( c("Arith","Compare","Math"), methods::getGroupMembers ))
  operators <- c("+", "-", "*", "^", "%%", "%/%", "/", "==", ">", "<", "!=", "<=", ">=")
  contains_math_operators <- unlist(lapply(operators, grepl, x, fixed = TRUE))
  contains_parentheses <- grepl(pattern = "\\(.*\\)", x)
  any(c(contains_math_operators, contains_parentheses))
}

