#' Typeset a(n) object
#'
#' @param x The object to be typeset.
#' @param data An optional data frame containing the data.
#' @param outcome An optional character vector specifying the outcome variable.
#' @param varnames An optional character vector specifying the variable names to
#' be used.
#' @param conf.level The confidence level for calculating confidence intervals.
#' @param conf.brackets An optional character vector specifying the format of
#' confidence interval brackets.
#' @param conf.separator An optional character vector specifying the separator
#' between lower and upper bounds of confidence intervals.
#' @param digits.pvalue The number of digits to display for p-values.
#' @param digits.effect The number of digits to display for effect estimates.
#' @param ref.value The reference value for categorical variables.
#' @param select An optional character vector specifying the variables to select
#' from the data frame.
#' @param filter An optional expression indicating rows to filter from the data
#' frame.
#' @param fold A logical value indicating whether to fold long lines.
#' @param exp A logical value indicating whether to exponentiate the coefficients.
#' @param term A logical value indicating whether to display interaction terms.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return a data frame with class 'typeset'.
#'
#' @export
typeset <- function(x,
                    data = NULL,
                    outcome = NULL,
                    varnames = NULL,
                    conf.level = 0.95,
                    conf.brackets = NULL,
                    conf.separator = NULL,
                    digits.pvalue = 3,
                    digits.effect = 2,
                    ref.value = "Reference",
                    select = NULL,
                    filter = NULL,
                    fold = FALSE,
                    exp = FALSE,
                    term = FALSE,
                    ...){
  UseMethod("typeset")
}


#' Print 'typeset' class.
#'
#' @param x a class of 'typeset'.
#' @param ... more arguments.
#'
#' @keywords internal
#' @return No return value.
#'
#' @export
print.typeset <- function(x, ...){

  if("P value" %in% names(x)){
    x[["P value"]] <- fmt_signif(x[["P value"]])
  }

  STAT.NAME <- c("t value", "Std. error", "Wald", "B", "\u03b2")
  if(any(STAT.NAME %in% names(x))){
    STAT.NAME <- STAT.NAME[STAT.NAME %in% names(x)]

    x[STAT.NAME] <- lapply(x[STAT.NAME], function(i){
      ifelse(is.na(i), NA, format(i, justify = "right"))
    })
  }

  print_booktabs(x, adj = c("left", "center"))
}
