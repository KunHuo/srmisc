#' Typeset a(n) object
#'
#' @param x
#' a(n) object.
#' @param data
#' a data frame of fitted models.
#' @param outcome
#' outcome variable name.
#' @param varnames
#' variable names.
#' @param conf.level
#' The confidence level to use for the confidence interval if conf.int = TRUE.
#' Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#' corresponds to a 95 percent confidence interval.
#' @param conf.brackets
#' brackets of CI format.
#' @param conf.separator
#' separate of CI format.
#' @param digits.pvalue
#' digits for P value, default 3.
#' @param digits.effect
#' digits for effect value (e.g., OR, HR, or RR), default 2.
#' @param ref.value
#' reference value.
#' @param select
#' statistic.
#' @param filter
#' filter.
#' @param fold
#' fold variables, default FALSE.
#' @param exp
#' exp effect, only used on typeset.default.
#' @param term
#' wether the columns of term, varname, and ref, default FALSE.
#' @param ...
#' Additional arguments.
#'
#' @return a data frame with class 'typeset'.
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
