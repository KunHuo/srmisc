#' Typeset a(n) lme object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
typeset.lme <- function(x,
                        data = NULL,
                        outcome = NULL,
                        varnames = NULL,
                        conf.level = 0.95,
                        conf.brackets = NULL,
                        conf.separator = NULL,
                        digits.pvalue = 3,
                        digits.effect = 2,
                        ref.value = "Reference",
                        select = c("effect", "p.value"),
                        filter = NULL,
                        fold = FALSE,
                        exp = FALSE,
                        term = FALSE,
                        ...){

  if(is.null(select)){
    select <- c("effect", "pvalue")
  }

  data <- helpers_extract_data(fit = x, data = data)

  varnames <- helpers_extract_varnames(fit = x,
                                       data = data,
                                       varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$sformula)[1]
  }else{
    event <- outcome
  }


  # tidy coefficients
  coefs <- tidy_lme(x, conf.level = conf.level, exp = FALSE, ...)

  # set the separator of the confidence interval.
  if(is.null(conf.separator)){
    conf.separator <- " to "
  }

  # format coefficients
  coefs <- helpers_fmt_coefs(coefs,
                             conf.brackets = conf.brackets,
                             conf.separator = conf.separator,
                             digits.pvalue = digits.pvalue,
                             digits.effect = digits.effect)

  out <- helpers_fmt_reg(data = data,
                         varnames = varnames,
                         fold = fold,
                         coefs = coefs)

  if(!fold){
    desc <- helpers_describe_event(data = data,
                                   event = event,
                                   varnames = varnames)
    out <- merge_left(out, desc, by = "term")
  }

  out <- merge_left(out, coefs, by = "term")
  out <- helpers_subset_stat(out, select)
  out <- helpers_set_reference(out,
                               value = ref.value,
                               digits.effect = digits.effect)
  out <- helpers_rename_output(out,
                               estimate = "\u03b2",
                               effect = "\u03b2",
                               statistic = "t value",
                               conf.level = conf.level)
  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- helpers_delete_terms(out, term)
  class(out) <- c("typeset", "data.frame")

  out
}
#
#
tidy_lme <- function(x, conf.int = TRUE, conf.level = 0.95, exp = TRUE, ...){

  coef <- summary(x)$tTable
  coef <- as.data.frame(coef)
  coef <- rownames_to_column(coef)

  conf.low  <- coef[[2]] - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * coef[[3]]
  conf.high <- coef[[2]] + stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * coef[[3]]


  out <- data.frame(
    term = coef[[1]],
    estimate = coef[[2]],
    std.error = coef[[3]],
    statistic = coef[[5]],
    effect = coef[[2]],
    conf.low = conf.low,
    conf.high = conf.high,
    p.value = coef[[6]],
    stringsAsFactors = FALSE
  )

  out
}
