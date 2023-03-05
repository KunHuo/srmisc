#' Typeset a(n) coeftest object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
typeset.coeftest <- function(x,
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


  if(is.null(data)){
    stop("data can not be NULL.", call. = FALSE)
  }

  if(is.null(outcome)){
    stop("outcome can not be NULL.", call. = FALSE)
  }

  if(is.null(varnames)){
    stop("varnames can not be NULL.", call. = FALSE)
  }

  # set the separator of the confidence interval.
  if(is.null(conf.separator)){
    conf.separator <- ", "
  }

  coefs <- unclass(x)


  estimate <- coefs[, 1]
  std.error <- coefs[, 2]
  statistic <- coefs[, 3]
  p.value <- coefs[, 4]
  effect <- estimate
  conf.low <- estimate - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error
  conf.high <- estimate + stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error

  if(exp) {
    effect <- exp(effect)
    conf.low <- exp(conf.low)
    conf.high <- exp(conf.high)
  }

  coefs <- data.frame(term = row.names(coefs),
                      estimate,
                      std.error,
                      statistic,
                      p.value, conf.low, conf.high, effect)


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
                                   event = outcome,
                                   varnames = varnames)
    select <- helper_default_describe(data, outcome, select)
    out <- merge_left(out, desc, by = "term")
  }

  out <- merge_left(out, coefs, by = "term")
  out <- helpers_subset_stat(out, select)
  out <- helpers_set_reference(out, value = ref.value, digits.effect = digits.effect)


  out <- helpers_rename_output(out,
                               estimate = "estimate",
                               effect = "effect",
                               statistic = "statistic",
                               conf.level = conf.level)

  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- helpers_delete_terms(out, term)
  class(out) <- c("typeset", "data.frame")

  out
}
