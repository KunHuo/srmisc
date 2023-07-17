#' Typeset a(n) crr object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
typeset.crr <- function(x,
                        data = NULL,
                        outcome = NULL,
                        varnames = NULL,
                        conf.level = 0.95,
                        conf.brackets = NULL,
                        conf.separator = NULL,
                        digits.pvalue = 3,
                        digits.effect = 2,
                        ref.value = "Reference",
                        select = c("net", "effect", "pvalue"),
                        filter = NULL,
                        fold = FALSE,
                        exp = FALSE,
                        term = FALSE,
                        ...){

  if(is.null(data)){
    data <- x$data
  }
  if(is.null(varnames)){
    varnames <- c(x$exposure, x$covariates)
  }

  if(is.null(outcome)){
    event <- x$outcome
  }else{
    event <- outcome
  }

  # tidy coefficients
  coefs <- tidy_crr(x, conf.level = conf.level, exp = TRUE, ...)

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

    tmpdat <- data

    tmpdat[[event]] <- ifelse(tmpdat[[event]] == x$failcode, 1, 0)

    desc <- helpers_describe_event(data = tmpdat,
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
                               estimate = "B",
                               effect = "HR",
                               statistic = "Wald",
                               conf.level = conf.level)

  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- helpers_delete_terms(out, term)
  class(out) <- c("typeset", "data.frame")

  out
}


tidy_crr <- function(x, conf.int = TRUE, conf.level = 0.95, exp = TRUE, ...){

  x <- summary(x)

  estimate <- x$coef[, 1]
  term <- row.names(x$coef)

  std.error <- x$coef[, 3]
  statistic <- (estimate / std.error) ^ 2

  effect <- estimate
  conf.low <- estimate - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error
  conf.high <- estimate + stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error

  if(exp) {
    effect <- exp(effect)
    conf.low <- exp(conf.low)
    conf.high <- exp(conf.high)
  }
  p.value <- 1 - stats::pchisq(statistic, df = 1)

  out <- data.frame(
    term,
    estimate,
    std.error,
    statistic,
    effect,
    conf.low,
    conf.high,
    p.value,
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  out

}

