#' @export
typeset.default <- function(x,
                            data = NULL,
                            outcome = NULL,
                            varnames = NULL,
                            conf.level = 0.95,
                            conf.brackets = NULL,
                            conf.separator = NULL,
                            digits.pvalue = 3,
                            digits.effect = 2,
                            ref.value = "Reference",
                            select = c("n", "effect", "p.value"),
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

  if("coeftest" %in% class(x)){
    x <- unclass(x)
  }

  x <- as.data.frame(x)

  if(!("term" %in% names(x))){
    term.col <- row.names(x)
    x <- cbind(data.frame(term = term.col), x)
    row.names(x) <- NULL
  }

  names(x) <- c("term", "estimate", "std.error", "statistic", "p.value")

  x$effect    <- x$estimate
  x$conf.low  <- x$estimate -
    stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * x$std.error
  x$conf.high <- x$estimate +
    stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * x$std.error

  if(exp) {
    x$effect <- exp(x$effect)
    x$conf.low <- exp(x$conf.low)
    x$conf.high <- exp(x$conf.high)
  }

  # format coefficients
  coefs <- helpers_fmt_coefs(x,
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
