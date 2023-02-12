#' Typeset a(n) multinom object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
typeset.multinom <- function(x,
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
                        exp = TRUE,
                        term = FALSE,
                        ...){

  data <- helpers_extract_data(fit = x, data = data)

  varnames <- helpers_extract_varnames(fit = x, data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$terms)[1]
  }else{
    event <- outcome
  }

  estimate.name  <- "B"
  effect.name    <- "OR"
  statistic.name <- "Wald"


  output <- lapply(1:nrow(stats::coef(x)), function(i){

    term0 <- x$coefnames
    estimate <- stats::coef(x)[i, ]
    effect <- estimate
    std.error <- summary(x)$standard.errors[i, ]

    conf.low  <- estimate -
      stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error
    conf.high <- estimate +
      stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error

    statistic <- (estimate / std.error) ^ 2

    p.value <- 1 - stats::pchisq(statistic, df = 1)

    if(exp) {
      effect <- exp(effect)
      conf.low <- exp(conf.low)
      conf.high <- exp(conf.high)
    }

    res <- data.frame(term = term0,
               estimate = estimate,
               std.error = std.error,
               effect = effect,
               conf.low = conf.low,
               conf.high = conf.high,
               statistic = statistic,
               p.value = p.value)

    row.names(res) <- NULL

    coefs <- helpers_fmt_coefs(res,
                               conf.brackets = conf.brackets,
                               conf.separator = conf.separator,
                               digits.pvalue = digits.pvalue,
                               digits.effect = digits.effect)

    out <- helpers_fmt_reg(data = data,
                           varnames = varnames,
                           fold = fold,
                           coefs = coefs)

    out <- merge_left(out, coefs, by = "term")

    if(is.null(select)){
      select <- c("estimate", "std.error", "effect", "p")
    }

    out <- helpers_subset_stat(out, select)
    out <- helpers_set_reference(out,
                                 value = ref.value,
                                 digits.effect = digits.effect)
    out <- helpers_rename_output(out,
                                 estimate = "B",
                                 effect = "OR",
                                 statistic = "Wald",
                                 conf.level = conf.level)

    if(!is.null(filter)){
      out <- out[out$varname %in% filter, ]
    }

    out <- helpers_delete_terms(out, term)

    out
  })

  names(output) <- row.names(stats::coef(x))

  output <- list_rbind(output, collapse.names = TRUE)
  class(output) <- c("typeset", "data.frame")
  output

}
