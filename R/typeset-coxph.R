#' Typeset a(n) coxph object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
typeset.coxph <- function(x,
                          data = NULL,
                          outcome = NULL,
                          varnames = NULL,
                          conf.level = 0.95,
                          conf.brackets = NULL,
                          conf.separator = NULL,
                          digits.pvalue = 3,
                          digits.effect = 2,
                          ref.value = "Reference",
                          select = c("net", "effect", "p.value"),
                          filter = NULL,
                          fold = FALSE,
                          exp = FALSE,
                          term = FALSE,
                          ...){

  data <- helpers_extract_data(fit = x, data = data)

  varnames <- helpers_extract_varnames(fit = x, data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$formula)[2]
  }else{
    event <- outcome
  }

  # tidy coefficients
  coefs <- tidy_coxph(x, conf.level = conf.level, exp = TRUE, ...)

  # format coefficients
  coefs <- helpers_fmt_coefs(coefs,
                             conf.brackets = conf.brackets,
                             conf.separator = conf.separator,
                             digits.pvalue = digits.pvalue,
                             digits.effect = digits.effect)

  out <- helpers_fmt_reg(data = data,
                         varnames = varnames,
                         fold = fold, coefs = coefs)

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



tidy_coxph <- function(x, conf.int = TRUE, conf.level = 0.95, exp = TRUE, ...) {

  s <- summary(x)
  co <- as.data.frame(stats::coef(s))

  if (!is.null(x$frail)) {
    nn <- c("estimate", "std.error", "statistic", "p.value")
  } else if (isTRUE(s$used.robust)) {
    nn <- c("estimate", "std.error", "robust.se", "statistic", "p.value")
  } else {
    nn <- c("estimate", "std.error", "statistic", "p.value")
  }

  if (is.null(x$frail) && is.null(x$penalty)) {
    ret <- co[, -2, drop = FALSE]
    names(ret) <- nn
  } else {
    ret <- co[, -c(3, 5), drop = FALSE]
    names(ret) <- nn
  }

  ret <- cbind(data.frame(term = row.names(ret)), ret)
  row.names(ret) <- NULL

  if (conf.int) {
    ci <- confint_terms(x, level = conf.level)
    ret <- merge_left(ret, ci, by = "term")
  }

  ret$statistic <- ret$statistic ^ 2
  ret$effect <- ret$estimate

  if (exp) {
    ret <- exponentiate(ret)
  }

  ret
}
