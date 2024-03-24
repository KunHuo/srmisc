#' Typeset a(n) micelm object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
typeset.micelm<- function(x,
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

  data <- helpers_extract_data(fit = x[[1]], data = data)

  varnames <- helpers_extract_varnames(fit = x[[1]],
                                       data = data,
                                       varnames = varnames)


  if(is.null(select)){
    select <- c("n", "effect", "p.value")
  }

  if(is.null(outcome)){
    event <- all.vars(x[[1]]$terms)[1]
  }else{
    event <- outcome
  }

  # set the separator of the confidence interval.
  if(is.null(conf.separator)){
    conf.separator <- " to "
  }

  # tidy coefficients
  coefs <- tidy_mice(x, conf.int = TRUE, exp = FALSE, conf.level = conf.level, ...)

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
    out  <- merge_left(out, desc, by = "term")
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


