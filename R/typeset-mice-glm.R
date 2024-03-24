#' Typeset a(n) miceglm object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
#'
#' @seealso [stats::glm()]
typeset.miceglm <- function(x,
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

  if(x[[1]]$family$family == "gaussian"){
    typeset_gaussian_mice (
      x = x,
      data = data,
      outcome = outcome,
      varnames = varnames,
      conf.level = conf.level,
      conf.brackets = conf.brackets,
      conf.separator = conf.separator,
      digits.pvalue = digits.pvalue,
      digits.effect = digits.effect,
      ref.value = ref.value,
      select = select,
      filter = filter,
      fold = fold,
      term = term,
      ...)
  }else if(x[[1]]$family$family == "binomial" | x[[1]]$family$family == "quasibinomial"){
    typeset_logit_mice(
      x = x,
      data = data,
      outcome = outcome,
      varnames = varnames,
      conf.level = conf.level,
      conf.brackets = conf.brackets,
      conf.separator = conf.separator,
      digits.pvalue = digits.pvalue,
      digits.effect = digits.effect,
      ref.value = ref.value,
      select = select,
      filter = filter,
      fold = fold,
      term = term,
      ...)
  }else if(x[[1]]$family$family == "poisson"){
    typeset_poisson_mice(
      x = x,
      data = data,
      outcome = outcome,
      varnames = varnames,
      conf.level = conf.level,
      conf.brackets = conf.brackets,
      conf.separator = conf.separator,
      digits.pvalue = digits.pvalue,
      digits.effect = digits.effect,
      ref.value = ref.value,
      select = select,
      filter = filter,
      fold = fold,
      term = term,
      ...)
  }else{
    stop("This famly is not supported of 'glm'", call. = FALSE)
  }
}


typeset_gaussian_mice <- function(x,
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
                             term = FALSE,
                             ...) {

  data <- helpers_extract_data(fit = x[[1]], data = data)

  varnames <- helpers_extract_varnames(fit = x[[1]], data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x[[1]]$formula)[1]
  }else{
    event <- outcome
  }

  coefs <- tidy_mice(x, conf.int = TRUE, conf.level = conf.level, exp = FALSE, ...)

  # format coefficients
  coefs <- helpers_fmt_coefs(coefs,
                             conf.brackets = conf.brackets,
                             conf.separator = " to ",
                             digits.pvalue = digits.pvalue,
                             digits.effect = digits.effect)

  out <- helpers_fmt_reg(data = data,
                         varnames = varnames,
                         fold = fold,
                         coefs = coefs)
  if(!fold){
    desc <- helpers_describe_event(data = data, event = event, varnames = varnames)
    select <- helper_default_describe(data, event, select)
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
                               statistic = "t",
                               conf.level = conf.level)

  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- helpers_delete_terms(out, term)
  class(out) <- c("typeset", "data.frame")

  out
}


typeset_logit_mice <- function(x,
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
                          term = FALSE,
                          ...) {

  data <- helpers_extract_data(fit = x[[1]], data = data)
  varnames <- helpers_extract_varnames(fit = x[[1]], data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x[[1]]$formula)[1]
  }else{
    event <- outcome
  }

  coefs <- tidy_mice(x, conf.int = TRUE, conf.level = conf.level, exp = TRUE, ...)

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

  if(!fold) {
    desc <- helpers_describe_event(data = data,
                                   event = event,
                                   varnames = varnames)
    select <- helper_default_describe(data, event, select)
    out <- merge_left(out, desc, by = "term")
  }

  out <- merge_left(out, coefs, by = "term")
  out <- helpers_subset_stat(out, select)
  out <- helpers_set_reference(out,
                               value = ref.value,
                               digits.effect = digits.effect)

  effect.name <- switch(x[[1]]$family$link,
                        logit = "OR",
                        log = "RR",
                        identity = "RD",
                        "OR")

  out <- helpers_rename_output(out,
                               estimate = "B",
                               effect = effect.name,
                               statistic = "Wlad",
                               conf.level = conf.level)
  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- helpers_delete_terms(out, term)

  #attr(out, "title") <- "Multivariable logistic regression"
  class(out) <- c("typeset", "data.frame")

  out
}


typeset_poisson_mice <- function(x,
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
                            term = FALSE,
                            ...) {

  data <- helpers_extract_data(fit = x[[1]], data = data)
  varnames <- helpers_extract_varnames(fit = x[[1]], data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x[[1]]$formula)[1]
  }else{
    event <- outcome
  }


  pool_coefs <- function(term, B, se) {
    estimate  <- mean(B)
    std.error <- sqrt(mean(se ^ 2) + stats::var(B))

    effect <- exp(estimate)
    conf.low  <- exp(estimate - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error)
    conf.high <- exp(estimate +stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error)

    statistic <- (estimate / std.error) ^ 2
    p.value <- 1 - stats::pchisq(statistic, df = 1)

    data.frame(term = term,
               estimate = estimate,
               std.error = std.error,
               statistic = statistic,
               p.value = p.value,
               effect = effect,
               conf.low = conf.low,
               conf.high = conf.high)
  }


  coefs <- lapply(x, \(fit){
    coef <- lmtest::coeftest(fit, vcov = sandwich::sandwich)
    coef <- unclass(coef)
    coef <- as.data.frame(coef)
    rownames_to_column(coef)
  })

  terms <- coefs[[1]]$term

  coefs <- lapply(terms, \(term){
    estimate <- sapply(coefs, \(m){
      m[[2]][m$term == term]
    })

    std.error <- sapply(coefs, \(m){
      m[[3]][m$term == term]
    })

    pool_coefs(term, estimate, std.error)
  })

  coefs <- do.call(rbind, coefs)


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

  if(!fold) {
    desc <- helpers_describe_event(data = data,
                                   event = event,
                                   varnames = varnames)
    select <- helper_default_describe(data, event, select)
    out <- merge_left(out, desc, by = "term")
  }

  out <- merge_left(out, coefs, by = "term")
  out <- helpers_subset_stat(out, select)
  out <- helpers_set_reference(out,
                               value = ref.value,
                               digits.effect = digits.effect)


  out <- helpers_rename_output(out,
                               estimate = "B",
                               effect =  "RR",
                               statistic = "Wlad",
                               conf.level = conf.level)
  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- helpers_delete_terms(out, term)

  #attr(out, "title") <- "Multivariable logistic regression"
  class(out) <- c("typeset", "data.frame")

  out
}
