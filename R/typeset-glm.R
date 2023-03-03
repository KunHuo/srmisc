#' Typeset a(n) glm object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
#'
#' @seealso [stats::glm()]
#'
#' @examples
#' data("cancer")
#' data("cancer.codes")
#'
#' dat <- codes2labels(data = cancer, codes = cancer.codes)
#'
#' fit <- glm(status ~ age + size + race + size + meta,
#'            data = dat,
#'            family = binomial())
#' typeset(fit)
typeset.glm <- function(x,
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
  if(x$family$family == "gaussian"){
    typeset_gaussian (
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
  }else if(x$family$family == "binomial" | x$family$family == "quasibinomial"){
    typeset_logit(
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
  }else if(x$family$family == "poisson"){
    typeset_poisson(
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


tidy_glm <- function(x, conf.int = TRUE, conf.level = 0.95, exp = TRUE, ...) {

  warn_on_appropriated_glm_class(x)
  warn_on_subclass(x)

  ret <- as.data.frame(summary(x)$coefficients)
  ret <- cbind(data.frame(term = row.names(ret)), ret)
  row.names(ret) <- NULL
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # summary(x)$coefficients misses rank deficient rows (i.e. coefs that
  # summary.lm() sets to NA), catch them here and add them back
  coefs <- as.data.frame(stats::coef(x))
  names(coefs) <- "estimate"
  coefs <- cbind(data.frame(term = row.names(coefs)), coefs)
  row.names(coefs) <- NULL
  ret <- merge_left(coefs, y = ret[-2], by = "term")

  if (conf.int) {
    ci <-  confint_terms(x, level = conf.level)
    ret <- merge_left(ret, ci, by = "term")
  }

  ret$statistic <- ret$statistic ^ 2
  ret$effect <- ret$estimate

  if (exp) {
    ret <- exponentiate(ret)
  }

  ret
}


exponentiate <- function(data) {
  data$effect <- exp(data$effect)

  if ("conf.low" %in% colnames(data)) {
    data$conf.low <- exp(data$conf.low)
    data$conf.high <- exp(data$conf.high)
  }
  data
}


warn_on_appropriated_glm_class <- function(x) {
  warn_on_glm2(x)
  warn_on_stanreg(x)

  invisible(TRUE)
}

# the output of glm2::glm2 has the same class as objects outputted
# by stats::glm2. glm2 outputs are currently not supported (intentionally)
# so warn that output is not maintained.
warn_on_glm2 <- function(x) {
  if (!is.null(x$method)) {
    if (x$method == "glm.fit2") {
      warning("The supplied model object seems to be outputted from the glm2 ",
              "package. Tidiers for glm2 output are currently not ",
              "maintained; please use caution in interpreting broom output.")
    }
  }

  invisible(TRUE)
}

# stanreg objects subclass glm, glm tidiers error out (uninformatively),
# and the maintained stanreg tidiers live in broom.mixed.
warn_on_stanreg <- function(x) {
  if (!is.null(x$stan_function)) {
    stop("The supplied model object seems to be outputted from the rstanarm ",
         "package. Tidiers for mixed model output now live in the broom.mixed package.")
  }

  invisible(TRUE)
}


typeset_gaussian <- function(x,
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

  data <- helpers_extract_data(fit = x, data = data)

  varnames <- helpers_extract_varnames(fit = x, data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$formula)[1]
  }else{
    event <- outcome
  }

  coefs <- tidy_glm(x, conf.level = conf.level, exp = FALSE, ...)

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


typeset_logit <- function(x,
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

  data <- helpers_extract_data(fit = x, data = data)
  varnames <- helpers_extract_varnames(fit = x, data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$formula)[1]
  }else{
    event <- outcome
  }

  coefs <- tidy_glm(x, conf.level = conf.level, exp = TRUE, ...)

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

  effect.name <- switch(x$family$link,
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



typeset_poisson <- function(x,
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

  data <- helpers_extract_data(fit = x, data = data)
  varnames <- helpers_extract_varnames(fit = x, data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$formula)[1]
  }else{
    event <- outcome
  }

  coefs <- lmtest::coeftest(x, vcov = sandwich::sandwich)
  coefs <- unclass(coefs)

  estimate <- coefs[, 1]
  std.error <- coefs[, 2]
  statistic <- coefs[, 3]
  p.value <- coefs[, 4]
  effect <- exp(estimate)
  conf.low <- exp(estimate - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error)
  conf.high <- exp(estimate + stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * std.error)

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
