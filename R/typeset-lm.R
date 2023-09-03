#' Typeset a(n) lm object
#'
#' @inheritParams typeset
#'
#' @inherit typeset return
#' @export
#'
#' @examples
#' fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
#'
#' # base use
#' typeset(fit)
#'
#' # select stat
#' typeset(fit, select = c("n", "effect", "se", "t", "p"))
#'
#' # filter species
#' typeset(fit,
#'         select = c("n", "effect", "se", "t", "p"),
#'         filter = "Species")
#'
#' # fold variables
#' typeset(fit, fold = TRUE)
#'
#' # other params
#' typeset(fit,
#'         conf.brackets = "[]",
#'         conf.separator = ", ",
#'         digits.effect = 5,
#'         digits.pvalue = 5)
typeset.lm <- function(x,
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

  data <- helpers_extract_data(fit = x, data = data)

  varnames <- helpers_extract_varnames(fit = x,
                                       data = data,
                                       varnames = varnames)


  if(is.null(select)){
    select <- c("n", "effect", "p.value")
  }

  if(is.null(outcome)){
    event <- all.vars(x$terms)[1]
  }else{
    event <- outcome
  }

  # set the separator of the confidence interval.
  if(is.null(conf.separator)){
    conf.separator <- " to "
  }

  # tidy coefficients
  coefs <- tidy_lm(x, conf.level = conf.level, ...)

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


tidy_lm <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  warn_on_subclass(x)

  ret <- as.data.frame(summary(x)$coefficients)
  ret <- cbind(data.frame(term = row.names(ret)), ret)
  row.names(ret) <- NULL
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # summary(x)$coefficients misses rank deficient rows (i.e. coefficients that
  # summary.lm() sets to NA), catch them here and add them back
  coefs <- as.data.frame(stats::coef(x))
  names(coefs) <- "estimate"
  coefs <- cbind(data.frame(term = row.names(coefs)), coefs)
  row.names(coefs) <- NULL
  ret$effect <- ret$estimate
  ret <- merge_left(coefs, y = ret[-2], by = "term")

  if (conf.int) {
    ci <- confint_terms(x, level = conf.level)
    ret <- merge_left(ret, ci, by = "term")
  }
  ret
}


warn_on_subclass <- function(x) {
  if (length(class(x)) > 1 && class(x)[1] != "glm") {
    subclass <- class(x)[1]
    dispatched_method <- class(x)[class(x) %in% c("glm", "lm")][1]

    # warning(
    #   "Tidiers for objects of class ",
    #   subclass,
    #   " are not maintained by the broom team, and are only supported through ",
    #   "the ",
    #   dispatched_method,
    #   " tidier method. Please be cautious in interpreting and reporting ",
    #   "broom output.",
    #   call. = FALSE
    # )
  }
}


confint_terms <- function(x, ...) {
  # warn on arguments silently being ignored
  ci <- suppressMessages(stats::confint.default(x, ...))

  # confint called on models with a single predictor
  # often returns a named vector rather than a matrix :(
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1)
    rownames(ci) <- names(stats::coef(x))[1]
  }
  ci <- as.data.frame(ci)
  ci <- cbind(data.frame(term = row.names(ci)), ci)
  row.names(ci) <- NULL
  names(ci) <- c("term", "conf.low", "conf.high")
  ci
}

