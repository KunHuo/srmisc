#' Student's t-Test
#'
#' Performs one and two sample t-tests on vectors of data.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param ... further arguments to be passed to or from methods. For the formula
#' method, this includes arguments of the default method, but not paired.
#'
#' @export
t_test2 <- function(x, ...){
  UseMethod("t_test2")
}


#' @rdname t_test2
#'
#' @param y an optional (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less". You can specify
#' just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in
#' means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal. If TRUE then the pooled variance is used to estimate
#' the variance otherwise the Welch (or Satterthwaite) approximation to the
#' degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#'
#' @export
t_test2.default <-
  function(x,
           y = NULL,
           alternative = c("two.sided", "less", "greater"),
           mu = 0,
           paired = FALSE,
           var.equal = FALSE,
           conf.level = 0.95,
           ...)
  {
    alternative <- match.arg(alternative)

    if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
      stop("'mu' must be a single number")
    if (!missing(conf.level) &&
        (length(conf.level) != 1 || !is.finite(conf.level) ||
         conf.level < 0 || conf.level > 1))
      stop("'conf.level' must be a single number between 0 and 1")
    if (!is.null(y)) {
      dname <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
      if (paired)
        xok <- yok <- stats::complete.cases(x, y)
      else {
        yok <- !is.na(y)
        xok <- !is.na(x)
      }
      y <- y[yok]
    }
    else {
      dname <- deparse1(substitute(x))
      if (paired)
        stop("'y' is missing for paired test")
      xok <- !is.na(x)
      yok <- NULL
    }
    x <- x[xok]
    if (paired) {
      x <- x - y
      y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- stats::var(x)
    if (is.null(y)) {
      if (nx < 2)
        stop("not enough 'x' observations")
      df <- nx - 1
      stderr <- sqrt(vx / nx)
      if (stderr < 10 * .Machine$double.eps * abs(mx))
        stop("data are essentially constant")
      tstat <- (mx - mu) / stderr
      method <- if (paired)
        "Paired t-test"
      else
        "One Sample t-test"
      estimate <-
        stats::setNames(mx, if (paired)
          "mean difference"
          else
            "mean of x")
    } else {
      ny <- length(y)
      if (nx < 1 || (!var.equal && nx < 2))
        stop("not enough 'x' observations")
      if (ny < 1 || (!var.equal && ny < 2))
        stop("not enough 'y' observations")
      if (var.equal && nx + ny < 3)
        stop("not enough observations")
      my <- mean(y)
      vy <- stats::var(y)
      method <- paste(if (!var.equal)
        "Welch", "Two Sample t-test")
      estimate <- c(mx, my)
      names(estimate) <- c("mean of x", "mean of y")
      if (var.equal) {
        df <- nx + ny - 2
        v <- 0
        if (nx > 1)
          v <- v + (nx - 1) * vx
        if (ny > 1)
          v <- v + (ny - 1) * vy
        v <- v / df
        stderr <- sqrt(v * (1 / nx + 1 / ny))
      } else {
        stderrx <- sqrt(vx / nx)
        stderry <- sqrt(vy / ny)
        stderr <- sqrt(stderrx ^ 2 + stderry ^ 2)
        df <- stderr ^ 4 / (stderrx ^ 4 / (nx - 1) + stderry ^ 4 / (ny - 1))
      }
      if (stderr < 10 * .Machine$double.eps * max(abs(mx), abs(my)))
        stop("data are essentially constant")
      tstat <- (mx - my - mu) / stderr
    }
    if (alternative == "less") {
      pval <- stats::pt(tstat, df)
      cint <- c(-Inf, tstat + stats::qt(conf.level, df))
    }
    else if (alternative == "greater") {
      pval <- stats::pt(tstat, df, lower.tail = FALSE)
      cint <- c(tstat - stats::qt(conf.level, df), Inf)
    }
    else {
      pval <- 2 * stats::pt(-abs(tstat), df)
      alpha <- 1 - conf.level
      cint <- stats::qt(1 - alpha / 2, df)
      cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if (paired)
      "mean difference"
    else if (!is.null(y))
      "difference in means"
    else
      "mean"
    attr(cint, "conf.level") <- conf.level
    rval <- list(
      statistic = tstat,
      parameter = df,
      p.value = pval,
      conf.int = cint,
      estimate = estimate,
      null.value = mu,
      stderr = stderr,
      alternative = alternative,
      method = method,
      data.name = dname
    )
    class(rval) <- "htest"
    rval
  }

#' @rdname t_test2
#' @title Student's t-Test
#'
#' @param formula a formula of the form lhs ~ rhs where lhs is a numeric variable
#' giving the data values and rhs either 1 for a one-sample or paired test or a
#' factor with two levels giving the corresponding groups. If lhs is of class
#' "Pair" and rhs is 1, a paired test is done,
#' @param data an optional matrix or data frame (or similar: see model.frame)
#' containing the variables in the formula formula. By default the variables
#' are taken from environment(formula).
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs.
#'
#' @return A list with class "htest".
#' @export
t_test2.formula <-
  function (formula, data, subset, na.action, ...)
  {
    if (missing(formula) || (length(formula) != 3L))
      stop("'formula' missing or incorrect")
    oneSampleOrPaired <- FALSE
    if (length(attr(stats::terms(formula[-2L]), "term.labels")) != 1L)
      if (formula[[3L]] == 1L)
        oneSampleOrPaired <- TRUE
    else
      stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ") # works in all cases
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    if (!oneSampleOrPaired) {
      g <- factor(mf[[-response]])
      if (nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
      DATA <- split(mf[[response]], g)
      ## Call the default method.
      y <- t_test2(x = DATA[[1L]], y = DATA[[2L]], ...)
      if (length(y$estimate) == 2L) {
        names(y$estimate) <- paste("mean in group", levels(g))
        names(y$null.value) <-
          paste("difference in means between",
                paste("group", levels(g), collapse = " and "))
      }
    }
    else {
      # 1-sample and paired tests
      respVar <- mf[[response]]
      if (inherits(respVar, "Pair")) {
        ## Call the default method.
        y <- t_test2(x = respVar[, 1L],
                     y = respVar[, 2L],
                     paired = TRUE,
                     ...)
      }
      else {
        ## Call the default method.
        y <- t_test2(x = respVar, ...)
      }
    }
    y$data.name <- DNAME
    y
  }
