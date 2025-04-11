#' Wilcoxon Rank Sum and Signed Rank Tests
#'
#' Performs one- and two-sample Wilcoxon tests on vectors of data; the latter
#' is also known as 'Mann-Whitney' test.
#'
#' @param x numeric vector of data values. Non-finite (e.g., infinite or missing)
#' values will be omitted.
#' @param ... further arguments to be passed to or from methods. For the formula
#' method, this includes arguments of the default method, but not paired.
#'
#' @return A list with class "htest"
#' @export
wilcox_test2 <- function(x, ...) {
  UseMethod("wilcox_test2")
}


#' @rdname wilcox_test2
#'
#' @param y an optional numeric vector of data values: as with x non-finite
#' values will be omitted.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less". You can specify
#' just the initial letter.
#' @param mu a number specifying an optional parameter used to form the null
#' hypothesis.
#' @param paired a logical indicating whether you want a paired test.
#' @param exact a logical indicating whether an exact p-value should be computed.
#' @param correct a logical indicating whether to apply continuity correction in
#' the normal approximation for the p-value.
#' @param conf.int a logical indicating whether a confidence interval should be
#' computed.
#' @param conf.level confidence level of the interval.
#' @param tol.root (when conf.int is true:) a positive numeric tolerance, used
#' in uniroot(*, tol=tol.root) calls.
#' @param digits.rank a number; if finite, rank(signif(r, digits.rank)) will be
#' used to compute ranks for the test statistic instead of (the default) rank(r).
#'
#' @export
wilcox_test2.default <-
  function(x,
           y = NULL,
           alternative = c("two.sided", "less", "greater"),
           mu = 0,
           paired = FALSE,
           exact = NULL,
           correct = TRUE,
           conf.int = FALSE,
           conf.level = 0.95,
           tol.root = 1e-4,
           digits.rank = Inf,
           ...)
  {
    alternative <- match.arg(alternative)
    if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
      stop("'mu' must be a single number")
    if (conf.int) {
      if (!((length(conf.level) == 1L)
            && is.finite(conf.level)
            && (conf.level > 0)
            && (conf.level < 1)))
        stop("'conf.level' must be a single number between 0 and 1")
    }

    if (!is.numeric(x))
      stop("'x' must be numeric")
    if (!is.null(y)) {
      if (!is.numeric(y))
        stop("'y' must be numeric")
      DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
      if (paired) {
        if (length(x) != length(y))
          stop("'x' and 'y' must have the same length")
        OK <- stats::complete.cases(x, y)
        x <- x[OK] - y[OK]
        y <- NULL
      }
      else {
        y <- y[!is.na(y)]
      }
    } else {
      DNAME <- deparse1(substitute(x))
      if (paired)
        stop("'y' is missing for paired test")
    }
    x <- x[!is.na(x)]

    if (length(x) < 1L)
      stop("not enough (non-missing) 'x' observations")
    CORRECTION <- 0
    if (is.null(y)) {
      METHOD <- "Wilcoxon signed rank test"
      x <- x - mu
      ZEROES <- any(x == 0)
      if (ZEROES)
        x <- x[x != 0]
      n <- as.double(length(x))
      if (is.null(exact))
        exact <- (n < 50)
      r <- rank(abs(if (is.finite(digits.rank))
        signif(x, digits.rank)
        else
          x))
      STATISTIC <- stats::setNames(sum(r[x > 0]), "V")
      TIES <- length(r) != length(unique(r))

      if (exact && !TIES && !ZEROES) {
        METHOD <- sub("test", "exact test", METHOD, fixed = TRUE)
        PVAL <-
          switch(
            alternative,
            "two.sided" = {
              p <- if (STATISTIC > (n * (n + 1) / 4))
                stats::psignrank(STATISTIC - 1, n, lower.tail = FALSE)
              else
                stats::psignrank(STATISTIC, n)
              min(2 * p, 1)
            },
            "greater" = stats::psignrank(STATISTIC - 1, n, lower.tail = FALSE),
            "less" = stats::psignrank(STATISTIC, n)
          )
        if (conf.int) {
          ## Exact confidence interval for the median in the
          ## one-sample case.  When used with paired values this
          ## gives a confidence interval for mean(x) - mean(y).
          x <- x + mu             # we want a conf.int for the median
          alpha <- 1 - conf.level
          diffs <- outer(x, x, `+`)
          diffs <- sort(diffs[!lower.tri(diffs)]) / 2
          cint <-
            switch(
              alternative,
              "two.sided" = {
                qu <- stats::qsignrank(alpha / 2, n)
                if (qu == 0)
                  qu <- 1
                ql <- n * (n + 1) / 2 - qu
                achieved.alpha <- 2 * stats::psignrank(trunc(qu) -
                                                  1, n)
                c(diffs[qu], diffs[ql + 1])
              },
              "greater" = {
                qu <- stats::qsignrank(alpha, n)
                if (qu == 0)
                  qu <- 1
                achieved.alpha <- stats::psignrank(trunc(qu) - 1, n)
                c(diffs[qu], +Inf)
              },
              "less" = {
                qu <- stats::qsignrank(alpha, n)
                if (qu == 0)
                  qu <- 1
                ql <- n * (n + 1) / 2 - qu
                achieved.alpha <- stats::psignrank(trunc(qu) - 1, n)
                c(-Inf, diffs[ql + 1])
              }
            )
          if (achieved.alpha - alpha > alpha / 2) {
            warning("requested conf.level not achievable")
            conf.level <- 1 - signif(achieved.alpha, 2)
          }
          attr(cint, "conf.level") <- conf.level
          ESTIMATE <- c("(pseudo)median" = stats::median(diffs))
        }
      } else {
        ## not exact, maybe ties or zeroes
        NTIES <- table(r)
        z <- STATISTIC - n * (n + 1) / 4
        SIGMA <- sqrt(n * (n + 1) * (2 * n + 1) / 24
                      - sum(NTIES ^ 3 - NTIES) / 48)
        if (correct) {
          CORRECTION <-
            switch(
              alternative,
              "two.sided" = sign(z) * 0.5,
              "greater" = 0.5,
              "less" = -0.5
            )
          METHOD <- paste(METHOD, "with continuity correction")
        }
        z <- (z - CORRECTION) / SIGMA
        PVAL <- switch(
          alternative,
          "less" = stats::pnorm(z),
          "greater" = stats::pnorm(z, lower.tail = FALSE),
          "two.sided" = 2 * min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
        )
        if (conf.int) {
          ## Asymptotic confidence interval for the median in the
          ## one-sample case.  When used with paired values this
          ## gives a confidence interval for mean(x) - mean(y).
          ## Algorithm not published, thus better documented here.
          x <- x + mu
          alpha <- 1 - conf.level
          if (n > 0) {
            ## These are sample based limits for the median
            ## [They don't work if alpha is too high]
            mumin <- min(x)
            mumax <- max(x)
            ## wdiff(d, zq) returns the absolute difference between
            ## the asymptotic Wilcoxon statistic of x - mu - d and
            ## the quantile zq.
            W <- function(d) {
              ## also fn(x, correct, alternative)
              xd <- x - d
              xd <- xd[xd != 0]
              nx <- length(xd)
              dr <- rank(abs(if (is.finite(digits.rank))
                signif(xd, digits.rank)
                else
                  xd))
              zd <- sum(dr[xd > 0]) - nx * (nx + 1) / 4
              NTIES.CI <- table(dr)
              SIGMA.CI <- sqrt(nx * (nx + 1) * (2 * nx + 1) / 24
                               - sum(NTIES.CI ^ 3 - NTIES.CI) / 48)
              if (SIGMA.CI == 0)
                warning(
                  "cannot compute confidence interval when all observations are zero or tied",
                  call. = FALSE
                )
              CORRECTION.CI <-
                if (correct) {
                  switch(
                    alternative,
                    "two.sided" = sign(zd) * 0.5,
                    "greater" = 0.5,
                    "less" = -0.5
                  )
                } else
                  0
              (zd - CORRECTION.CI) / SIGMA.CI
            }
            Wmumin <- W(mumin)
            Wmumax <- if (!is.finite(Wmumin))
              NA
            else
              W(mumax) # if(): warn only once
          }
          if (n == 0 ||
              !is.finite(Wmumax)) {
            # incl. "all zero / ties" warning above
            cint <- structure(c(if (alternative == "less")
              - Inf
              else
                NaN, if (alternative == "greater")
                  + Inf
              else
                NaN), conf.level = 0)
            ESTIMATE <- if (n > 0)
              c(midrange = (mumin + mumax) / 2)
            else
              NaN
          } else {
            # (Wmumin, Wmumax) are finite
            wdiff <- function(d, zq)
              W(d) - zq
            ## Here we optimize the function wdiff in d over the set
            ## c(mumin, mumax).
            ## This returns a value from c(mumin, mumax) for which
            ## the asymptotic Wilcoxon statistic is equal to the
            ## quantile zq.  This means that the statistic is not
            ## within the critical region, and that implies that d
            ## is a confidence limit for the median.
            ##
            ## As in the exact case, interchange quantiles.
            root <- function(zq) {
              stats::uniroot(
                wdiff,
                lower = mumin,
                upper = mumax,
                f.lower = Wmumin - zq,
                f.upper = Wmumax - zq,
                tol = tol.root,
                zq = zq
              )$root
            }

            cint <- switch(
              alternative,
              "two.sided" = {
                repeat {
                  ## FIXME: no need to loop for finding boundary alpha !!
                  mindiff <- Wmumin - stats::qnorm(alpha / 2, lower.tail = FALSE)
                  maxdiff <- Wmumax - stats::qnorm(alpha / 2)
                  if (mindiff < 0 || maxdiff > 0)
                    alpha <- alpha * 2
                  else
                    break
                }
                if (alpha >= 1 || 1 - conf.level < alpha * 0.75) {
                  conf.level <- 1 - pmin(1, alpha)
                  warning("requested conf.level not achievable")
                }
                if (alpha < 1) {
                  l <- root(zq = stats::qnorm(alpha / 2, lower.tail = FALSE))
                  u <- root(zq = stats::qnorm(alpha / 2))
                  c(l, u)
                } else {
                  ## alpha >= 1
                  rep(stats::median(x), 2)
                }
              },
              "greater" = {
                repeat {
                  ## FIXME: no need to loop for finding boundary alpha !!
                  mindiff <- Wmumin - stats::qnorm(alpha, lower.tail = FALSE)
                  if (mindiff < 0)
                    alpha <- alpha * 2
                  else
                    break
                }
                if (alpha >= 1 || 1 - conf.level < alpha * 0.75) {
                  conf.level <- 1 - pmin(1, alpha)
                  warning("requested conf.level not achievable")
                }
                l <- if (alpha < 1)
                  root(zq = stats::qnorm(alpha, lower.tail = FALSE))
                else
                  ## alpha >= 1
                  stats::median(x)
                c(l, +Inf)

              },
              "less" = {
                repeat {
                  ## FIXME: no need to loop for finding boundary alpha !!
                  maxdiff <- Wmumax - stats::qnorm(alpha / 2)
                  if (maxdiff > 0)
                    alpha <- alpha * 2
                  else
                    break
                }
                if (alpha >= 1 || 1 - conf.level < alpha * 0.75) {
                  conf.level <- 1 - pmin(1, alpha)
                  warning("requested conf.level not achievable")
                }
                u <- if (alpha < 1)
                  root(zq = stats::qnorm(alpha))
                else
                  stats::median(x)
                c(-Inf, u)
              }
            )
            attr(cint, "conf.level") <- conf.level
            correct <- FALSE # for W(): no continuity correction for estimate
            ESTIMATE <- c("(pseudo)median" =
                            stats::uniroot(
                              W,
                              lower = mumin,
                              upper = mumax,
                              tol = tol.root
                            )$root)
          } # regular (Wmumin, Wmumax)
        } # end{conf.int}
        if (exact && TIES) {
          warning("cannot compute exact p-value with ties")
          if (conf.int)
            warning("cannot compute exact confidence interval with ties")
        }
        if (exact && ZEROES) {
          warning("cannot compute exact p-value with zeroes")
          if (conf.int)
            warning("cannot compute exact confidence interval with zeroes")
        }
      }
    }
    else {
      ##-------------------------- 2-sample case ---------------------------
      if (length(y) < 1L)
        stop("not enough 'y' observations")
      METHOD <- "Wilcoxon rank sum test"
      r <- c(x - mu, y)
      r <- rank(if (is.finite(digits.rank))
        signif(r, digits.rank)
        else
          r)
      n.x <- as.double(length(x))
      n.y <- as.double(length(y))
      if (is.null(exact))
        exact <- (n.x < 50) && (n.y < 50)
      STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
      TIES <- (length(r) != length(unique(r)))
      if (exact && !TIES) {
        METHOD <- sub("test", "exact test", METHOD, fixed = TRUE)
        PVAL <-
          switch(
            alternative,
            "two.sided" = {
              p <- if (STATISTIC > (n.x * n.y / 2))
                stats::pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
              else
                stats::pwilcox(STATISTIC, n.x, n.y)
              min(2 * p, 1)
            },
            "greater" = {
              stats::pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
            },
            "less" = stats::pwilcox(STATISTIC, n.x, n.y)
          )
        if (conf.int) {
          ## Exact confidence interval for the location parameter
          ## mean(x) - mean(y) in the two-sample case (cf. the
          ## one-sample case).
          alpha <- 1 - conf.level
          diffs <- sort(outer(x, y, `-`))
          cint <-
            switch(
              alternative,
              "two.sided" = {
                qu <- stats::qwilcox(alpha / 2, n.x, n.y)
                if (qu == 0)
                  qu <- 1
                ql <- n.x * n.y - qu
                achieved.alpha <- 2 * stats::pwilcox(trunc(qu) -
                                                1, n.x, n.y)
                c(diffs[qu], diffs[ql + 1])
              },
              "greater" = {
                qu <- stats::qwilcox(alpha, n.x, n.y)
                if (qu == 0)
                  qu <- 1
                achieved.alpha <- stats::pwilcox(trunc(qu) - 1, n.x, n.y)
                c(diffs[qu], +Inf)
              },
              "less" = {
                qu <- stats::qwilcox(alpha, n.x, n.y)
                if (qu == 0)
                  qu <- 1
                ql <- n.x * n.y - qu
                achieved.alpha <- stats::pwilcox(trunc(qu) - 1, n.x, n.y)
                c(-Inf, diffs[ql + 1])
              }
            )
          if (achieved.alpha - alpha > alpha / 2) {
            warning("Requested conf.level not achievable")
            conf.level <- 1 - achieved.alpha
          }
          attr(cint, "conf.level") <- conf.level
          ESTIMATE <- c("difference in location" = stats::median(diffs))
        }
      }
      else {
        ## not exact, maybe ties or zeroes
        NTIES <- table(r)
        z <- STATISTIC - n.x * n.y / 2
        SIGMA <- sqrt((n.x * n.y / 12) *
                        ((n.x + n.y + 1)
                         - sum(NTIES ^ 3 - NTIES)
                         / ((n.x + n.y) * (n.x + n.y - 1))))
        if (correct) {
          CORRECTION <- switch(
            alternative,
            "two.sided" = sign(z) * 0.5,
            "greater" = 0.5,
            "less" = -0.5
          )
          METHOD <- paste(METHOD, "with continuity correction")
        }
        z <- (z - CORRECTION) / SIGMA
        PVAL <- switch(
          alternative,
          "less" = stats::pnorm(z),
          "greater" = stats::pnorm(z, lower.tail = FALSE),
          "two.sided" = 2 * min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
        )
        if (conf.int) {
          ## Asymptotic confidence interval for the location
          ## parameter mean(x) - mean(y) in the two-sample case
          ## (cf. one-sample case).
          ##
          ## Algorithm not published, for a documentation see the
          ## one-sample case.
          alpha <- 1 - conf.level
          mumin <- min(x) - max(y)
          mumax <- max(x) - min(y)
          W <- function(d) {
            ## also fn (x, y, n.x, n.y, correct, alternative)
            dr <- c(x - d, y)
            dr <- rank(if (is.finite(digits.rank))
              signif(dr, digits.rank)
              else
                dr)
            NTIES.CI <- table(dr)
            dz <- sum(dr[seq_along(x)]) - n.x * (n.x + 1) / 2 - n.x * n.y / 2
            CORRECTION.CI <-
              if (correct) {
                switch(
                  alternative,
                  "two.sided" = sign(dz) * 0.5,
                  "greater" = 0.5,
                  "less" = -0.5
                )
              } else
                0
            SIGMA.CI <- sqrt((n.x * n.y / 12) *
                               ((n.x + n.y + 1)
                                - sum(NTIES.CI ^ 3 - NTIES.CI)
                                / ((n.x + n.y) * (
                                  n.x + n.y - 1
                                ))))
            if (SIGMA.CI == 0)
              warning("cannot compute confidence interval when all observations are tied",
                      call. = FALSE)
            (dz - CORRECTION.CI) / SIGMA.CI
          }
          wdiff <- function(d, zq)
            W(d) - zq
          Wmumin <- W(mumin)
          Wmumax <- W(mumax)
          root <- function(zq) {
            ## in extreme cases we need to return endpoints,
            ## e.g.  wilcox.test(1, 2:60, conf.int=TRUE)
            f.lower <- Wmumin - zq
            if (f.lower <= 0)
              return(mumin)
            f.upper <- Wmumax - zq
            if (f.upper >= 0)
              return(mumax)
            stats::uniroot(
              wdiff,
              lower = mumin,
              upper = mumax,
              f.lower = f.lower,
              f.upper = f.upper,
              tol = tol.root,
              zq = zq
            )$root
          }
          cint <- switch(
            alternative,
            "two.sided" = {
              l <- root(zq = stats::qnorm(alpha / 2, lower.tail = FALSE))
              u <- root(zq = stats::qnorm(alpha / 2))
              c(l, u)
            },
            "greater" = {
              l <- root(zq = stats::qnorm(alpha, lower.tail = FALSE))
              c(l, +Inf)
            },
            "less" = {
              u <- root(zq = stats::qnorm(alpha))
              c(-Inf, u)
            }
          )
          attr(cint, "conf.level") <- conf.level
          correct <- FALSE # for W(): no continuity correction for estimate
          ESTIMATE <- c("difference in location" =
                          stats::uniroot(
                            W,
                            lower = mumin,
                            upper = mumax,
                            tol = tol.root
                          )$root)
        } ## {conf.int}

        if (exact && TIES) {
          warning("cannot compute exact p-value with ties")
          if (conf.int)
            warning("cannot compute exact confidence intervals with ties")
        }
      }
    }

    names(mu) <- if (paired ||
                     !is.null(y))
      "location shift"
    else
      "location"
    RVAL <- list(
      statistic = STATISTIC,
      parameter = NULL,
      p.value = as.numeric(PVAL),
      null.value = mu,
      alternative = alternative,
      method = METHOD,
      data.name = DNAME
    )
    if (conf.int)
      RVAL <- c(RVAL, list(conf.int = cint, estimate = ESTIMATE))
    class(RVAL) <- "htest"
    RVAL
  }

#' @rdname wilcox_test2
#'
#' @param formula a formula of the form lhs ~ rhs where lhs is a numeric variable
#' giving the data values and rhs either 1 for a one-sample or paired test or a
#' factor with two levels giving the corresponding groups. If lhs is of class
#' "Pair" and rhs is 1, a paired test is done
#' @param data an optional matrix or data frame (or similar: see model.frame)
#' containing the variables in the formula formula. By default the variables are
#' taken from environment(formula).
#' @param subset an optional vector specifying a subset of observations to be used.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs.
#' @export
wilcox_test2.formula <-
  function(formula, data, subset, na.action, ...)
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
      y <- wilcox_test2(x = DATA[[1L]], y = DATA[[2L]], ...)
    }
    else {
      # 1-sample and paired tests
      respVar <- mf[[response]]
      if (inherits(respVar, "Pair")) {
        ## Call the default method.
        y <- wilcox_test2(x = respVar[, 1L],
                         y = respVar[, 2L],
                         paired = TRUE,
                         ...)
      }
      else {
        ## Call the default method.
        y <- wilcox_test2(x = respVar, ...)
      }
    }
    y$data.name <- DNAME
    y
  }
