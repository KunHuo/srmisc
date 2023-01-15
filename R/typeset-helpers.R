helpers_extract_data <- function(fit, data = NULL){
  if(is.null(data)){
    data <- fit$data
    if(is.null(data)){
      data <- fit$model
    }
    if(is.null(data)){
      stop("Must specify the data set to fit the model.", call. = FALSE)
    }
  }
  data
}


helpers_extract_varnames <- function(fit, data = NULL, varnames = NULL){
  if(is.null(varnames)){
    varnames <- attr(fit$terms, "term.labels")
    if(is.null(varnames)){
      stop("The variable names of the fitted model must be specified.", call. = FALSE)
    }
  }
  varnames <- varnames[varnames %in% names(data)]
  varnames
}


helpers_fmt_coefs <- function(x,
                         conf.brackets = NULL,
                         conf.separator = NULL,
                         digits.pvalue = 3,
                         digits.effect = 2,
                         ci = TRUE){

  x$p.value <- fmt_pvalue(x$p.value, digits = digits.pvalue)
  x[sapply(x, is.numeric)] <- lapply(x[sapply(x, is.numeric)], function(i){
    fmt_digits(i, digits = digits.effect)
  })
  if(ci){
    ci.format <- helpers_fmt_conf(conf.brackets, conf.separator)
    x$effect <- sprintf(ci.format, x$effect, x$conf.low, x$conf.high)
  }
  x
}


helpers_fmt_conf <- function(conf.brackets = NULL, conf.separate = NULL){
  if(is.null(conf.brackets)){
    conf.brackets <- "()"
  }
  if(is.null(conf.separate)){
    conf.separate <- "\u2013"
  }
  sprintf("%%s %s%%s%s%%s%s",
          substr(conf.brackets, 1, 1),
          conf.separate,
          substr(conf.brackets, 2, 2))
}


helpers_fmt_reg <- function(data, varnames, fold, coefs){
  add.first <- NULL
  add.last <- NULL
  if("(Intercept)" %in% coefs$term){
    add.first <- "(Intercept)"
  }

  if(any(regex_detect(coefs$term, pattern = ":", fixed = TRUE))){
    add.last <- coefs$term[regex_detect(coefs$term, pattern = ":", fixed = TRUE)]
  }

  fmt_reg(data = data,
                  varnames = varnames,
                  fold = fold,
                  add.first = add.first,
                  add.last = add.last)
}


helpers_describe_event <- function(data, event = NULL, varnames = NULL, method = "n.total", digits = 1){
  if(length(unique(data[[event]])) != 2L){
    res <- lapply(varnames, function(x){
      if(is.factor(data[[x]]) | is.character(data[[x]])){
        res <- freq(x = data[[x]])
        res$term <- paste0(x, res$term)
        res
      }else if (is.numeric(data[[x]])){
        data.frame(term = x, overall = length(data[[x]]))
      }
    })
    res <- do.call(rbind, res)
    names(res)[2] <- "n.total"
    res[,] <- lapply(res[,], function(x) as.character(x))
    method <- "n.total"
    res

  }else{
    res <- lapply(varnames, function(x){
      if(is.factor(data[[x]]) | is.character(data[[x]])){
        res <- freq(data[[x]], g = data[[event]])
        res[[2]] <- as.numeric(res[[2]])
        res[[3]] <- as.numeric(res[[3]])
        res$total <- res[[2]] + res[[3]]
        res$event <- sprintf("%d/%d", res[[3]], res$total)
        res$term <- paste0(x, res$term)
      }else if (is.numeric(data[[x]])){
        res <- freq(rep(x, nrow(data)), g = data[[event]])
        res[[2]] <- as.numeric(res[[2]])
        res[[3]] <- as.numeric(res[[3]])
        res$total <- res[[2]] + res[[3]]
        res$event <- sprintf("%d/%d", res[[3]], res$total)
      }
      res
    })
    res <- do.call(rbind, res)
    names(res) <- c("term", "n.non.event", "n.event", "n.total", "n.event.total")
    res$r.non.event <- sprintf("%s%%", fmt_digits(res$n.non.event / res$n.total * 100, 1))
    res$r.event <- sprintf("%s%%", fmt_digits(res$n.event / res$n.total * 100, 1))
    res$n.ratio <- sprintf("%d (%s)", res$n.event, res$r.event)
    res[,] <- lapply(res[,], function(x) as.character(x))
    res
  }
}


freq <- function(x, g = NULL, type = 1, digits = NULL){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  freqs <- as.vector(freqs)
  if(is.null(digits)){
    freqs <- sprintf("%d", freqs)
  }
  freqs <- matrix(freqs, ncol = ncol)
  colnames(freqs)  <- cname
  freqs <- as.data.frame(freqs, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), freqs)
}


helpers_subset_stat <- function(data, values){

  values <- tolower(values)
  values <- sapply(values, function(x){
    if(x %in% c("b", "estimate")){
      "estimate"
    }else if(x %in% c("se", "std.error")){
      "std.error"
    }else if(x %in% c("effect", "e", "hr", "or", "rr", "pr")){
      "effect"
    }else if(x %in% c("stat", "statistic", "wald", "t", "t.value")){
      "statistic"
    }else if( x %in% c("p", "p.value", "pvalue")){
      "p.value"
    }else if(x %in% c("n.total", "ntotal", "total", "n", "nt")) {
      "n.total"
    }else if(x %in% c("n.event", "nevent", "event", "ne")) {
      "n.event"
    }else if(x %in% c("n.nonevent", "nnonevent", "nonevent", "nne")) {
      "n.non.event"
    }else if(x %in% c("n.event.total", "neventtotal", "net")) {
      "n.event.total"
    }else{
      NA
    }
  })
  values <- unique(values)
  values <- values[!is.na(values)]
  values <- values[values %in% names(data)]
  data <- data[c("term", "varname", "ref", "variable", values)]
  data
}


helpers_set_reference <- function(data, value, digits.effect = 2){
  if(is.numeric(value)){
    value <- fmt_digits(value, digits.effect)
  }
  if(!is.null(data$estimate)){
    data$estimate[data$ref] <- fmt_digits(0, digits.effect)
  }
  if(!is.null(data$effect)){
    data$effect[data$ref] <- value
  }
  data
}


helpers_rename_output <- function(out,
                                  variable = "Variable",
                                  estimate = "Estimate",
                                  effect = "Effect",
                                  statistic = "Statistic",
                                  std.error = "Std. error",
                                  p.value = "P value",
                                  conf.level = 0.95){

  names(out)[which(names(out) == "variable")] <- variable
  names(out)[which(names(out) == "estimate")] <- estimate
  names(out)[which(names(out) == "effect")] <- sprintf("%s (%d%% CI)", effect, conf.level * 100)
  names(out)[which(names(out) == "std.error")] <- std.error
  names(out)[which(names(out) == "statistic")] <- statistic
  names(out)[which(names(out) == "p.value")] <- p.value

  names(out)[which(names(out) == "n.total")] <- "No. of total"
  names(out)[which(names(out) == "n.event")] <- "No. of event"
  names(out)[which(names(out) == "n.non.event")] <- "No. of nonevent"
  names(out)[which(names(out) == "n.event.total")] <- "No. of event/total"

  out
}


helpers_delete_terms <- function(data, term = FALSE){

  delete_column_by_name <- function(data, varnames){
    index <- sapply(varnames, function(x) { which(names(data) == x) })
    data[, -index, drop = FALSE]
  }

  if(!term){
    if("term" %in% names(data)){
      data <- delete_column_by_name(data, "term")
    }
    if("varname" %in% names(data)){
      data <- delete_column_by_name(data, "varname")
    }
    if("ref" %in% names(data)){
      data <- delete_column_by_name(data, "ref")
    }
  }
  data
}

