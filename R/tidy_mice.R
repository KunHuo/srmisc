tidy_mice <- function(x, conf.int = TRUE, exp = FALSE, conf.level = 0.95, ...){

  ret <- mice::pool(x)
  ret <- summary(ret)

  if(exp){
    ret$effect <- exp(ret$estimate)
    if (conf.int) {
      ret$conf.low  <- exp(ret$estimate - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * ret$std.error)
      ret$conf.high <- exp(ret$estimate + stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * ret$std.error)
    }
  }else{
    ret$effect <- ret$estimate
    if (conf.int) {
      ret$conf.low  <- ret$estimate - stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * ret$std.error
      ret$conf.high <- ret$estimate + stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE) * ret$std.error
    }
  }

  if("df" %in% names(ret)){
    ret <- ret[-which(names(ret) == "df")]
  }
  ret$term <- as.character(ret$term)
  class(ret) <- "data.frame"
  ret
}
