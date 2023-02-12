#' Is object an empty vector or NULL and NA?
#'
#' @param x object to test.
#'
#' @return a logical.
#' @export
#'
#' @examples
#' is_empty(NULL)
#' is_empty(list())
#' is_empty(list(NULL))
is_empty <- function(x){
  length(x) == 0L
}


delete_duplicate_values <- function(x){
  for(i in rev(seq_along(x))){
    if(i != 1){
      if(x[i] == x[i - 1]){
        x[i] <- NA
      }
    }
  }
  return(x)
}


#' Change language to Chinese
#'
#' @param data a data frame
#'
#' @return a data frame.
#' @export
chinese <- function(data){

  ldata <- data

  if("list" %in% class(ldata)){
    data <- data[[1]]
  }

  VARIABLES <- list(variable = "\u53d8\u91cf",
                    Items = "\u9879\u76ee",
                    overall = "\u6240\u6709",
                    Statistic = "\u7edf\u8ba1\u503c",
                    "P value" = "P\u503c",
                    Threshold = "\u6700\u4f73\u622a\u65ad\u503c",
                    "Cut-off" = "\u6700\u4f73\u622a\u65ad\u503c",
                    Accuracy = "\u51c6\u786e\u5ea6",
                    Sensitivity = "\u654f\u611f\u5ea6",
                    Specificity = "\u7279\u5f02\u5ea6",
                    PPV = "\u9633\u6027\u9884\u6d4b\u503c",
                    NPV = "\u9634\u6027\u9884\u6d4b\u503c",
                    Combine = "\u8054\u5408",
                    missing = "\u7f3a\u5931",
                    "(Intercept)" = "\u622a\u8ddd\u9879",
                    B = "\u56de\u5f52\u7cfb\u6570",
                    "Std. error" = "\u6807\u51c6\u8bef",
                    wald = "Wald\u7edf\u8ba1\u503c",
                    "No. of event/total" = "\u4e8b\u4ef6\u4f8b\u6570\u002f\u603b\u4f8b\u6570",
                    "No. of total" = "\u603b\u4f8b\u6570",
                    "No. of event" = "\u4e8b\u4ef6\u4f8b\u6570",
                    "Training set" = "\u8bad\u7ec3\u96c6",
                    "Validation set" = "\u9a8c\u8bc1\u96c6",
                    "Brier score" = "Brier\u8bc4\u5206",
                    Models = "\u6a21\u578b",
                    Nomogram = "\u5217\u7ebf\u56fe\u6a21\u578b")


  TITLES <- list("Performance metrics" = "\u9884\u6d4b\u6548\u80fd\u8bc4\u4ef7\u6307\u6807",
                 "Wilcoxon rank sum test" = "Wilcoxon\u79e9\u548c\u68c0\u9a8c",
                 "Baseline characteritics" = "\u60a3\u8005\u57fa\u7ebf\u8d44\u6599",
                 "Multivariable logistic regression analysis results for nomogram" = "\u5217\u7ebf\u56fe\u6a21\u578b\u7684\u591a\u56e0\u7d20\u006c\u006f\u0067\u0069\u0073\u0074\u0069\u0063\u56de\u5f52\u5206\u6790\u7ed3",
                 "Univariable logistic regression analysis results for nomogram" = "\u5217\u7ebf\u56fe\u6a21\u578b\u7684\u5355\u56e0\u7d20\u006c\u006f\u0067\u0069\u0073\u0074\u0069\u0063\u56de\u5f52\u5206\u6790\u7ed3")

  for(i in 1:length(VARIABLES)){
    pattern <- paste0("^", names(VARIABLES)[i], "\\b")
    for(j in 1:ncol(data)){
      if(regex_detect(string = names(data)[j],
                      pattern = pattern,
                      ignore.case = TRUE)){
        names(data)[j] <- regex_replace(string = names(data)[j],
                                        pattern = pattern,
                                        replacement = VARIABLES[[i]],
                                        ignore.case = TRUE)
      }
    }
  }

  for(i in 1:length(VARIABLES)){

    pattern <- paste0("(^|\\s)", names(VARIABLES)[i], "\\b")
    for(j in 1:nrow(data)){
      if(regex_detect(string = data[j, 1],
                      pattern = pattern,
                      ignore.case = TRUE)){
        data[j, 1] <- regex_replace(string = data[j, 1],
                                    pattern = pattern,
                                    replacement = VARIABLES[[i]],
                                    ignore.case = TRUE)
      }

      if(data[j, 1] == "(Intercept)"){
        data[j, 1] <- VARIABLES[["(Intercept)"]]
      }
    }
  }

  title <- attr(data, "title")
  if(!is.null(title)){
    if(title %in% names(TITLES)){
      attr(data, "title") <- TITLES[[title]]
    }
  }


  if("list" %in% class(ldata)){
    ldata[[1]] <- data
    ldata
  }else{
    data
  }

}
