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


#' Convert numeric to factor by quantiles
#'
#' @param data a data frame
#' @param varname variable name.
#' @param n indicates how many quantiles to convert.
#' @param median Whether to calculate the median of each group.
#' @param right logical, indicating if the intervals should be closed on the
#' right (and open on the left) or vice versa.
#' @param labels labels for the levels of the resulting category. By default,
#' labels are constructed using "(a,b]" interval notation. If labels = FALSE,
#' simple integer codes are returned instead of a factor.
#' @param ... further arguments passed to or from other methods
#'
#' @return a data frame.
#' @export
cut_quantile <- function(data, varname, n = 4, median = TRUE, right = TRUE, labels = NULL, ...){
  varname <- select_variable(data, varname)
  g <- cut(data[[varname]],
           breaks = stats::quantile(data[[varname]], probs = (0:n) / n),
           include.lowest = TRUE,
           labels = labels,
           right = right,
           ...)
  m <- tapply(data[[varname]], g, median, na.rm = TRUE)
  m <- m[match(g, names(m))]

  if(paste0("gq_", varname) %in% names(data)){
    data <- data[, -which(names(data) == paste0("gq_", varname)),drop = FALSE]
  }
  data <- append2(data, g, after = varname, names = paste0("gq_", varname))
  if(median){
    if(paste0("mq_", varname) %in% names(data)){
      data <- data[, -which(names(data) == paste0("mq_", varname)),drop = FALSE]
    }
    data <- append2(data, m, after = paste0("gq_", varname), names = paste0("mq_", varname))
  }
  data
}


#' Describe event
#'
#' @param data a data frame.
#' @param event event variable name.
#' @param varnames variable names.
#' @param method describe method.
#' @param digits digits.
#'
#' @return a data frame.
#' @export
describe_event <- function(data, event = NULL, varnames = NULL, method = "n.total", digits = 1){
  helpers_describe_event(data,
                         event = event,
                         varnames = varnames,
                         method = method,
                         digits = digits)
}


#' Enhanced c function
#'
#' @param text a vector.
#'
#' @return a vector.
#' @export
#'
#' @examples
#'
#' cc("age, gender")
#' cc(c("age, gender", "man"))
cc <- function(text){
  out <- lapply(text, \(x){
    if(regex_detect(x, pattern = ",", fixed = TRUE)){
      regex_split(string = x, pattern = ",\\s+")[[1]]
    }else{
      x
    }
  })
  unlist(out)
}


#' As Enhanced c function
#'
#' @param text a vector.
#'
#' @return a vector.
#' @export
#'
#' @examples
#' as_cc(c("a", "b", "c"))
as_cc <- function(text){
  paste(text, collapse = ", ")
}


#' As formula
#'
#' @param x x variables.
#' @param y y varaibles.
#'
#' @return a formula.
#' @export
as_frm <- function(x = NULL, y = NULL){
  if(!is.null(x)){
    x <- cc(x)
    x <- paste(x, collapse = " + ")
  }else{
    x <- "1"
  }

  if(is.null(y)){
    frm <- paste(" ~ ", x, sep = "")
  }else{
    frm <- paste(y, x, sep = " ~ ")
  }
  stats::as.formula(frm)
}


#' View data
#'
#' @param data a data frame
#'
#' @return a data frame
#' @export
overview <- function(data){

  out <- lapply(names(data), \(x){

    if(any(is.na(data[[x]]))){
      Unique <- length(unique(data[[x]])) - 1
    }else{
      Unique <- length(unique(data[[x]]))
    }

    Missing <- sprintf("%d (%.1f%%)", sum(is.na(data[[x]])),
                                          sum(is.na(data[[x]]) / length(data[[x]]) * 100))

    data.frame(Variable = x,
               Labels = get_var_label(data, x, default = ".name"),
               Missing = Missing,
               Unique = Unique)
  })
  out <- list_rbind(out, varname = "Col")
  class(out) <- c("overview", "data.frame")
  out
}


#' Print overview object
#'
#' @param x an overview object
#' @param ... unused.
#'
#' @keywords internal
#' @export
print.overview <- function(x, ...){
  print_booktabs(x, ...)
}


#' Sample n rows from a data frame
#'
#' @param data A data.frame.
#' @param size  the number of rows to select.
#' @param replace Sample with or without replacement?
#' @param seed 	a single value, interpreted as an integer, or NULL.
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' rand_sample_n(iris, size = 15)
rand_sample_n <- function(data, size, replace = FALSE, seed = 1234){
  set.seed(seed)
  if(size > nrow(data)){
    size <- nrow(data)
  }
  index <- sample(1:nrow(data), size = size, replace = replace)
  data <- data[index, , drop = FALSE]
  row.names(data) <- NULL
  data
}


#' Length of unique values
#'
#' @param x an R object.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#' values should be stripped before the computation proceeds.
#'
#' @export
#' @examples
#' unique_length(iris$Species)
unique_length <- function(x, na.rm = FALSE){
  if(na.rm){
    length(unique(na.omit(x)))
  }else{
    length(unique(x))
  }
}
