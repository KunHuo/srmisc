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
                    Variable = "\u53d8\u91cf",
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
                    Nomogram = "\u5217\u7ebf\u56fe\u6a21\u578b",
                    Characteristic = "\u7279\u5f81")


  TITLES <- list("Performance metrics" = "\u9884\u6d4b\u6548\u80fd\u8bc4\u4ef7\u6307\u6807",
                 "Wilcoxon rank sum test" = "Wilcoxon\u79e9\u548c\u68c0\u9a8c",
                 "Baseline characteritics" = "\u60a3\u8005\u57fa\u7ebf\u8d44\u6599",
                 "Multivariable logistic regression analysis results for nomogram" = "\u5217\u7ebf\u56fe\u6a21\u578b\u7684\u591a\u56e0\u7d20\u006c\u006f\u0067\u0069\u0073\u0074\u0069\u0063\u56de\u5f52\u5206\u6790\u7ed3",
                 "Univariable logistic regression analysis results for nomogram" = "\u5217\u7ebf\u56fe\u6a21\u578b\u7684\u5355\u56e0\u7d20\u006c\u006f\u0067\u0069\u0073\u0074\u0069\u0063\u56de\u5f52\u5206\u6790\u7ed3")

  NOTES <- list("Calculated by one-way analysis of variance." = "\u91c7\u7528\u5355\u56e0\u7d20\u65b9\u5dee\u5206\u6790\u8ba1\u7b97\u3002",
                "Calculated by Kruskal-Wallis rank sum test." = "\u91c7\u7528kruskal-wallis\u79e9\u548c\u68c0\u9a8c\u8ba1\u7b97\u3002",
                "Calculated by two sample t-test." = "\u91c7\u7528\u72ec\u7acb\u6837\u672ct\u68c0\u9a8c\u8ba1\u7b97\u3002",
                "Calculated by Wilcoxon rank sum test." = "\u91c7\u7528wilcoxon\u79e9\u548c\u68c0\u9a8c\u8ba1\u7b97\u3002",
                "Calculated by Pearson's Chi-squared test." = "\u91c7\u7528\u5361\u65b9\u68c0\u9a8c\u8ba1\u7b97\u3002",
                "Calculated by Fisher's exact test." = "\u91c7\u7528fisher\u7cbe\u786e\u6982\u7387\u6cd5\u8ba1\u7b97\u3002",
                "Data were mean\u00B1SD, median (IQR) or n(%), unless otherwise specified." = "\u6ce8\uff1a\u9664\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u63cf\u8ff0\u4e3a\u5747\u6570\u00b1\u6807\u51c6\u5dee\u3001\u4e2d\u4f4d\u6570\u0028\u0049\u0051\u0052\u0029\u6216\u006e\u0028\u0025\u0029\u3002",
                "Data were median (IQR) or n(%), unless otherwise specified."  = "\u6ce8\uff1a\u9664\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u63cf\u8ff0\u4e3a\u4e2d\u4f4d\u6570\u0028\u0049\u0051\u0052\u0029\u6216\u006e\u0028\u0025\u0029\u3002",
                "Data were median (IQR), unless otherwise specified." = "\u6ce8\uff1a\u9664\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u63cf\u8ff0\u4e3a\u4e2d\u4f4d\u6570\u0028\u0049\u0051\u0052\u0029\u3002",
                "Data were n(%), unless otherwise specified." = "\u6ce8\uff1a\u9664\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u63cf\u8ff0\u4e3a\u006e\u0028\u0025\u0029\u3002",
                "Data were mean\u00B1SD, unless otherwise specified." = "\u6ce8\uff1a\u9664\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u63cf\u8ff0\u4e3a\u5747\u6570\u00b1\u6807\u51c6\u5dee\u3002",
                "Data were mean\u00B1SD or n(%), unless otherwise specified." = "\u6ce8\uff1a\u9664\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u63cf\u8ff0\u4e3a\u5747\u6570\u00b1\u6807\u51c6\u5dee\u6216\u006e\u0028\u0025\u0029\u3002")

  for(i in 1:length(VARIABLES)){
    # pattern <- paste0("^", names(VARIABLES)[i], "\b")
    pattern <- names(VARIABLES)[i]
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

    # pattern <- paste0("(^|\s)", names(VARIABLES)[i], "\b")

    pattern <- names(VARIABLES)[i]
    for(j in 1:nrow(data)){
      if(regex_detect(string = data[j, 1],
                      pattern = pattern, fixed = TRUE,
                      ignore.case = TRUE)){
        data[j, 1] <- regex_replace(string = data[j, 1],
                                    pattern = pattern,
                                    replacement = VARIABLES[[i]],
                                    ignore.case = TRUE,
                                    fixed = TRUE)
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

  note <- attr(data, "note")
  if(!is.null(note)){
    for(i in 1:length(NOTES)){
      note <- regex_replace(note, names(NOTES)[i], NOTES[[i]], fixed = TRUE)
    }
    attr(data, "note") <- note
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
#' @param SD Whether to calculate the per 1-SD.
#' @param right logical, indicating if the intervals should be closed on the
#' right (and open on the left) or vice versa.
#' @param labels labels for the levels of the resulting category. By default,
#' labels are constructed using "(a,b]" interval notation. If labels = FALSE,
#' simple integer codes are returned instead of a factor.
#' @param ... further arguments passed to or from other methods
#'
#' @return a data frame.
#' @export
cut_quantile <- function(data, varname, n = 4, median = TRUE, SD = TRUE, right = TRUE, labels = NULL, ...){
  varname <- select_variable(data, varname)

  if(!is.null(labels)){
    if(labels == 3){
      labels <- sprintf("Tertile %d", 1:3)
    }else if(labels == 4){
      labels <- sprintf("Quartile %d", 1:4)
    }else if(labels == 5){
      labels <- sprintf("Quintile %d", 1:5)
    }
  }

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


  if(SD){
    if(paste0("sd_", varname) %in% names(data)){
      data <- data[, -which(names(data) == paste0("sd_", varname)),drop = FALSE]
    }
    s <- data[[varname]] / stats::sd(data[[varname]], na.rm = TRUE)
    data <- append2(data, s, after = paste0("gq_", varname), names = paste0("sd_", varname))
  }

  if(is.null(labels)){
    if(n == 3){
      data[[paste0("gq_", varname)]] <- factor(data[[paste0("gq_", varname)]],
                                               levels = levels(data[[paste0("gq_", varname)]]),
                                               labels = sprintf("%s %d %s", "Tertile", 1:3, levels(data[[paste0("gq_", varname)]])))
    }else if(n == 4){
      data[[paste0("gq_", varname)]] <- factor(data[[paste0("gq_", varname)]],
                                               levels = levels(data[[paste0("gq_", varname)]]),
                                               labels = sprintf("%s %d %s", "Quartile", 1:4, levels(data[[paste0("gq_", varname)]])))
    }else if(n == 5){
      data[[paste0("gq_", varname)]] <- factor(data[[paste0("gq_", varname)]],
                                               levels = levels(data[[paste0("gq_", varname)]]),
                                               labels = sprintf("%s %d %s", "Quintile", 1:5, levels(data[[paste0("gq_", varname)]])))
    }
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
      regex_split(string = x, pattern = ",\\s*")[[1]]
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


#' Overview of Data Structure
#'
#' Generate an overview of the data structure including variable names, labels
#' (if available), class, missing values, unique values, and sample values.
#'
#' @param data The input data frame.
#' @param digits The number of digits to display for percentages. Default is 1.
#' @param label Logical indicating whether to include variable labels. Default is FALSE.
#' @param value Logical indicating whether to show values Default is FALSE.
#' @param language Specify language, 'en' is English, 'cn' or 'zh' is Chinese.
#' Default is 'en'.
#'
#'
#' @return A data frame providing an overview of the data structure.
#'
#' @details This function generates an overview of the data structure including
#' variable names, labels (if available), class, missing values, unique values,
#' and sample values. If the number of unique values for a variable exceeds 5,
#' only the first 5 values are displayed followed by "...". The function
#' calculates the percentage of missing values for each variable.
#'
#' @export
#'
#' @examples
#' overview(iris)
overview <- function(data, digits = 1, label = FALSE, value = FALSE, language = "en"){

  out <- lapply(names(data), \(x){
    if(label){
      Variable <- get_var_label(data, x, default = ".name")
    }else{
      Variable <- x
    }

    if(unique_length(data[[x]]) <= 5L){
      Value <- paste(unique(data[[x]]), collapse = ", ")
    }else{
      Value <- paste0(paste(utils::head(data[[x]]), collapse = ", "), ", ...")
    }

    data.frame(Variable = Variable,
               Label = get_var_label(data, x, default = ".name"),
               Type = data_type(class(data[[x]]), language),
               "Missing (n)" = sum(is.na(data[[x]])),
               "Missing (%)" = sprintf("%s%%", fmt_digits(sum(is.na(data[[x]])) / length(data[[x]]) * 100, digits = digits)),
               Unique = unique_length(data[[x]]),
               Value = Value)
  })


  out <- list_rbind(out, varname = "No.")
  names(out)[5:6] <- c("Miss (n)", "Miss (%)")

  if(language != "en"){
    names(out) <- c("\u5e8f\u53f7",  # No.
                    "\u53d8\u91cf",  # Variable
                    "\u6807\u7b7e",  # Label
                    "\u6570\u636e\u7c7b\u578b", # Type
                    "\u7f3a\u5931\u4f8b\u6570", # Miss (n)
                    "\u7f3a\u5931\u6bd4\u4f8b", # Miss (%)
                    "\u552f\u4e00\u503c", # Unique
                    "\u6570\u636e")        # Value
  }

  if(language == "en"){
    fmt <- "Note: %d of the %d variables had missing values."
  }else{
    fmt <- "\u6ce8\uff1a%d\u4e2a\u53d8\u91cf(\u5171%d\u4e2a\u53d8\u91cf)\u5b58\u5728\u7f3a\u5931\u503c\u3002"
  }

  if(sum(out[[5]]) == 0){
    out <- out[-c(5:6)]
    attr(out, "note") <- sprintf(fmt, 0, ncol(data))
  }else{
    attr(out, "note") <- sprintf(fmt, sum(out[[5]] != 0), ncol(data))
  }


  if(!value){
    out <- out[-ncol(out)]
  }

  attr(out, "title") <- ifelse(language == "en", "Data structure", "\u6570\u636e\u7ed3\u6784")

  class(out) <- c("overview", "data.frame")
  out
}


#' Print Method for Overview Objects
#'
#' Print method for objects of class "overview".
#'
#' @param x An object of class "overview".
#' @param ... Additional arguments to be passed to print_booktabs function.
#'
#' @details This function prints the overview object using the print_booktabs
#' function, which is suitable for printing tables with booktabs formatting.
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
    length(unique(stats::na.omit(x)))
  }else{
    length(unique(x))
  }
}


#' Add table number
#'
#' @param data a data frame.
#' @param num table number.
#' @param chinese Chinese, default FALSE.
#' @param sep  separator.
#'
#' @return a data frame.
#' @export
add_table_num <- function(data, num = 1, chinese = FALSE, sep = ":"){

  title <- attr(data, "title")

  if(is_empty(title)){
    if(chinese){
      title <- sprintf("\u8868%s", as.character(num))
    }else{
      title <- sprintf("Table %s", as.character(num))
    }
  }else{
    if(chinese){
      title <- sprintf("\u8868%s  %s", as.character(num), title)
    }else{
      title <- sprintf("Table %s%s  %s", as.character(num), sep, title)
    }
  }

  data <- add_title(data, title)
  data
}


#' Find Text in Files
#'
#' This function searches for a specific text pattern within files in the 'R' directory.
#'
#' @param text A character string specifying the text pattern to search for.
#' @param ... Additional arguments to be passed to the [regex_detect] function.
#'
#' @return A character vector containing the filenames where the text pattern was found.
#'
#' @keywords internal
#'
#' @export
find_text <- function(text, ...){
  files <- list.files(path = "R", full.names = TRUE)
  res <- lapply(files, \(file){
    filetext <- readLines(file)

    index <- regex_detect(filetext, pattern = text, ...)

    if(any(index)){
      data.frame(files = basename(file), lines = which(index) )
    }

  })
  res <- do.call(rbind, res)

  if(!is.null(res)){
    print_booktabs(res)
  }else{
    cat("\nNot find.\n\n")
  }
  invisible(res)
}


#' Find non-ASCII characters
#'
#' This function searches for non-ASCII characters in the text files located in
#' the "R" directory.
#'
#' @return A data frame containing information about the files, lines,
#' and non-ASCII characters found.
#'
#' @keywords internal
#'
#' @export
find_non_ascii <- function(){
  files <- list.files(path = "R", full.names = TRUE)

  res <- lapply(files, \(file){

    filetext <- readLines(file)

    index <- stringr::str_detect(filetext, pattern = '[^\\x00-\\x7F]')

    if(any(index)){
      lines <- which(index)

      values <- sapply(lines, \(i){
        v <- stringr::str_extract_all(filetext[i], pattern = '[^\\x00-\\x7F]')[[1]]
        v <- paste(v, collapse = "  ")
      })

      data.frame(files = basename(file), lines = lines, values = values)
    }

  })
  res <- do.call(rbind, res)

  if(!is.null(res)){
    print_booktabs(res)
  }else{
    cat("\nNot find.\n\n")
  }
  invisible(res)
}


#' Split a Text into Lines
#'
#' This function splits a given text into lines based on a separator.
#'
#' @param ... One or more character vectors containing the text to be combined
#' into lines.
#' @param sep The separator used to combine the texts into lines.
#'
#' @return A character vector representing the lines of the input text.
#'
#' @examples
#'
#' l('This function splits a given text
#'    into lines based on a separator.')
#'
#' l('With 25–30 variables potentially associated with mortality in critically ill
#'   patients with COVID-19 the minimum sample size required approximately 250–300
#'   deaths.',
#'
#'   'Comparisons of continuous variables between groups were done using t-tests,
#'   one-way ANOVA, or equivalent non-parametric tests.'
#' )
#'
#' l('With 25–30 variables potentially associated with mortality in critically ill
#'   patients with COVID-19 the minimum sample size required approximately 250–300
#'   deaths.',
#'
#'   'Comparisons of continuous variables between groups were done using t-tests,
#'   one-way ANOVA, or equivalent non-parametric tests.',
#'   sep = "\n\n")
#'
#' @export
l <- function(..., sep = "\n"){

  lt <- list(...)

  text <- sapply(lt, \(x){
    # Replace any occurrences of newline characters surrounded by optional spaces
    # with the specified separator
    gsub("\\s*\n\\s*", " ", x)
  })

  text <- paste(text, collapse = sep)
  class(text) <- c("lines", "character")
  text
}


#' Print Method for Lines
#'
#' This function defines a print method for objects of class "lines".
#'
#' @param x An object of class "lines" to be printed.
#' @param ... Additional arguments to be passed to the [cat] function.
#'
#' @examples
#' x <- l("This is a\nmultiline\nstring.")
#' print(x)
#'
#' @keywords internal
#'
#' @export
print.lines <- function(x, ...){
  cat(x, ...)
}

#' Calculate counts and proportions for a vector, including missing values
#'
#' This function computes the counts and proportions for each unique category in a vector.
#' If there are missing values (NA), it includes them as a category labeled "<NA>".
#' The function also adds a row showing the total count and proportion (always 1).
#'
#' @param x A vector (numeric, character, or factor) for which the counts and proportions are to be computed.
#'
#' @return A data frame containing the categories, their counts, and proportions,
#'         as well as a "Total" row that sums all counts and assigns a proportion of 1.
#'
#' @examples
#' x <- c("Male", "Female", "Female", NA, "Male", "Male", "Female")
#' counts_prop(x)
#'
#' y <- c(1, 2, 2, NA, 1, 1, 3)
#' counts_prop(y)
#'
#' @export
counts_prop <- function(x) {
  count_table <- table(x, useNA = "ifany")
  proportions <- count_table / sum(count_table)

  result <- data.frame(
    Category = names(count_table),
    Count = as.integer(count_table),
    Proportion = round(as.numeric(proportions), 4),
    stringsAsFactors = FALSE
  )

  result$Category[which(is.na(result$Category))] <- "<NA>"

  total_count <- sum(count_table)
  total_proportion <- 1
  total_row <- data.frame(
    Category = "Total",
    Count = total_count,
    Proportion = total_proportion,
    stringsAsFactors = FALSE
  )

  result <- rbind(result, total_row)

  return(result)
}

#' Sum values across specified columns for each row in a data frame
#'
#' This function calculates the sum of specified columns for each row in a data frame.
#' If all values in the specified columns are missing (NA), it returns NA for that row.
#' The function also allows for excluding NA values when calculating the sum.
#'
#' @param data A data frame containing the data.
#' @param cols A vector of column names or indices that specify the columns to be summed.
#' @param na.rm A logical value indicating whether to remove NA values before summing. Default is TRUE.
#'
#' @return A vector of sums for each row, with NA returned for rows where all values in the specified columns are NA.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(5, NA, 7, 8))
#' sum_rows(df, c("a", "b"))
#' sum_rows(df, c("a", "b"), na.rm = FALSE)
#'
#' @export
sum_rows <- function(data, cols, na.rm = TRUE) {
  cols <- select_variable(data, cols)
  apply(data[cols], 1, function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(sum(x, na.rm = na.rm))
    }
  })
}

