#' Median Split Function
#'
#' Splits a numeric vector into two groups based on the median value.
#'
#' @param x A numeric vector to be split.
#' @param high_label Label for the group above the median (default: "High").
#' @param low_label Label for the group below the median (default: "Low").
#' @param ordered_factor Logical; if TRUE, returns an ordered factor (default: FALSE).
#' @param addvalue Logical; if TRUE, appends the median value to the labels (default: FALSE).
#' @param digits Number of digits to round the median value (default: NULL).
#' @param lower Logical; if TRUE, uses ">" for the high group, otherwise ">=" (default: FALSE).
#'
#' @return A factor vector with two levels: `low_label` and `high_label`.
#'
#' @examples
#' x <- rnorm(100)
#' median_split(x)
#' median_split(x, addvalue = TRUE, digits = 2)
#'
#' @export
median_split <- function(x,
                         high_label = "High",
                         low_label = "Low",
                         ordered_factor = FALSE,
                         addvalue = FALSE,
                         digits = NULL,
                         lower = FALSE) {
  if (!is.numeric(x)) {
    stop("Input variables must be numeric")
  }

  median_val <- median(x, na.rm = TRUE)

  if(lower){
     group <- ifelse(x > median_val, high_label, low_label)
  }else{
    group <- ifelse(x >= median_val, high_label, low_label)
  }

  if(addvalue){
    if(is.null(digits)){
      digits <- max(n_digits(x), na.rm = TRUE)
    }

    if(lower){
      labels <- c(sprintf("%s (≤%s)", low_label, fmt_digits(median_val, digits)),
                  sprintf("%s (>%s)", high_label, fmt_digits(median_val, digits)))
    }else{
      labels <- c(sprintf("%s (<%s)", low_label, fmt_digits(median_val, digits)),
                  sprintf("%s (≥%s)", high_label, fmt_digits(median_val, digits)))
    }

  }else{
    labels <- c(low_label, high_label)
  }

  if (ordered_factor) {
    factor(
      group,
      levels = c(low_label, high_label),
      labels = labels,
      ordered = TRUE
    )
  } else {
    factor(group, levels = c(low_label, high_label), labels = labels)
  }
}


#' Median Split Function for Data Frames
#'
#' Splits specified numeric columns in a data frame into two groups based on the median value.
#'
#' @param df A data frame containing numeric columns to be split.
#' @param cols A character vector of column names or a numeric vector of column indices to split (default: all numeric columns).
#' @param high_label Label for the group above the median (default: "High").
#' @param low_label Label for the group below the median (default: "Low").
#' @param ordered_factor Logical; if TRUE, returns an ordered factor (default: FALSE).
#' @param addvalue Logical; if TRUE, appends the median value to the labels (default: FALSE).
#' @param digits Number of digits to round the median value (default: NULL).
#' @param lower Logical; if TRUE, uses ">" for the high group, otherwise ">=" (default: FALSE).
#'
#' @return A data frame with new columns containing the split groups.
#'
#' @examples
#' df <- data.frame(a = rnorm(100), b = rnorm(100))
#' median_split_df(df, cols = "a")
#' median_split_df(df, cols = c("a", "b"), addvalue = TRUE, digits = 2)
#'
#' @export
median_split_df <- function(df,
                            cols = NULL,
                            high_label = "High",
                            low_label = "Low",
                            ordered_factor = FALSE,
                            addvalue = FALSE,
                            digits = NULL,
                            lower = FALSE) {

  cols <- select_variable(df, cols)

  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  # If cols is NULL, use all numeric columns
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  } else if (is.numeric(cols)) {
    cols <- names(df)[cols]
  }

  # Check if specified columns are numeric
  non_numeric_cols <- cols[!sapply(df[cols], is.numeric)]
  if (length(non_numeric_cols) > 0) {
    stop("The following columns are not numeric: ", paste(non_numeric_cols, collapse = ", "))
  }

  # Apply median_split to each specified column
  for (col in cols) {
    split_col <- median_split(
      x = df[[col]],
      high_label = high_label,
      low_label = low_label,
      ordered_factor = ordered_factor,
      addvalue = addvalue,
      digits = digits,
      lower = lower
    )
    df[[paste0("m_", col)]] <- split_col
  }

  return(df)
}
