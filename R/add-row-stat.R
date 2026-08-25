#' Add Row Sum
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#' @param digits Integer indicating the number of decimal places. If NULL,
#'   no rounding is performed.
#'
#' @return A data.frame with the row sums appended.
#' @export
add_row_sum <- function(data, cols = NULL, name = "row_sum", digits = NULL) {
  .finalize_row_stat(data, cols, name, digits,
                     function(d) rowSums(d, na.rm = TRUE))
}

#' Add Row Mean
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#' @param digits Integer indicating the number of decimal places. If NULL,
#'   no rounding is performed.
#'
#' @return A data.frame with the row means appended.
#' @export
add_row_mean <- function(data, cols = NULL, name = "row_mean", digits = NULL) {
  .finalize_row_stat(data, cols, name, digits,
                     function(d) rowMeans(d, na.rm = TRUE))
}

#' Add Row Median
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#' @param digits Integer indicating the number of decimal places. If NULL,
#'   no rounding is performed.
#'
#' @return A data.frame with the row medians appended.
#' @export
add_row_median <- function(data, cols = NULL, name = "row_median", digits = NULL) {
  .finalize_row_stat(data, cols, name, digits,
                     function(d) apply(d, 1, stats::median, na.rm = TRUE))
}

#' Add Row Standard Deviation
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#' @param digits Integer indicating the number of decimal places. If NULL,
#'   no rounding is performed.
#'
#' @return A data.frame with the row standard deviations appended.
#' @export
add_row_sd <- function(data, cols = NULL, name = "row_sd", digits = NULL) {
  .finalize_row_stat(data, cols, name, digits,
                     function(d) apply(d, 1, stats::sd, na.rm = TRUE))
}

#' Add Row Minimum
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#' @param digits Integer indicating the number of decimal places. If NULL,
#'   no rounding is performed.
#'
#' @return A data.frame with the row minima appended.
#' @export
add_row_min <- function(data, cols = NULL, name = "row_min", digits = NULL) {
  .finalize_row_stat(data, cols, name, digits,
                     function(d) apply(d, 1, min, na.rm = TRUE))
}

#' Add Row Maximum
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#' @param digits Integer indicating the number of decimal places. If NULL,
#'   no rounding is performed.
#'
#' @return A data.frame with the row maxima appended.
#' @export
add_row_max <- function(data, cols = NULL, name = "row_max", digits = NULL) {
  .finalize_row_stat(data, cols, name, digits,
                     function(d) apply(d, 1, max, na.rm = TRUE))
}

#' Add Row Count of Missing Values
#'
#' @param data A data.frame or matrix.
#' @param cols Columns to include in the calculation. Can be column names,
#'   indices, or NULL (default, all numeric columns).
#' @param name Character string for the name of the new column.
#'
#' @return A data.frame with the row-wise missing counts appended.
#' @export
add_row_na <- function(data, cols = NULL, name = "row_na") {
  .finalize_row_stat(data, cols, name, NULL,
                     function(d) rowSums(is.na(d)))
}

.finalize_row_stat <- function(data, cols, name, digits, stat_fun) {
  data <- as.data.frame(data)

  if (is.null(cols)) {
    cols <- sapply(data, is.numeric)
  }
  cols <- select_numeric(data, varnames = cols, type = "index")

  vals <- stat_fun(data[, cols, drop = FALSE])

  if (!is.null(digits)) {
    vals <- round(vals, digits)
  }

  data[[name]] <- vals
  data
}
