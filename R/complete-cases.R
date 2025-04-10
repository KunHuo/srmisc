
#' Filter Missing Values from Vectors and Data Frames
#'
#' @description Removes NA values from vectors or filters complete cases (rows with no NA)
#'              in data frames. Supports subsetting specific columns for data frame validation.
#'
#' @param x Input data (vector or data frame)
#' @param vars Optional; for data frames: column names/indexes to check for NAs.
#'             If NULL (default), checks all columns.
#'
#' @return Filtered data matching the input type:
#' - For vectors: vector with NA values removed
#' - For data frames: data frame with complete rows
#'
#' @examples
#' # Vector example
#' complete_cases(c(1, NA, 3))  # Returns c(1, 3)
#'
#' # Data frame examples
#' df <- data.frame(A = c(1, NA, 3), B = c(NA, 5, 6))
#' complete_cases(df)             # Keeps only complete rows
#' complete_cases(df, vars = "A") # Keeps rows where column A is not NA
#'
#' @export
complete_cases <- function(x, vars = NULL) {
  if (is.vector(x)) {
    # Vector input: remove NA values
    return(x[!is.na(x)])

  } else if (is.data.frame(x)) {
    # Data frame input
    if (is.null(vars)) {
      # Check all columns
      return(x[stats::complete.cases(x), ])
    } else {
      # Check specified columns
      vars <- select_variable(x, vars)
      rows_to_keep <- stats::complete.cases(x[, vars, drop = FALSE])
      return(x[rows_to_keep, ])
    }

  } else {
    stop("Input must be a data.frame or vector.")
  }
}
