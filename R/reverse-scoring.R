#' Reverse Scoring Function for Vectors and Data Frames
#'
#' @description Performs reverse scoring on numeric vectors or specified columns in a data frame.
#'              Commonly used for psychological scales and questionnaire data processing.
#'
#' @param x Input data: a numeric vector or data frame containing numeric columns
#' @param vars Optional; for data frames: column names/indexes to reverse.
#'             If NULL (default), reverses all numeric columns.
#' @param min_score Optional manual override for minimum possible score
#' @param max_score Optional manual override for maximum possible score
#'
#' @return Object of the same type as input (vector or data frame) with reverse-scored values
#'
#' @examples
#' # Vector example
#' reverse_scoring(1:5)  # Returns c(5,4,3,2,1)
#'
#' # Data frame example
#' df <- data.frame(Q1 = 1:5, Q2 = c(0,1,2,3,4))
#' reverse_scoring(df)
#'
#' # Specify columns and scoring range
#' reverse_scoring(df, vars = "Q1", min_score = 1, max_score = 5)
#'
#' @export
reverse_scoring <- function(x, vars = NULL, min_score = NULL, max_score = NULL) {

  vars <- cc(vars)

  # Validate input type
  if (!is.vector(x) && !is.data.frame(x)) {
    stop("Input must be a vector or data frame")
  }

  # Core reverse scoring logic
  reverse_core <- function(vec) {
    if (!is.numeric(vec)) {
      warning("Non-numeric data skipped in reverse scoring")
      return(vec)
    }

    actual_min <- if (!is.null(min_score)) min_score else min(vec, na.rm = TRUE)
    actual_max <- if (!is.null(max_score)) max_score else max(vec, na.rm = TRUE)

    actual_max + actual_min - vec
  }

  # Handle vector input
  if (is.vector(x) && !is.list(x)) {
    return(reverse_core(x))
  }

  # Handle data frame input
  if (is.data.frame(x)) {
    # Auto-select numeric columns if vars not specified
    if (is.null(vars)) {
      numeric_cols <- sapply(x, is.numeric)
      vars <- names(x)[numeric_cols]
      if (length(vars) == 0) warning("No numeric columns found")
    }

    # Convert numeric indexes to names
    if (is.numeric(vars)) vars <- names(x)[vars]

    # Process specified columns
    for (var in vars) {
      if (var %in% names(x)) {
        x[[var]] <- reverse_core(x[[var]])
      } else {
        warning(paste("Column", var, "not found - skipping"))
      }
    }
    return(x)
  }

  stop("Unsupported input type")
}
