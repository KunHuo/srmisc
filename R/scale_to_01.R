#' Scale numeric data to a custom range
#'
#' This function scales numeric data to a specified range
#' It handles vectors, matrices, and dataframes. For dataframes, you can specify
#' which variables to scale.
#'
#' @param x A numeric vector, matrix, or dataframe containing the data to scale
#' @param vars For dataframe input: character vector of column names to scale.
#'             If NULL (default), scales all numeric columns.
#' @param range Numeric vector of length 2 specifying the desired output range.
#'              Default is c(0, 1).
#' @param na.rm Logical indicating whether to ignore NA values (default TRUE)
#' @return An object of the same class as x with values scaled to specified range
#' @export
scale_range <- function(x, vars = NULL, range = c(0, 1), na.rm = TRUE) {

  vars <- select_numeric(x, vars)

  # Validate range parameter
  if (!is.numeric(range) || length(range) != 2) {
    stop("range must be a numeric vector of length 2")
  }
  if (range[1] >= range[2]) {
    stop("range[1] must be less than range[2]")
  }

  # Internal scaling function
  .scale <- function(v) {
    if (!is.numeric(v)) return(v)  # Skip non-numeric columns

    min_val <- min(v, na.rm = na.rm)
    max_val <- max(v, na.rm = na.rm)

    # Handle constant values
    if (min_val == max_val) {
      warning("All values are identical in one or more variables. Setting to midpoint of range.")
      return(rep(mean(range), length(v)))
    }

    # Standardize to [0,1] first
    standardized <- (v - min_val) / (max_val - min_val)
    # Then scale to desired range
    standardized * (range[2] - range[1]) + range[1]
  }

  # Handle different input types
  if (is.data.frame(x)) {
    if (is.null(vars)) {
      # Scale all numeric columns if vars not specified
      numeric_cols <- sapply(x, is.numeric)
      x[numeric_cols] <- lapply(x[numeric_cols], .scale)
    } else {
      # Check if specified vars exist
      missing_vars <- setdiff(vars, names(x))
      if (length(missing_vars) > 0) {
        stop("Variables not found in dataframe: ", paste(missing_vars, collapse = ", "))
      }
      # Scale only specified vars
      x[vars] <- lapply(x[vars], .scale)
    }
    return(x)
  } else if (is.matrix(x)) {
    if (!is.numeric(x)) {
      stop("Matrix must be numeric")
    }
    return(apply(x, 2, .scale))
  } else if (is.numeric(x)) {
    return(.scale(x))
  } else {
    stop("Input must be a numeric vector, matrix, or dataframe")
  }
}
