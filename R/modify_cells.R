#' Modify values in a data frame with factor handling
#'
#' @description
#' Safely modify values in specified cells of a data frame, including proper
#' handling of factor levels when updating factor columns.
#'
#' @param df A data frame to modify
#' @param rows Numeric/character vector specifying target rows (indices or names)
#' @param cols Numeric/character vector specifying target columns (indices or names)
#' @param values Vector of values to insert (must match length of rows/cols)
#'
#' @return Modified data frame with updated values and adjusted factor levels
#'
#' @details
#' Features:
#' - Handles both numeric indices and named rows/columns
#' - Automatically adds new factor levels when needed
#' - Preserves existing factor levels and column types
#' - Validates input dimensions and existence of rows/columns
#'
#' @examples
#' # Create sample data
#' df <- data.frame(
#'   ID = 1:3,
#'   Grade = factor(c("A", "B", "C")),
#'   Score = c(90, 85, 88),
#'   row.names = c("Row1", "Row2", "Row3")
#' )
#'
#' # Modify single cell (numeric index)
#' modify_cells(df, rows=2, cols=3, values=95)
#'
#' # Modify multiple cells with row/col names
#' modify_cells(df,
#'             rows=c("Row1", "Row3"),
#'             cols=c("Grade", "Score"),
#'             values=c("D", 100))
#'
#' # Add new factor level
#' modify_cells(df, rows=1, cols="Grade", values="D")
#'
#' @export
modify_cells <- function(df, rows, cols, values) {
  # Input validation
  if (!is.data.frame(df)) stop("Input must be a data frame")
  if (length(rows) != length(cols) || length(rows) != length(values)) {
    stop("rows, cols, and values must have equal length")
  }

  # Convert to numeric indices
  col_indices <- convert_to_indices(cols, names(df), "column")
  row_indices <- convert_to_indices(rows, rownames(df), "row")

  # Factor level preprocessing
  df <- update_factor_levels(df, col_indices, values)

  # Perform modifications
  for (i in seq_along(values)) {
    df[row_indices[i], col_indices[i]] <- values[i]
  }

  df <- droplevels(df)

  return(df)
}

# Helper function: Convert names to indices
convert_to_indices <- function(input, names, type) {
  if (is.character(input)) {
    indices <- match(input, names)
    if (anyNA(indices)) {
      stop("Invalid ", type, " names: ", paste(input[is.na(indices)], collapse=", "))
    }
  } else if (is.numeric(input)) {
    indices <- input
    max_index <- if (type == "column") length(names) else nrow(names)
    if (any(indices < 1 | indices > max_index)) {
      stop(type, " indices out of bounds")
    }
  } else {
    stop(type, " specification must be numeric or character")
  }
  return(indices)
}

# Helper function: Update factor levels
update_factor_levels <- function(df, col_indices, values) {
  unique_cols <- unique(col_indices)

  for (col in unique_cols) {
    if (is.factor(df[[col]])) {
      # Get values for this column
      relevant_values <- values[col_indices == col]

      # Check for new levels
      current_levels <- levels(df[[col]])
      new_levels <- unique(c(current_levels, relevant_values))

      # Update factor levels if needed
      if (length(new_levels) > length(current_levels)) {
        df[[col]] <- factor(df[[col]], levels = new_levels)
      }
    }
  }

  # df <- droplevels(df)

  return(df)
}
