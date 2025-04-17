
#' Relocate specified rows to a target position in a data frame
#'
#' @description
#' This function reorders rows in a data frame by moving specified rows to a
#' designated position while preserving the relative order of other rows.
#' Supports numeric indices, logical conditions, and row names for row selection.
#'
#' @param data A data frame whose rows need to be reordered.
#' @param which Rows to move. Can be:
#'   - Numeric vector of row indices
#'   - Logical vector of same length as `nrow(data)`
#'   - Character vector of row names (if rownames exist)
#' @param to Target position (1-based index) where the selected rows should be moved.
#'   Must be between 1 and `nrow(data) - length(which) + 1`.
#'
#' @return A data frame with rows reordered as specified.
#'
#' @details
#' The function first removes the specified rows from their original positions,
#' then inserts them at the target position. All other rows maintain their
#' relative order. This is different from simple sorting operations.
#'
#' @examples
#' df <- data.frame(ID = 1:5,
#'                  Name = c("A", "B", "C", "D", "E"),
#'                  Score = c(85, 92, 78, 88, 95))
#'
#' # Move rows 3 and 5 to position 2
#' relocate_rows(df, c(3,5), 2)
#'
#' # Move high-scoring rows to the top
#' relocate_rows (df, df$Score > 90, 1)
#'
#' # Move last row to first position
#' relocate_rows(df, nrow(df), 1)
#'
#' @export
relocate_rows <- function(data, which, to) {
  # Input validation
  if (!is.data.frame(data)) stop("'df' must be a data frame")
  n <- nrow(data)
  if (n == 0) stop("Data frame is empty")

  # Process row selection
  if (is.logical(which)) {
    if (length(which) != n) {
      stop("Logical vector length must match number of rows")
    }
    which <- which(which)
  } else if (is.character(which)) {
    if (is.null(rownames(data))) stop("Data frame has no rownames")
    which <- match(which, rownames(data))
    if (anyNA(which)) stop("Some row names not found")
  }

  # Convert to integer indices
  which <- as.integer(which)
  if (any(which < 1 | which > n)) stop("Row indices out of bounds")

  # Validate target position
  if (!is.numeric(to) || length(to) != 1) stop("'to' must be a single number")
  to <- as.integer(to)
  max_pos <- (n - length(which)) + 1
  if (to < 1 || to > max_pos) {
    stop("'to' must be between 1 and ", max_pos)
  }

  # Generate new order
  all_rows <- seq_len(n)
  remaining <- setdiff(all_rows, which)
  new_order <- append(remaining, which, after = to - 1)

  # Return reordered data frame
  data[new_order, , drop = FALSE]
}
