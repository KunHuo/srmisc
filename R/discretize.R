#' Discretize a Numeric Variable into Categorical Levels
#'
#' @description
#' Converts a continuous numeric variable into discrete categories based on
#' user-defined cut points and conditions. Supports both vector and data.frame inputs.
#' Levels are applied sequentially; values are assigned to the first matching level.
#'
#' @param data A numeric vector or a data.frame containing the variable to discretize.
#' @param varname Character string specifying the column name when \code{data} is a data.frame.
#'   Ignored if \code{data} is a vector.
#' @param level Character vector of expressions defining the discretization levels.
#'   Supported formats:
#'   \itemize{
#'     \item \code{">=10"} — greater than or equal to 10
#'     \item \code{">10"} — strictly greater than 10
#'     \item \code{"<=20"} — less than or equal to 20
#'     \item \code{"<20"} — strictly less than 20
#'     \item \code{"5-15"} — inclusive range from 5 to 15
#'     \item \code{"-10"} — left unbounded (equivalent to \code{<=10})
#'     \item \code{"10-"} — right unbounded (equivalent to \code{>=10})
#'   }
#' @param labels Optional character vector of labels for the resulting categories.
#'   Must have the same length as \code{level}. If \code{NULL}, the raw level expressions are used.
#' @param new_varname Optional character string specifying a new column name for the result
#'   when \code{data} is a data.frame. If \code{NULL}, the original \code{varname} column is overwritten.
#'
#' @return
#' \itemize{
#'   \item If \code{data} is a vector: returns a factor with the discretized values.
#'   \item If \code{data} is a data.frame: returns the data.frame with the discretized column added or replaced.
#' }
#'
#' @details
#' The function processes levels sequentially. Values are assigned to the first matching level
#' and are not reassigned by subsequent levels. Overlapping levels trigger a warning but do not
#' stop execution. Unmatched values remain as \code{NA}.
#'
#' @examples
#' # Example 1: Basic vector discretization with custom labels
#' ages <- c(3, 8, 16, 30, 65, 80)
#' discretize(ages, level = c("<13", "13-17", "18-64", ">=65"),
#'            labels = c("Child", "Teen", "Adult", "Senior"))
#'
#' # Example 2: Data frame input — add a new column
#' df <- data.frame(student = c("Alice", "Bob", "Charlie"),
#'                  score = c(58, 74, 93))
#' discretize(df, "score", level = c("<60", "60-79", ">=80"),
#'            labels = c("Fail", "Pass", "Excellent"),
#'            new_varname = "grade")
#'
#' # Example 3: Unbounded ranges and strict inequalities
#' temps <- c(-5, 0, 23, 38, 40)
#' discretize(temps, level = c("-0", "0-30", ">30"),
#'            labels = c("Freezing", "Mild", "Hot"))
#'
#' # Example 4: Default level expressions as labels
#' discretize(c(55, 71, 86, 94), level = c("<60", "60-79", ">=80"))
#'
#' # Example 5: Overlapping levels trigger a warning
#' discretize(c(10, 20, 30, 40), level = c(">15", ">25"),
#'            labels = c("Above 15", "Above 25"))
#'
#' @export
discretize <- function(data, varname, level, labels = NULL, new_varname = NULL) {



  level <- cc(level)

  if(!is.null(labels)){
    labels <- cc(labels)
  }

  # Determine input type
  is_vector_input <- is.numeric(data)
  is_df_input <- is.data.frame(data)

  if (!is_vector_input && !is_df_input) {
    stop("Input must be a vector or a data.frame")
  }

  # Extract target vector
  if (is_vector_input) {
    x <- data
  } else {
    varname <- select_variable(data, varname)
    x <- data[[varname]]
  }

  # Input validation
  if (!is.numeric(x)) {
    stop("Variable must be numeric")
  }
  if (length(level) < 1) {
    stop("At least one level is required")
  }
  if (!is.null(labels) && length(labels) != length(level)) {
    stop("Length of labels must match length of level")
  }

  # Parse level expressions
  parse_level <- function(expr) {
    expr <- gsub(" ", "", expr)  # Remove spaces

    if (grepl("^>=", expr)) {
      list(type = "ge", value = as.numeric(gsub(">=", "", expr)))
    } else if (grepl("^>", expr)) {
      list(type = "gt", value = as.numeric(gsub(">", "", expr)))
    } else if (grepl("^<=", expr)) {
      list(type = "le", value = as.numeric(gsub("<=", "", expr)))
    } else if (grepl("^<", expr)) {
      list(type = "lt", value = as.numeric(gsub("<", "", expr)))
    } else if (grepl("^-", expr)) {
      # Left unbounded: "-10" means <= 10
      list(type = "le", value = as.numeric(gsub("-", "", expr)))
    } else if (grepl("-$", expr)) {
      # Right unbounded: "10-" means >= 10
      list(type = "ge", value = as.numeric(gsub("-", "", expr)))
    } else if (grepl("-", expr)) {
      parts <- strsplit(expr, "-")[[1]]
      if (length(parts) == 2) {
        list(type = "range", lower = as.numeric(parts[1]), upper = as.numeric(parts[2]))
      } else {
        stop(paste("Invalid range expression:", expr))
      }
    } else {
      stop(paste("Unrecognized expression:", expr))
    }
  }

  # Parse all levels
  parsed_levels <- lapply(level, parse_level)

  # Check for potential overlaps
  check_overlap <- function(parsed_levels) {
    for (i in seq_along(parsed_levels)) {
      p1 <- parsed_levels[[i]]
      for (j in seq_along(parsed_levels)) {
        if (i < j) {
          p2 <- parsed_levels[[j]]

          # Simple overlap detection for range types
          if (p1$type == "range" && p2$type == "range") {
            if (p1$lower <= p2$upper && p2$lower <= p1$upper) {
              warning(paste("Potential overlap between levels", i, "and", j))
            }
          }

          # Check inequality vs range overlaps
          if (p1$type %in% c("ge", "gt") && p2$type == "range") {
            val <- ifelse(p1$type == "ge", p1$value, p1$value + 0.001)
            if (val <= p2$upper) {
              warning(paste("Potential overlap between levels", i, "and", j))
            }
          }

          if (p1$type %in% c("le", "lt") && p2$type == "range") {
            val <- ifelse(p1$type == "le", p1$value, p1$value - 0.001)
            if (val >= p2$lower) {
              warning(paste("Potential overlap between levels", i, "and", j))
            }
          }
        }
      }
    }
  }

  check_overlap(parsed_levels)

  # Use original levels as labels if not provided
  if (is.null(labels)) {
    labels <- level
  }

  # Initialize result vector
  result <- rep(NA, length(x))

  # Classify each level sequentially
  for (i in seq_along(parsed_levels)) {
    condition <- switch(parsed_levels[[i]]$type,
                        "ge" = x >= parsed_levels[[i]]$value,
                        "gt" = x > parsed_levels[[i]]$value,
                        "le" = x <= parsed_levels[[i]]$value,
                        "lt" = x < parsed_levels[[i]]$value,
                        "range" = x >= parsed_levels[[i]]$lower & x <= parsed_levels[[i]]$upper
    )

    # Assign label only to unclassified values that meet the condition
    result[condition & is.na(result)] <- labels[i]
  }

  # Convert to factor
  result <- factor(result, levels = labels)

  # Return based on input type
  if (is_vector_input) {
    return(result)
  } else {
    # data.frame input: determine target column name
    target_col <- ifelse(is.null(new_varname), varname, new_varname)
    data[[target_col]] <- result
    return(data)
  }
}
