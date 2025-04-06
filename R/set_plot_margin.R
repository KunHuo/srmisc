#' Set Plot Margins for ggplot2
#'
#' This function provides a convenient way to modify the plot margins of a ggplot2 object.
#' It allows specifying new margin values for any side while preserving existing values for
#' unspecified sides.
#'
#' @param top Numeric value for the top margin (in units specified by `unit` parameter).
#'            If NULL (default), keeps the current top margin.
#' @param right Numeric value for the right margin. If NULL (default), keeps the current right margin.
#' @param bottom Numeric value for the bottom margin. If NULL (default), keeps the current bottom margin.
#' @param left Numeric value for the left margin. If NULL (default), keeps the current left margin.
#' @param unit Character string specifying the unit of measurement for the margins.
#'             Default is "pt" (points). Other options include "cm", "in", etc.
#'
#' @return A ggplot2 theme object with modified plot margins.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Create a plot with default margins
#' p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
#'
#' # Modify top and bottom margins only
#' p + set_plot_margin(top = 20, bottom = 15)
#'
#' # Change all margins to 1cm
#' p + set_plot_margin(10, 10, 10, 10, "cm")
#' }
#'
#' @importFrom ggplot2 theme theme_get unit
#' @export
set_plot_margin <- function(top = NULL, right = NULL, bottom = NULL, left = NULL, unit = "pt") {

  current_margin <- if (is.null(ggplot2::theme_get()$plot.margin)) {
    grid::unit(rep(5.5, 4), "pt")
  } else {
    ggplot2::theme_get()$plot.margin
  }

  new_values <- c(
    if (!is.null(top)) top else as.numeric(current_margin[[1]]),
    if (!is.null(right)) right else as.numeric(current_margin[[2]]),
    if (!is.null(bottom)) bottom else as.numeric(current_margin[[3]]),
    if (!is.null(left)) left else as.numeric(current_margin[[4]])
  )

  ggplot2::theme(plot.margin = grid::unit(new_values, unit))
}
