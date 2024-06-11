#' Display Line Types
#'
#' This function generates a ggplot displaying various line types available in ggplot2.
#' It shows the line types "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash".
#'
#' @return A ggplot object displaying different line types.
#' @export
#'
#' @examples
#' show_linetypes()
show_linetypes <- function () {
  lt <- c("solid", "dashed", "dotted", "dotdash",  "longdash", "twodash")
  d <- data.frame(lt = factor(lt, levels = rev(lt)))

  ggplot2::ggplot() + ggplot2::scale_x_continuous(name = "", limits = c(0, 1), breaks = NULL) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_segment(data = d,  mapping = ggplot2::aes(x = 0, xend = 1, y = .data$lt, yend = .data$lt, linetype = .data$lt), linewidth = 0.5) +
    ggplot2::labs(y = "") +
    gg_theme_sci() +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

}

