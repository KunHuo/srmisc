#' Display Point Shapes
#'
#' This function generates a ggplot displaying various point shapes available in ggplot2.
#' It shows point shapes from 0 to 25, with each shape labeled by its corresponding number.
#'
#' @return A ggplot object displaying different point shapes.
#' @export
#'
#' @examples
#' show_point_shapes()
show_point_shapes <- function() {
  d = data.frame(p = c(0:25))
  ggplot2::ggplot() +
    ggplot2::scale_shape_identity() +
    ggplot2::geom_point(data = d, mapping = ggplot2::aes(x = .data$p%%6,  y = .data$p%/%6, shape = .data$p), size = 5, fill = pal_nejm_8(1)) +
    ggplot2::geom_text(data = d, mapping = ggplot2::aes(x = .data$p%%6, y = .data$p%/%6 + 0.25, label = .data$p), size = 3) +
    ggplot2::scale_y_reverse() +
    gg_theme_sci() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    gg_delete_x_title() +
    gg_delete_y_title()
}


list("A", "BB", "last", "CCC")
