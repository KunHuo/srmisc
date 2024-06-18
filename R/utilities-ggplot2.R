#' ggplo2 theme for SCI
#'
#' @param font.size font size, default 12,
#' @param font.family font family, detault 'serif' (Times New Roman).
#' @param axis.line.size axis line size, default 0.25.
#' @param axis.ticks.length axis ticks length, default 0.12.
#' @param legend.key.size legend key size, dsfault 1.
#' @param face.bold a logical, whether bold the title of axis, plot, strip, and
#' legend, default FALSE.
#' @param panel.border a logical, whether plot panel border, default FALSE.
#' @param aspect.ratio the ratio of the width to the height, default NULL.
#' @param panel.grid.major panel grid major.
#' @param panel.grid.minor panel grid minor.
#' @param panel.spacing panel spacing.
#' @param strip.background strip background.
#' @param plot.margin plot margin, top, right, bottom, left.
#' @param ... further arguments pass to the [ggplot2::theme()] function.
#'
#' @return A theme of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'  geom_density(aes(x = price, fill = color)) +
#'  gg_theme_sci()
gg_theme_sci <- function(font.size = 12,
                         font.family = "serif",
                         axis.line.size = 0.25,
                         axis.ticks.length = 0.12,
                         legend.key.size = 1.0,
                         face.bold = FALSE,
                         panel.grid.major = FALSE,
                         panel.grid.minor = FALSE,
                         panel.border = FALSE,
                         panel.spacing = 0.6,
                         strip.background = "gray90",
                         aspect.ratio = NULL,
                         plot.margin = c(0.4, 0.6, 0.4, 0.4),
                         ...) {

  face <- ifelse(face.bold, "bold", "plain")

  if(panel.grid.major){
    pg.major = ggplot2::element_line(color = "gray90", size = axis.line.size)
  }else{
    pg.major = ggplot2::element_blank()
  }

  if(panel.grid.minor){
    pg.minor = ggplot2::element_line(color = "gray90", size = axis.line.size, linetype = "dashed")
  }else{
    pg.minor = ggplot2::element_blank()
  }

  if(panel.border){
    pborder = ggplot2::element_rect(color = "black", size = axis.line.size)
  }else{
    pborder = ggplot2::element_rect(color = "NA")
  }

  ggplot2::theme_bw(
    base_size   = font.size,
    base_family = font.family,
    base_line_size = axis.line.size,
    base_rect_size = axis.line.size) +

    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA),
      panel.grid = ggplot2::element_blank(),
      panel.border = pborder,
      panel.grid.major = pg.major,
      panel.grid.minor = pg.minor,
      panel.spacing = ggplot2::unit(panel.spacing, "cm"),

      strip.background = ggplot2::element_rect(fill = strip.background, size = axis.line.size),

      axis.line = ggplot2::element_line(size = axis.line.size, color = "black",lineend = "square"),
      axis.ticks.length = ggplot2::unit(axis.ticks.length, "cm"),
      axis.ticks = ggplot2::element_line(color = "black", size = axis.line.size),
      axis.text  = ggplot2::element_text(color = "black", size = font.size),
      axis.title = ggplot2::element_text(color = "black", size = font.size, face = face),

      legend.background = ggplot2::element_rect(fill = "NA"),
      legend.text       = ggplot2::element_text(color = "black", size = font.size),
      legend.title      = ggplot2::element_text(face = face),
      legend.key.size   = ggplot2::unit(legend.key.size, "lines"),

      plot.title = ggplot2::element_text(size = font.size + 2, face = face),
      plot.title.position = "plot",
      plot.margin = ggplot2::unit(plot.margin, "cm"), # top, right, bottom, left

      strip.text = ggplot2::element_text(color = "black", size = font.size, face = face),
      aspect.ratio = aspect.ratio,
      complete = FALSE,
      ...
    )
}


.is_waiver <- function(value){
  class(value) == "waiver"
}


#' Bold axis title for ggplot2
#'
#' @param bold a logical indicating whether to bold the title of the axis,
#' default TRUE.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_bold_axis_title()
gg_bold_axis_title <- function(bold = TRUE){
  face <- ifelse(bold, "bold", "plain")
  ggplot2::theme(
    axis.title = ggplot2::element_text(face = face)
  )
}


#' Bold tags for ggplot2
#'
#' @param bold a logical indicating whether to bold the tags, default TRUE.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_tags("A")
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_tags("A") +
#'   gg_bold_tags()
gg_bold_tags <- function(bold = TRUE){
  face <- ifelse(bold, "bold", "plain")
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = face)
  )
}


#' Set axis line size for ggplot2
#'
#' @param size line size, default 0.25.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_axis_line_size(2)
gg_axis_line_size <- function(size = 0.25){
  ggplot2::theme(
    axis.line  = ggplot2::element_line(size = size),
    axis.ticks = ggplot2::element_line(size = size)
  )
}


#' Set axis ticks length for ggplot2
#'
#' @param size axis ticks length, default 0.15.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_axis_ticks_length(2)
gg_axis_ticks_length <- function(size = 0.15){
  ggplot2::theme(
    axis.ticks.length = ggplot2::unit(size, "cm"),
  )
}


#' Set legend title for ggplot2
#'
#' @param value legend title, NULL indicates deletion.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_legend_title(NULL)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_legend_title("Colors of diamonds")
gg_legend_title <- function(value = NULL){
  if(length(value) == 0L){
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )
  }else{
    ggplot2::labs(color = value, fill = value, alpha = value, size = value, linetype = value)
  }
}


#' Set legend position for ggplot2
#'
#' @param position legend position, NULL for indicates deletion, or a character
#' of 'top', 'bottom', 'left' and 'right', or numeric vector (between 0 and 1)
#' of length two (x-axis and y-axis).
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_legend_position(NULL)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_legend_position("top")
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_legend_position(c(0.9, 0.9))
gg_legend_position <- function(position = NULL) {
  if(length(position) == 0L){
    ggplot2::theme(
      legend.position = "none"
    )
  }else{
    if(is.character(position)){
      ggplot2::theme(
        legend.position = position
      )
    }else{
      ggplot2::theme(
        legend.position = position,
        legend.justification = position
      )
    }
  }
}


#' Rotate X axis text for ggplot2
#'
#' @param angle angle.
#' @param hjust hjust.
#' @param vjust vjust.
#' @param ... unused.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [gg_rotate_y_text()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_rotate_x_text(45)
gg_rotate_x_text <- function (angle = 45, hjust = NULL, vjust = NULL, ...) {
  if (missing(hjust) & angle > 5)
    hjust <- 1
  if (missing(vjust) & angle == 90)
    vjust <- 0.5
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


#' Rotate Y axis text for ggplot2
#'
#' @param angle angle.
#' @param hjust hjust.
#' @param vjust vjust.
#' @param ... unused.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [gg_rotate_x_text()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color))
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_rotate_y_text(45)
gg_rotate_y_text <- function (angle = 45, hjust = NULL, vjust = NULL, ...) {
  if (missing(hjust) & angle == 90)
    hjust <- 0.5
  else if (missing(hjust) & angle > 5)
    hjust <- 1
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


#' Delete x title of ggplot2
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [gg_delete_y_title()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_delete_x_title()
gg_delete_x_title <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )
}


#' Delete y title of ggplot2
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [gg_delete_x_title()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_delete_y_title()
gg_delete_y_title <- function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank()
  )
}


#' Delete legend title of ggplot2
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [gg_delete_legend()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_delete_legend_title()
gg_delete_legend_title <- function() {
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )
}


#' Delete legend of ggplot2
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [gg_delete_legend_title()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_delete_legend()
gg_delete_legend <- function() {
  ggplot2::theme(
    legend.position = "none"
  )
}


#' Set tags for ggplt2
#'
#' @param tag tag.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_tags("A")
gg_tags <- function(tag){
  ggplot2::labs(title = tag)
}


#' Modify x-axis labels
#'
#' @param label the title of the respective axis.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [ggplot2::xlab()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_xlab("Price of diamonds")
gg_xlab <- function(label){
  ggplot2::xlab(label = label)
}


#' Modify y-axis labels
#'
#' @param label the title of the respective axis.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [ggplot2::ylab()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_ylab("Density")
gg_ylab <- function(label){
  ggplot2::ylab(label = label)
}


#' xbreaks for continuous of ggplot2
#'
#' @param breaks a numeric vector of positions.
#' @param expand if TRUE, adds a small expansion factor to the limits to ensure
#' that data and axes don't overlap. If FALSE, the default, limits are taken
#' exactly from the data or xlim.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [ggplot2::scale_x_continuous()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_xbreaks_continuous(breaks = seq(0, 20000, 5000), expand = FALSE)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_xbreaks_continuous(breaks = seq(0, 20000, 5000), expand = TRUE)
gg_xbreaks_continuous <- function(breaks, expand = FALSE){
  if(expand){
    ggplot2::scale_x_continuous(breaks = breaks,
                                limits = c(min(breaks), max(breaks)))
  }else{
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = breaks,
                                limits = c(min(breaks), max(breaks)))
  }
}


#' ybreaks for continuous of ggplot2
#'
#' @param breaks a numeric vector of positions.
#' @param expand if TRUE, adds a small expansion factor to the limits to ensure
#' that data and axes don't overlap. If FALSE, the default, limits are taken
#' exactly from the data or ylim.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @seealso [ggplot2::scale_y_continuous()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_ybreaks_continuous(breaks = seq(0, 5e-04, 1e-04), expand = FALSE)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_ybreaks_continuous(breaks = seq(0, 5e-04, 1e-04), expand = TRUE)
gg_ybreaks_continuous <- function(breaks, expand = FALSE){
  if(expand){
    ggplot2::scale_y_continuous(breaks = breaks,
                                limits = c(min(breaks), max(breaks)))
  }else{
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                breaks = breaks,
                                limits = c(min(breaks), max(breaks)))
  }
}


#' Set aspect ratio of the panel for ggplot2
#'
#' @description
#' The ratio represents the number of units on the y-axis equivalent to one unit
#' on the x-axis.
#'
#' @param ratio aspect ratio.
#'
#' @return An obejct of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_aspect_ratio(ratio = 1)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_aspect_ratio(ratio = 0.8)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_aspect_ratio(ratio = 1.5)
gg_aspect_ratio <- function(ratio = NULL){
  ggplot2::theme(aspect.ratio = ratio)
}


#' Draw a text label or mathematical expression for ggplot2
#'
#' @description This function can draw either a character string or mathematical
#' expression at the given coordinates.
#'
#' @param label string or plotmath expression to be drawn.
#' @param x the x location (origin) of the label.
#' @param y the y location (origin) of the label.
#' @param hjust horizontal justification. Default = 0.5 (flush-left at x),
#' 0.5 = centered on x, 1 = flush-right.
#' @param vjust Vertical justification. Default = 1 (ascender at y). 0 = baseline
#' at y, 0.5 = centered on y.
#' @param fontfamily the font family,default 'serif' (Times New Roman).
#' @param fontface the font face ("plain", "bold", etc.)
#' @param color text color.
#' @param size point size of text.
#' @param angle angle at which text is drawn.
#' @param lineheight line height of text.
#' @param alpha the alpha value of the text.
#'
#' @return An object of ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   geom_density(aes(x = price, fill = color)) +
#'   gg_draw_label(label = "Distribution of price by color",
#'                 x = 5000,
#'                 y = 3e-04)
gg_draw_label <- function (label,
                           x = 0.5,
                           y = 0.5,
                           hjust = 0,
                           vjust = 1,
                           fontfamily = "serif",
                           fontface = "plain",
                           color = "black",
                           size = 12,
                           angle = 0,
                           lineheight = 0.9,
                           alpha = 1) {
  text_par <- grid::gpar(col = color,
                         fontsize   = size,
                         fontfamily = fontfamily,
                         fontface   = fontface,
                         lineheight = lineheight,
                         alpha      = alpha)
  text.grob <- grid::textGrob(label,
                              x = grid::unit(0.5, "npc"),
                              y = grid::unit(0.5, "npc"),
                              hjust = hjust,
                              vjust = vjust,
                              rot   = angle,
                              gp    = text_par)
  ggplot2::annotation_custom(text.grob,
                             xmin = x,
                             xmax = x,
                             ymin = y,
                             ymax = y)
}


#' Save a ggplot (or other grid object) with sensible defaults
#'
#' @param plot Plot to save, defaults to last plot displayed.
#' @param path Path to save plot to (combined with file name).
#' @param width Width, default 8.3.
#' @param height Height.
#' @param units Units, default cm.
#' @param dpi DPI to use for raster graphics.
#' @param aspect.ratio aspect ratio of the panel.
#' @param ... Other arguments passed on to the graphics device function,
#' as specified by device.
#'
#' @return No return value.
#' @export
gg_save <- function(plot = ggplot2::last_plot(),
                    path,
                    width = 8.3,
                    height = width / 8 * 7,
                    units = "cm",
                    dpi = 300,
                    aspect.ratio,
                    ...) {

  if(!missing(aspect.ratio)){
    plot <- ggplot2::theme(aspect.ratio = aspect.ratio)
  }

   ggplot2::ggsave(
     filename = path,
     plot = plot,
     width = width,
     height = height,
     units = units,
     dpi = dpi,
     ...)
}



find_col_x <- function(plot, col = NULL){
  data <- ggplot2::ggplot_build(plot)$data[[1]]
  x <- data[["x"]]
  x <- sort(x)
  if(is.null(col)){
    attr(x, "class") <- NULL
    x
  }else{
    attr(x, "class") <- NULL
    x[col]
  }
}


find_col_y <- function(plot, col){
  data <- ggplot2::ggplot_build(plot)$data
  x <- find_col_x(plot, col)
  sapply(x, \(i){
    if(is.na(i)){
      NA
    }else{
      max(sapply(data, \(d){
        if("ymax" %in% names(d)){
          d <- d[d$x == i, "ymax"]
          if(length(d) == 0L){
            NA
          }else{
            max(d, na.rm = TRUE)
          }
        }else if("y" %in% names(d)){
          d <- d[d$x == i, "y"]
          if(length(d) == 0L){
            NA
          }else{
            max(d, na.rm = TRUE)
          }
        }
      }), na.rm = TRUE)
    }
  })
}


add_col_label <- function(plot, col, y = NULL, label, vjust = -0.5, family = "serif", size = 10, color = "black"){

  x <- find_col_x(plot = plot, col = col)

  if(is.null(y)){
    y <- find_col_y(plot = plot, col = col)
  }

  plot +
    ggplot2::annotate(geom = "text",
                      x = x,
                      y = y,
                      label = label,
                      vjust = vjust,
                      family = family,
                      size = size / 2.875,
                      color = color)
}


#' Add tags for ggplot
#'
#' @param plots a list of ggplot object.
#' @param tags tags.
#'
#' @return a list of ggplot object
#' @export
add_tags <- function(plots, tags = NULL){
  if(is.null(tags)){
    tags <- LETTERS[1:length(plots)]
  }
  Map(function(p, tag){
    p + gg_tags(tag)
  }, plots, tags)
}



#' Wrap plots into a patchwork
#'
#' @param ... multiple ggplots or a list containing ggplot objects.
#' @param ncol the dimensions of the grid to create - if both are NULL it will
#' use the same logic as facet_wrap() to set the dimensions
#' @param nrow the dimensions of the grid to create - if both are NULL it will
#' use the same logic as facet_wrap() to set the dimensions
#' @param byrow analogous to byrow in matrix(). If FALSE the plots will be
#' filled in in column-major order
#' @param widths the relative widths of each column and row in the grid. Will
#' get repeated to match the dimensions of the grid.
#' @param heights the relative heights of each column and row in the grid. Will
#' get repeated to match the dimensions of the grid.
#' @param tags tags.
#' @param guides A string specifying how guides should be treated in the layout.
#'  'collect' will collect guides below to the given nesting level, removing duplicates.
#'  'keep' will stop collection at this level and let guides be placed alongside
#'  their plot. auto will allow guides to be collected if a upper level tries,
#'  but place them alongside the plot if not. If you modify default guide
#'  "position" with theme(legend.position=...) while also collecting guides you
#'  must apply that change to the overall patchwork (see example).
#' @param design Specification of the location of areas in the layout. Can either
#' be specified as a text string or by concatenating calls to area() together.
#' See the examples for further information on use.
#' @param bold.tags bold tags.
#' @param plot.margin plot margin.
#'
#' @return a patchwork object.
#' @export
wrap_plots2 <- function(...,
                        ncol = NULL,
                        nrow = NULL,
                        byrow = NULL,
                        widths = NULL,
                        heights = NULL,
                        tags = NULL,
                        bold.tags = FALSE,
                        plot.margin = NULL,
                        guides = NULL,
                        design = NULL){
  plots <- list(...)
  plots <- list_flatten(plots)
  plots <- add_tags(plots, tags)

  plots <- lapply(plots, \(x){
    if(x$coordinates$clip == "on"){
      x <- x + ggplot2::coord_cartesian(clip = "off")
    }
    x
  })


  plots <- patchwork::wrap_plots(plots,
                        ncol = ncol,
                        nrow = nrow,
                        byrow = byrow,
                        widths = widths,
                        heights = heights,
                        guides = guides,
                        design = design)

  # plots <- plots & ggplot2::coord_cartesian(clip = "off")

  if(bold.tags){
    plots <- plots & gg_bold_tags()
  }

  if(!is.null(plot.margin)){
    plots <-  plots & ggplot2::theme(plot.margin = ggplot2::unit(plot.margin, "cm"))
  }

  plots
}


#' Align multiple plots vertically and/or horizontally
#'
#' @param ... List of plots to be aligned.
#' @param align (optional) Specifies whether graphs in the grid should be
#' horizontally ("h") or vertically ("v") aligned. Options are align="none",
#' "hv" (align in both directions)(default), "h", and "v".
#' @param axis (optional) Specifies whether graphs should be aligned by the
#' left ("l"), right ("r"), top ("t"), or bottom ("b") margins. Options are
#' axis="none" (default), or a string of any combination of "l", "r", "t",
#' and/or "b" in any order (e.g. axis="tblr" or axis="rlbt" for aligning all margins)
#' @param nrow (optional) Number of rows in the plot grid.
#' @param ncol (optional) Number of columns in the plot grid.
#' @param rel_widths (optional) Numerical vector of relative columns widths. For
#'  example, in a two-column grid, rel_widths = c(2, 1) would make the first
#'  column twice as wide as the second column.
#' @param rel_heights (optional) Numerical vector of relative rows heights.
#' Works just as rel_widths does, but for rows rather than columns.
#' @param labels (optional) List of labels to be added to the plots. You can
#' also set labels="AUTO" to auto-generate upper-case labels or labels="auto"
#' to auto-generate lower-case labels.
#' @param label_size (optional) Numerical value indicating the label size.
#' Default is 14.
#' @param label_fontfamily (optional) Font family of the plot labels. If not
#' provided, is taken from the current theme.
#' @param label_fontface (optional) Font face of the plot labels. Default is
#' "plain".
#' @param label_colour (optional) Color of the plot labels. If not provided, is
#' taken from the current theme.
#' @param label_x (optional) Single value or vector of x positions for plot
#' labels, relative to each subplot. Defaults to 0 for all labels. (Each label
#' is placed all the way to the left of each plot.)
#' @param label_y (optional) Single value or vector of y positions for plot
#' labels, relative to each subplot. Defaults to 1 for all labels. (Each label
#' is placed all the way to the top of each plot.)
#' @param hjust Adjusts the horizontal position of each label. More negative
#' values move the label further to the right on the plot canvas. Can be a single
#' value (applied to all labels) or a vector of values (one for each label).
#' @param vjust Adjusts the vertical position of each label. More positive values
#' move the label further down on the plot canvas. Can be a single value
#' (applied to all labels) or a vector of values (one for each label).
#' @param scale Individual number or vector of numbers greater than 0. Enables
#' you to scale the size of all or select plots. Usually it's preferable to set
#' margins instead of using scale, but scale can sometimes be more powerful.
#' @param greedy (optional) Defines the alignment policy when alignment axes are
#' specified via the axis option. greedy = TRUE tries to always align by
#' adjusting the outmost margin. greedy = FALSE aligns all columns/rows in the
#' gtable if possible.
#' @param byrow Logical value indicating if the plots should be arrange by row
#' (default) or by column.
#'
#' @return list plots.
#' @export
wrap_plots3 <- function(...,
                        align = c("hv", "h", "v", "none"),
                        axis = c("l", "r", "t", "b", "lr", "tb", "tblr", "none"),
                        nrow = NULL,
                        ncol = NULL,
                        rel_widths = 1,
                        rel_heights = 1,
                        labels = "AUTO",
                        label_size = 14,
                        label_fontfamily = "serif",
                        label_fontface = "plain",
                        label_colour = NULL,
                        label_x = 0,
                        label_y = 1,
                        hjust = -1,
                        vjust = 1.2,
                        scale = 1,
                        greedy = TRUE,
                        byrow = TRUE){

  plots <- list(...)
  plots <- list_flatten(plots)

  align <- match.arg(align)
  axis <- match.arg(axis)

  cowplot::plot_grid(
    plotlist = plots,
    align = align,
    axis = axis,
    nrow = nrow,
    ncol = ncol,
    rel_widths = rel_widths,
    rel_heights = rel_heights,
    labels = labels,
    label_size = label_size,
    label_fontfamily = label_fontfamily,
    label_fontface = label_fontface,
    label_colour = label_colour,
    label_x = label_x,
    label_y = label_y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    greedy = greedy,
    byrow = byrow
  )
}


#' Customize ggplot2 Text Elements
#'
#' This function customizes text elements (such as titles, labels, captions,
#' and axis text) in ggplot2 plots.
#'
#' @param which A character string specifying which text element to customize.
#' Possible values include "title", "subtitle", "caption", "x", "xlab",
#' "x.title", "y", "ylab", "y.title", "xy", "xylab", "xy.title", "axis.title",
#' "legendtitle", "legend.title", "legendtext", "legend.text", "x.text", "y.text",
#' "xy.text", "yxtext", and "axis.text".
#' @param size The size of the text. Default is 12.
#' @param color The color of the text. Default is "black".
#' @param face The font face of the text (e.g., "bold", "italic"). Default is NULL.
#' @param family The font family of the text. Default is NULL.
#' @param ... Additional arguments to be passed to ggplot2's 'element_text' function.
#'
#' @return A ggplot2 theme with customized text element(s).
#'
#' @export
gg_font <- function(which = "all", size = 12, color = "black", face = NULL, family = NULL, ...){

  elmt <- ggplot2::element_text(size = size, color = color, face = face, family = family, ...)

    switch(which,
           title = ggplot2::theme(plot.title = elmt),
           subtitle = ggplot2::theme(plot.subtitle = elmt),
           caption = ggplot2::theme(plot.caption = elmt),
           x = ggplot2::theme(axis.title.x = elmt),
           xlab = ggplot2::theme(axis.title.x = elmt),
           x.title = ggplot2::theme(axis.title.x = elmt),
           y = ggplot2::theme(axis.title.y = elmt),
           ylab = ggplot2::theme(axis.title.y = elmt),
           y.title = ggplot2::theme(axis.title.y = elmt),
           xy = ggplot2::theme(axis.title.x = elmt, axis.title.y = elmt),
           xylab = ggplot2::theme(axis.title.x = elmt, axis.title.y = elmt),
           xy.title = ggplot2::theme(axis.title.x = elmt, axis.title.y = elmt),
           axis.title = ggplot2::theme(axis.title.x = elmt, axis.title.y = elmt),
           legendtitle = ggplot2::theme(legend.title = elmt),
           legend.title = ggplot2::theme(legend.title = elmt),
           legendtext = ggplot2::theme(legend.text = elmt),
           legend.text = ggplot2::theme(legend.text = elmt),
           x.text = ggplot2::theme(axis.text.x = elmt),
           y.text = ggplot2::theme(axis.text.y = elmt),
           xy.text = ggplot2::theme(axis.text.x = elmt, axis.text.y = elmt),
           yxtext = ggplot2::theme(axis.text.x = elmt, axis.text.y = elmt),
           axis.text = ggplot2::theme(axis.text.x = elmt, axis.text.y = elmt),
           all = ggplot2::theme(
             plot.title = elmt,
             plot.subtitle = elmt,
             plot.caption = elmt,
             axis.title.x = elmt,
             axis.title.y = elmt,
             legend.title = elmt,
             legend.text = elmt,
             axis.text.x = elmt,
             axis.text.y = elmt),
           stop("Don't support ", which))
 }


#' Set Line Properties
#'
#' This function sets properties (such as width, type, and color) for lines in
#' a ggplot2 plot.
#'
#' @param plot The ggplot object to which line properties will be applied.
#' @param width The width of the lines.
#' @param type The type of the lines.
#' @param colour The color of the lines.
#'
#' @return The ggplot object with updated line properties.
#'
#' @export
set_line <- function(plot, width = NULL, type = NULL, colour = NULL){

  p <- ggplot2::ggplot_build(plot)

  for(i in 1:length(p$data)){
    if("linewidth" %in% names(p$data[[i]]) & "linetype" %in% names(p$data[[i]])){
      if(!is.null(width)){
        p$data[[i]]$linewidth <- width
      }

      if(!is.null(type)){
        p$data[[i]]$linetype <- type
      }

      if(!is.null(colour)){
        p$data[[i]]$colour <- colour

        # if("fill" %in% names(p$data[[i]])){
        #   p$data[[i]]$fill <- colour
        # }
      }
    }
  }

  p <- ggplot2::ggplot_gtable(p)
  class(p) <- c("gtable2", class(p))
  p
}


#' Set Point Properties
#'
#' This function sets properties (such as size, shape, fill, alpha, and stroke)
#' for points in a ggplot2 plot.
#'
#' @param plot The ggplot object to which point properties will be applied.
#' @param size The size of the points.
#' @param shape The shape of the points.
#' @param colour The color of the points.
#' @param alpha The transparency level of the points.
#' @param stroke The stroke color of the points.
#'
#' @return The ggplot object with updated point properties.
#'
#' @export
set_point <- function(plot, size = NULL, shape = NULL, colour = NULL, alpha = NULL, stroke = NULL){

  p <- ggplot2::ggplot_build(plot)

  for(i in 1:length(p$data)){
    if("shape" %in% names(p$data[[i]]) & "stroke" %in% names(p$data[[i]])){
      if(!is.null(size)){
        p$data[[i]]$size <- size
      }

      if(!is.null(shape)){
        p$data[[i]]$shape <- shape
      }

      if(!is.null(colour)){
        p$data[[i]]$colour <- colour
      }

      if(!is.null(alpha)){
        p$data[[i]]$alpha <- alpha
      }

      if(!is.null(stroke)){
        p$data[[i]]$stroke <- stroke
      }
    }
  }

  p <- ggplot2::ggplot_gtable(p)
  class(p) <- c("gtable2", class(p))
  p
}


#' Print Method for gtable2 Objects
#'
#' This function defines the print method for gtable2 objects, allowing them to
#' be printed as plots.
#'
#' @param x The gtable object to be printed.
#' @param ... Additional arguments passed to the plot function.
#'
#' @keywords internal
#'
#' @export
print.gtable2 <- function(x, ...){
  plot(x, ...)
}
