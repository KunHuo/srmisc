#' Color palettes of AAAS journal
#'
#' @param n choose the color numbers.
#' @param alpha transparency level, a real number in (0, 1].
#'
#' @return A character vectors of hex color.
#' @export
#'
#' @examples
#' pal_aaas_10()
#' show_colors(pal_aaas_10())
pal_aaas_10 <- function(n = 1:10, alpha = 1){

  cols <- c("#3B4992FF", "#EE0000FF", "#008B45FF", "#631879FF", "#008280FF",
            "#BB0021FF", "#5F559BFF", "#A20056FF", "#808180FF", "#1B1919FF")
  pals(cols = cols, n = n, alpha = alpha)
}


#' Color palettes of JAMA journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @examples
#' pal_jama_7()
#' show_colors(pal_jama_7())
pal_jama_7 <- function(n = 1:7, alpha = 1){
  cols <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF",
            "#6A6599FF", "#80796BFF")
  pals(cols = cols, n = n, alpha = alpha)
}


#' Color palettes of JCO journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @examples
#' pal_jco_10()
#' show_colors(pal_jco_10())
pal_jco_10 <- function(n = 1:10, alpha = 1){
  cols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF",
            "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF")
  pals(cols = cols, n = n, alpha = alpha)
}


#' Color palettes of NEJM journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @examples
#' pal_nejm_8()
#' show_colors(pal_jco_10())
pal_nejm_8 <- function(n = 1:8, alpha = 1){
  cols <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF",
            "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF")
  pals(cols = cols, n = n, alpha = alpha)
}


#' Color palettes of lancet journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @examples
#' pal_lancet_9()
#' show_colors(pal_lancet_9())
pal_lancet_9 <- function(n = 1:9, alpha = 1){
  cols <- c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", "#925E9FFF",
           "#FDAF91FF", "#AD002AFF", "#ADB6B6FF", "#1B1919FF")
  pals(cols = cols, n = n, alpha = alpha)
}


#' Set color alpha
#'
#' @param cols color.
#' @param alpha alpha.
#'
#' @return a string vector.
#' @export
set_alpha <- function(cols, alpha = 1){
  output <- sapply(cols, function(x){
    r <- grDevices::col2rgb(x, alpha = TRUE)
    grDevices::rgb(red = r[1, 1],
        green = r[2, 1],
        blue = r[3, 1],
        alpha = r[4, 1] * alpha,
        maxColorValue = 255)
  })
  names(output) <- NULL
  output
}


pals <- function(cols, n, alpha = 1){
  if(alpha < 0 | alpha > 1){
    stop("Transparency level, a real number in (0, 1].")
  }
  cols <- cols[n]
  set_alpha(cols = cols, alpha = alpha)
}


#' Show colors
#'
#' @description
#' A quick and dirty way to show colors in a plot.
#'
#' @param colors a character vector of colors.
#'
#' @return No return value.
#' @export
#'
#' @examples
#' show_colors(pal_jama_7())
show_colors <- function(colors){
  colors <- rev(colors)
  df     <- data.frame(y = colors, x = stats::runif(length(colors), min = 5, max = 10))
  df$y   <- factor(df$y, levels = colors)

  ggplot2::ggplot(df) +
    ggplot2::geom_col(ggplot2::aes_string(x = "x", y = "y", fill = "y"),
                      show.legend = FALSE,
                      width = 0.7) +
    ggplot2::scale_fill_manual(values = colors) +
    gg_theme_sci(plot.margin = rep(1.5, 4)) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())
}
