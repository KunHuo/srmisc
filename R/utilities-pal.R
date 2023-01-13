#' Color palettes of AAAS journal
#'
#' @param n choose the color numbers.
#' @param alpha transparency level, a real number in (0, 1].
#'
#' @return A character vectors of hex color.
#' @export
#'
#' @seealso [ggsci::scale_color_aaas()], [ggsci::scale_fill_aaas()]
#'
#' @examples
#' pal_aaas_10()
#' show_colors(pal_aaas_10())
pal_aaas_10 <- function(n = 1:10, alpha = 1){
  ggsci::pal_aaas(alpha = alpha)(10)[n]
}


#' Color palettes of JAMA journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @seealso [ggsci::scale_color_jama()], [ggsci::scale_fill_jama()]
#'
#' @examples
#' pal_jama_7()
#' show_colors(pal_jama_7())
pal_jama_7 <- function(n = 1:7, alpha = 1){
  ggsci::pal_jama(alpha = alpha)(7)[n]
}


#' Color palettes of JCO journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @seealso [ggsci::scale_color_jco()], [ggsci::scale_fill_jco()]
#'
#' @examples
#' pal_jco_10()
#' show_colors(pal_jco_10())
pal_jco_10 <- function(n = 1:10, alpha = 1){
  ggsci::pal_jco(alpha = alpha)(10)[n]
}


#' Color palettes of NEJM journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @seealso [ggsci::scale_color_nejm()], [ggsci::scale_fill_nejm()]
#'
#' @examples
#' pal_nejm_8()
#' show_colors(pal_jco_10())
pal_nejm_8 <- function(n = 1:8, alpha = 1){
  ggsci::pal_nejm(alpha = alpha)(8)[n]
}


#' Color palettes of lancet journal
#'
#' @inheritParams pal_aaas_10
#' @inherit pal_aaas_10 return
#' @export
#'
#' @seealso [ggsci::scale_color_lancet()], [ggsci::scale_fill_lancet()]
#'
#' @examples
#' pal_lancet_9()
#' show_colors(pal_lancet_9())
pal_lancet_9 <- function(n = 1:9, alpha = 1){
  ggsci::pal_lancet(alpha = alpha)(9)[n]
}


#' Show colors
#'
#' @description
#' A quick and dirty way to show colors in a plot.
#'
#' @param colors a character vector of colors.
#' @param labels label each colour with its hex name?
#' @param borders a logical indicating whether or not to show border colour for
#' each tile, default FALSE.
#' @param cex.label size of printed labels, as multiplier of default size.
#' @param ncol number of columns. If not supplied, tries to be as square as
#' possible.
#'
#' @return No return value.
#' @export
#'
#' @examples
#' show_colors(pal_jama_7())
#'
#' show_colors(pal_lancet_9(), labels = FALSE)
show_colors <- function(colors,
                        labels = TRUE,
                        borders = FALSE,
                        cex.label = 1,
                        ncol = NULL){

  if(borders){
    borders <- NULL
  }else{
    borders <- NA
  }
  scales::show_col(colours = colors,
                   labels = labels,
                   borders = borders,
                   cex_label = cex.label,
                   ncol = ncol)
}


# Reexport from ggsci package ---------------------------------------------

#' @importFrom ggsci scale_color_aaas
#' @export
ggsci::scale_color_aaas


#' @importFrom ggsci scale_fill_aaas
#' @export
ggsci::scale_fill_aaas


#' @importFrom ggsci scale_color_jco
#' @export
ggsci::scale_color_jco


#' @importFrom ggsci scale_fill_jco
#' @export
ggsci::scale_fill_jco


#' @importFrom ggsci scale_color_nejm
#' @export
ggsci::scale_color_nejm


#' @importFrom ggsci scale_fill_nejm
#' @export
ggsci::scale_fill_nejm


#' @importFrom ggsci scale_color_jama
#' @export
ggsci::scale_color_jama


#' @importFrom ggsci scale_fill_jama
#' @export
ggsci::scale_fill_jama


#' @importFrom ggsci scale_color_lancet
#' @export
ggsci::scale_color_lancet


#' @importFrom ggsci scale_fill_lancet
#' @export
ggsci::scale_fill_lancet
