#' Color palettes of AAAS journal
#'
#' @param n Choose the color from the 10 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_aaas_10 <- function(n = 1:10, alpha = 1){
  ggsci::pal_aaas(alpha = alpha)(10)[n]
}


#' Color palettes of JAMA journal
#'
#' @param n Choose the color from the 7 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_jama_7 <- function(n = 1:7, alpha = 1){
  ggsci::pal_jama(alpha = alpha)(7)[n]
}


#' Color palettes of JCO journal
#'
#' @param n Choose the color from the 10 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_jco_10 <- function(n = 1:10, alpha = 1){
  ggsci::pal_jco(alpha = alpha)(10)[n]
}


#' Color palettes of NEJM journal
#'
#' @param n Choose the color from the 8 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_nejm_8 <- function(n = 1:8, alpha = 1){
  ggsci::pal_nejm(alpha = alpha)(8)[n]
}


#' Color palettes of lancet journal
#'
#' @param n Choose the color from the 9 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
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
