# Shared plotting style for runrgetics figures: one colour-blind-safe palette and
# a theme_classic()-based theme, used across the package's plotting functions so
# every figure looks consistent.

#' runrgetics colour palette
#'
#' A colour-blind-safe qualitative palette (Okabe-Ito ordering) used by the
#' package's plotting functions.
#'
#' @param n Number of colours to return. If `NULL` (default) the full palette is
#'   returned; if `n` exceeds the palette length the colours are recycled.
#'
#' @returns A character vector of hex colour codes.
#' @export
#'
#' @examples
#' runrgetics_pal(4)
runrgetics_pal <- function(n = NULL) {
  pal <- c(
    "#0072B2", # blue
    "#D55E00", # vermillion
    "#009E73", # bluish green
    "#56B4E9", # sky blue
    "#E69F00", # orange
    "#CC79A7", # reddish purple
    "#F0E442", # yellow
    "#000000"  # black
  )
  if (is.null(n)) {
    return(pal)
  }
  if (n <= length(pal)) {
    return(pal[seq_len(n)])
  }
  # more groups than base colours: interpolate to keep them distinct
  grDevices::colorRampPalette(pal)(n)
}

#' runrgetics ggplot2 theme
#'
#' A [ggplot2::theme_classic()]-based theme shared by the package's plotting
#' functions, for a consistent look across figures.
#'
#' @param base_size Base font size (points).
#'
#' @returns A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_runrgetics()
theme_runrgetics <- function(base_size = 12) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05)),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' runrgetics discrete colour scale
#'
#' Convenience [ggplot2::scale_colour_manual()] wrapper using [runrgetics_pal()].
#'
#' @param ... Passed to [ggplot2::scale_colour_manual()].
#'
#' @returns A ggplot2 colour scale.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_runrgetics() +
#'   theme_runrgetics()
scale_colour_runrgetics <- function(...) {
  ggplot2::scale_colour_manual(values = runrgetics_pal(), ...)
}
