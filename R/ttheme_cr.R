#' The default theme for ggtable
#'
#' @param base_size The base font size.
#' @param font_family The base font family.
#'
#' @export
ttheme_cr <- function(base_size = base_font$size,
                      font_family = base_font$family) {
  ttheme(
    text = ggplot2::element_text(
      family = font_family, size = base_size,
      face = "plain", colour = "black",
      lineheight = 0.9, angle = 0,
      hjust = 0.5, vjust = 0.5,
      margin = ggplot2::margin(t = 0.2, r = 0, b = 0.2, l = 0, unit = "mm"),
      debug = FALSE
    ),
    rect = ggplot2::element_rect(
      fill = NA, colour = NA,
      size = NA, linetype = NA
    ),
    title = ggplot2::element_text(size = base_size * 2),
    table.title = ggplot2::element_text(face = "bold"),
    table.subtitle = ggplot2::element_text(),
    panel.title = ggplot2::element_text(
      size = base_size * (10 / 9), face = "italic", hjust = 0.0,
      margin = ggplot2::margin(t = 0.8, r = 0, b = 0.8, l = 0, unit = "mm")
    ),
    caption = ggplot2::element_text(hjust = 0.0),
    caption.source = ggplot2::element_text(),
    caption.note = ggplot2::element_text(),
    header = ggplot2::element_text(
      size = base_size * (10 / 9), face = "bold"
    ),
    header.main = ggplot2::element_text(
      margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    ),
    header.super = ggplot2::element_text(),
    header.background = ggplot2::element_rect(fill = NA),
    data = ggplot2::element_text(
      size = base_size * (10 / 9),
      margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    ),
    data.background = ggplot2::element_rect(fill = c("#DAEEF3", NA)),
    margin = ggplot2::margin(t = 54, b = 54, r = 50, l = 50, unit = "pt")
  )
}
