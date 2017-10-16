#' PDF a ggtable
#'
#' @param table A table to save.
#' @param path A location to save the PDF.
#'
#' @export
ggtsave <- function(x, filename, width = 11, height = 8.5) {

  grDevices::pdf(
    file = filename,
    width = width, height = height,
    onefile = TRUE
  )

  on.exit(utils::capture.output(grDevices::dev.off()))

  # save a list of ggtables to the same pdf
  if (all(sapply(x, is.ggtable))) {
    for(i in x) {
      grid.draw(i)
    }
  } else {
    grid.draw(x)
  }

  invisible()
}
