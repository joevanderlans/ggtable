#' Create a new ggtable
#'
#' `ggtable` initializes a ggtable object.  Use the `+` operator to add
#' specifications and features to the table.
#'
#' @param data Data to display on the table.
#'
#' @export
ggtable <- function(data) {
  structure(
    list(data = data,
         ttheme = ttheme_cr(base_font$size,
                            base_font$family),
         tcols = default_tcols(data),
         theaders = list(),
         tpanels = NULL,
         tbreaks = NULL),
    class = c("ggtable", "ggt")
  )
}

is.ggtable <- function(x) {
  inherits(x, "ggtable")
}

#' @export
print.ggtable <- function(table) {
  # the null pdf absorbs a blank page created in the build
  grDevices::pdf(NULL)

  built <- ggtable_build(table)
  gtable <- ggtable_gtable(built)

  grDevices::dev.off()

  for (i in 1:length(gtable)) {
    grid::grid.newpage()
    grid::grid.draw(gtable[[i]])
  }

  invisible(table)
}

#' @export
plot.ggtable <- print.ggtable

grid.draw.ggtable <- function(x) {
  print(x)
}
