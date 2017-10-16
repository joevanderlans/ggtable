#' Add super headers to a ggtable
#'
#' @param ... Vectors or a list of vectors which specify the columns that each
#'   superheader includes.  The name of each argument specifies the title of the
#'   superheader.
#'
#' @export
theaders <- function(...) {

  theaders <- list(...)
  if (is.list(theaders[[1]])) {
    theaders <- theaders[[1]]
  }

  structure(theaders, class = "theaders")
}

#' Update the super headers on a ggtable
#'
#' @param table A table to update.
#' @param newtheaders The new superheaders.
#'
#' @export
update_theaders <- function(table, new_theaders) {
  old_theaders <- table$theaders
  old_theaders[[length(old_theaders) + 1]] <- new_theaders
  old_theaders
}

add_theaders <- function(gtable, theaders, vars, ttheme) {
  if (length(theaders) > 0) {
    for (i in 1:length(theaders)) {
      gtable <- add_theader_row(
        gtable, theaders[[i]], vars, ttheme
      )
    }
  }

  gtable
}

add_theader_row <- function(gtable, theaders, vars, ttheme) {

  labels <- names(theaders)

  grobs <- lapply(
    labels,
    element_render,
    element = "header.super",
    ttheme = ttheme,
    expand_y = TRUE
  )

  height <- lapply(grobs, grid::grobHeight)
  height <- do.call(grid::unit.c, height)
  height <- max(height)

  gtable <- gtable::gtable_add_rows(
    gtable, height, pos = 0
  )

  start_end <- lapply(
    theaders,
    find_start_end,
    vars = vars
  )

  for (i in 1:length(grobs)) {
    gtable <- add_theader(gtable, grobs[[i]], start_end[[i]])
    gtable <- gtable_add_border(
      gtable,
      l = start_end[[i]][1],
      r = start_end[[i]][2]
    )
  }

  gtable
}

add_theader <- function(gtable, grob, start_end) {
  gtable::gtable_add_grob(
    gtable, grob, name = "theader",
    t = 1, b = 1, l = start_end[1], r = start_end[2],
    clip = "off"
  )
}

find_start_end <- function(theader, vars) {

  start <- match(theader[[1]], vars)
  end <- match(theader[[2]], vars)

  c(start, end)
}
