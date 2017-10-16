#' Add features to a ggtable
#'
#' @param e1 An object of class `ggtable` or a `ttheme`.
#' @param e2 A ggtable component, including `tcols`, `ttheme`, `tlabs`,
#'   `tbreaks`, `tpanels`, and `theaders`.
#'
#' @export
"+.ggt" <- function(e1, e2) {
  e2name <- deparse(substitute(e2))
  if (inherits(e1, "ttheme")) {
    add_ttheme(e1, e2, e2name)
  } else if (inherits(e1, "ggtable")) {
    add_ggtable(e1, e2, e2name)
  }
}

add_ggtable <- function(table, object, objectname) {
  if (inherits(object, "tcols")) {
    table$tcols <- update_tcols(table, object)
  } else if (inherits(object, "tlabs")) {
    table$tlabs <- update_tlabs(table, object)
  } else if (inherits(object, "ttheme")) {
    table$ttheme <- update_ttheme(table, object)
  } else if (inherits(object, "theaders")) {
    table$theaders <- update_theaders(table, object)
  } else if (inherits(object, "tpanels")) {
    table$tpanels <- update_tpanels(table, object)
  } else if (inherits(object, "tbreaks")) {
    table$tbreaks <- update_tbreaks(table, object)
  } else {
    stop("Cannot add ", objectname, " to a ggtable",
         call. = FALSE)
  }

  table
}
