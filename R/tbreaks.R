#' Add page breaks to a ggtable
#'
#' @param breaks A vector which specifies the last row number that will appear
#'   on each page.
#'
#' @export
tbreaks <- function(breaks) {
  structure(breaks, class = "tbreaks")
}

#' Update the tbreaks for a ggtable
#'
#' @param table A table to update.
#' @param new_tbreaks The new tbreaks object.
#'
#' @export
update_tbreaks <- function(table, new_tbreaks) {
  new_tbreaks
}

process_tbreaks <- function(breaks) {
  breaks <- c(0, breaks)
  breaks <- diff(breaks)

  page_var <- mapply(
    rep,
    1:length(breaks),
    each = breaks,
    SIMPLIFY = FALSE
  )

  factor(unlist(page_var))
}
