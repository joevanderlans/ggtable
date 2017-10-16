#' Modify the title and caption of a ggtable
#'
#' @param title The title for the table.
#' @param subtitle The subtitle for the table.
#' @param source The source line for the table.
#' @param note The note section for the table.
#' @param wrap_source A logical value to indicate if the source should wrap.
#' @param wrap_note A logical value to indicate if the note should wrap.
#' @param wrap_len An integer indicating the target wrap length.
#' @param source_exdent An integer indicating the indentation of subsequent
#'   lines in the source.
#' @param note_exdent An integer indicating the indentation of subsequent lines
#'   in the note.
#'
#' @export
tlabs <- function(..., wrap_note = TRUE, wrap_source = TRUE,
                  wrap_len = 80, source_exdent = 0, note_exdent = 6) {

  args <- list(...)
  if (is.list(args[[1]])) args <- args[[1]]

  # wrap source
  if (wrap_source && !is.null(args$source)) {
    args$source <- wrap_text(
      args$source, width = wrap_len,
      exdent = source_exdent
    )
  }

  # wrap note
  if (wrap_note && !is.null(args$note)) {
    args$note <- wrap_text(
      args$note, width = wrap_len,
      exdent = note_exdent
    )
  }

  structure(args, class = "tlabs")
}

#' Update the labels on a ggtable
#'
#' @param table A table to modify.
#' @param tlabs The new labels.
#'
#' @export
update_tlabs <- function(table, tlabs) {
  plyr::defaults(tlabs, table$tlabs)
}

# wrap text for source and note
wrap_text <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}
