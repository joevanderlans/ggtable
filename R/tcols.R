#' Create a column for a ggtable
#'
#' @param var A character string to specify the variable to displayed on the
#'   table.
#' @param header A character string to specify the column header to display on
#'   the table.
#' @param format_fun A function to apply to the values in the column that will
#'   generate the values to display on the table.
#' @param width A unit value (i.e., unit(2, "cm")) to specificy the width of the
#'   column.
#' @param hjust A numeric value between 0 (left) and 1 (right) to specify the
#'   horizontal justification for the data values.
#' @param x A numeric value between 0 (left) and 1 (right) to specify the
#'   horizontal position for the data values.
#' @param header_hjust A numeric value between 0 (left) and 1 (right) to specify
#'   the horizontal justification for the headers.
#' @param header_x A numeric value between 0 (left) and 1 (right) to specify the
#'   horizontal position for the headers.
#' @param wrap_len the target length at which the values in the column should
#'   wrap.
#'
#' @export
tcol <- function(var,
                 header,
                 format_fun,
                 width,
                 hjust,
                 x,
                 header_hjust,
                 header_x,
                 wrap_len) {

  tcol <- find_args()

  structure(
    tcol,
    class = "tcol"
  )
}

#' Modify the columns of a ggtable
#'
#' @param ... tcols or a list of tcols with which to update your table.
#'
#' @export
tcols <- function(...) {

  tcols <- list(...)

  if (all(sapply(tcols[[1]], inherits, what = "tcol"))) {
    tcols <- tcols[[1]]
  }

  names(tcols) <- extract_tcol_feature(tcols, "var")
  structure(tcols, class = "tcols")
}

#' Update the columns on a ggtable
#'
#' @param table A table to update.
#' @param tcols The new columns.
#'
#' @export
update_tcols <- function(table, new_tcols) {

  old_tcols <- table$tcols

  for (name in names(new_tcols)) {
    new_tcol <- new_tcols[[name]]
    old_tcol <- old_tcols[[name]]

    new_tcol <- plyr::defaults(new_tcol, old_tcol)

    new_tcols[[name]] <- new_tcol
  }

  new_tcols
}

#' Create the default columns for a ggtable
#'
#' @param data The data with which to create default columns.
#'
#' @export
default_tcols <- function(data) {

  vars <- names(data)
  numeric_vars <- lapply(data, is.numeric)

  default_tcols <- mapply(
    default_tcol,
    var = vars,
    is_numeric = numeric_vars,
    SIMPLIFY = FALSE
  )

  tcols(default_tcols)
}

#' Create a default column for a ggtable
#'
#' @param var A character string to specify the variable with which to create a
#'   default column.
#' @param is_numeric A logical to indicate if the variable is numeric.
#'
#' @export
default_tcol <- function(var, is_numeric) {
  if (is_numeric) {
    default_tcol_numeric(var)
  } else {
    default_tcol_character(var)
  }
}

#' Create a default numeric column for a ggtable
#'
#' @param var A character string to specify the variable with which to create a
#'   default column.
#'
#' @export
default_tcol_numeric <- function(var) {

  format_fun <- function(x) format(x, trim = TRUE, scientific = FALSE)

  tcol(
    var = var,
    header = var,
    format_fun = format_fun,
    width = NULL,
    hjust = 1,
    x = 0.6,
    header_hjust = 0.5,
    header_x = 0.5,
    wrap_len = NULL
  )
}

#' Create a default character column for a ggtable
#'
#' @param var A character string to specify the variable with which to create a
#'   default column.
#'
#' @export
default_tcol_character <- function(var) {
  tcol(
    var = var,
    header = var,
    format_fun = NULL,
    width = NULL,
    hjust = 0,
    x = 0.05,
    header_hjust = 0.5,
    header_x = 0.5,
    wrap_len = NULL
  )
}

#' Create a spacer column
#'
#' @param width A unit value (i.e., unit(2, "cm")) to specificy the width of the
#'   column.
#'
#' @export
tcol_blank <- function(width = grid::unit(1, "cm")) {
  tcol(
    var = "BLANK_VAR",
    header = "",
    format_fun = NULL,
    width = width,
    hjust = 0.5,
    x = 0.5,
    header_hjust = 0.5,
    header_x = 0.5,
    wrap_len = NULL
  )
}

#' @export
format_tcol <- function(tcol, data) {

  var <- tcol$var
  format_fun <- tcol$format_fun
  wrap_len <- tcol$wrap_len

  if (identical(var, "BLANK_VAR")) {
    x <- rep("", nrow(data))
  } else {
    x <- data[[var]]
  }

  if (!is.null(format_fun)) {
    x <- format_fun(x)
  }

  if (!is.null(wrap_len)) {
    x <- strwrap(x, width = wrap_len, simplify = FALSE)
    x <- vapply(x, paste, collapse = "\n", FUN.VALUE = character(1))
  }

  list(x)
}

# extract a vector of all tcol values of a particular feature for a set of tcols
extract_tcol_feature <- function(tcols, feature) {
  sapply(tcols, `[[`, i = feature)
}
