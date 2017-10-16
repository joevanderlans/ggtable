#' Divide the ggtable into panels
#'
#' @param var A variable by which to divide the ggtable into panels.  The values
#'   of the variable specify the panel titles.
#'
#' @export
tpanels <- function(var) {
  structure(var, class = "tpanels")
}

#' Update the tpanels for a ggtable
#'
#' @param table A table to update.
#' @param new_tpanels The new tpanels object.
#'
#' @export
update_tpanels <- function(table, new_tpanels) {
  new_tpanels
}

split_panels <- function(data, panel_var) {
  split(data, panel_var)
}

is_panel_segregated <- function(panel_var) {
  unique_panel_var <- unique(panel_var)

  number_each <- vapply(
    unique_panel_var,
    count_panel_var,
    panel_var = panel_var,
    FUN.VALUE = numeric(1)
  )

  model_panel_var <- rep(unique_panel_var, times = number_each)
  all(model_panel_var == panel_var |
        (is.na(model_panel_var) & is.na(panel_var)))
}

count_panel_var <- function(panel_var, value) {
  if (is.na(value)) {
    sum(is.na(panel_var))
  } else {
    sum(panel_var == value, na.rm = TRUE)
  }
}

set_panel_names <- function(data, panel_var) {

  panel_names <- lapply(panel_var, unique)

  mapply(
    set_names,
    page_data = data,
    panel_names = panel_names,
    SIMPLIFY = FALSE
  )
}

set_names <- function(page_data, panel_names) {
  names(page_data) <- panel_names
  page_data
}
