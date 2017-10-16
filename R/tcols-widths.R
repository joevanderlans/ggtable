# update the default widths with the tcol widths
update_widths <- function(gtables, tcols) {

  vars <- extract_tcol_feature(tcols, "var")
  widths <- lapply(tcols, `[[`, i = "width")

  default_widths <- extract_widths(gtables)
  names(default_widths) <- vars

  widths <- calc_widths(widths, default_widths)

  set_widths(gtables, widths)
}

# find the current widths of the table
extract_widths <- function(gtables) {

  default_widths <- lapply(gtables, `[[`, i = "widths")

  # use the max width across all pages
  widths_all <- do.call(
    mapply, c(list(FUN = max, SIMPLIFY = FALSE), default_widths)
  )

  widths_all
}

# use the tcol widths if specified
calc_widths <- function(widths, defaults) {

  for (var in names(widths)) {
    if (!is.null(widths[[var]]))
      defaults[[var]] <- widths[[var]]
  }

  defaults
}

# specify the widths for a table
set_widths <- function(gtables, widths) {

  widths <- do.call(grid::unit.c, widths)

  lapply(
    gtables,
    function(gtable, widths) {
      gtable$widths <- widths
      gtable
    },
    widths = widths
  )
}
