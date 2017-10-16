#' Build a ggtable for rendering
#'
#' @param table A ggtable object to build.
#'
#' @export
ggtable_build <- function(table) {

  data <- table$data
  tcols <- table$tcols
  tpanels <- table$tpanels
  tbreaks <- table$tbreaks

  # check if the panel var is segregated in the data
  if (!is.null(tpanels) &&
      !is_panel_segregated(data[[tpanels]])) {
    # arrange data by the panel variable
    data <- data[order(data[[tpanels]]), , drop = FALSE]
  }

  # add dummy panels and page breaks if none
  if (is.null(tpanels)) {
    panel_var <- rep("NO_PANEL_VAR", nrow(data))
  } else {
    panel_var <- data[[tpanels]]
  }

  if (is.null(tbreaks)) {
    tbreaks <- tbreaks(nrow(data))
  }

  # format data according to tcols
  data <- lapply(tcols, format_tcol, data = data)
  data <- data.frame(data, stringsAsFactors = FALSE)

  # split by page breaks
  page_var <- process_tbreaks(tbreaks)
  data <- split(data, page_var)

  # split the panel var by page breaks
  panel_var <- split(panel_var, page_var)
  panel_var <- lapply(panel_var, factor)

  # for each page, split by the panel var
  data <- mapply(
    split_panels,
    data = data,
    panel_var = panel_var,
    SIMPLIFY = FALSE
  )

  data <- set_panel_names(data, panel_var)

  table$data <- data

  structure(
    table,
    class = "ggtable_built"
  )
}

#' Create a table from a built ggtable object
#'
#' @param built A built ggtable with which to create a ggtable.
#'
#' @export
ggtable_gtable <- function(built) {

  stopifnot(inherits(built, "ggtable_built"))

  data <- built$data
  tcols <- built$tcols
  theaders <- built$theaders
  ttheme <- built$ttheme
  tlabs <- built$tlabs

  ttheme_grid <- ttheme_helper(ttheme, tcols)

  # header portion of table ---------------------------------------------------
  headers <- extract_tcol_feature(tcols, "header")
  vars <- extract_tcol_feature(tcols, "var")

  table_header <- gtable_table(
    t(headers), name = "table.header",
    fg_fun = ttheme_grid$colhead$fg_fun,
    bg_fun = ttheme_grid$colhead$bg_fun,
    fg_params = ttheme_grid$colhead$fg_params,
    bg_params = ttheme_grid$colhead$bg_params,
    padding = ttheme_grid$colhead$padding,
    fg_params_byrow = TRUE
  )

  # add border under header
  table_header <- gtable_add_border(table_header)

  pans <- table_header$layout

  # add super headers
  table_header <- add_theaders(table_header, theaders, vars, ttheme)

  # create title
  title <- element_render(
    ttheme, "table.title", tlabs$title, expand_y = TRUE
  )
  title_height <- grid::grobHeight(title)

  # create subtitle
  subtitle <- element_render(
    ttheme, "table.subtitle", tlabs$subtitle, expand_y = TRUE
  )
  subtitle_height <- grid::grobHeight(subtitle)

  # add spacer row
  table_header <- add_spacer_row(table_header, 25.5, pos = 0)

  # add subtitle
  table_header <- gtable::gtable_add_rows(
    table_header, subtitle_height, pos = 0
  )
  table_header <- gtable::gtable_add_grob(
    table_header, subtitle, name = "subtitle",
    t = 1, b = 1, l = min(pans$l), r = max(pans$r),
    clip = "off"
  )

  # add title
  table_header <- gtable::gtable_add_rows(
    table_header, title_height, pos = 0
  )
  table_header <- gtable::gtable_add_grob(
    table_header, title, name = "title",
    t = 1, b = 1, l = min(pans$l), r = max(pans$r),
    clip = "off"
  )

  # caption portion of table ---------------------------------------------------

  # create source
  source <- element_render(
    ttheme, "caption.source", tlabs$source, expand_y = TRUE
  )
  source_height <- grid::grobHeight(source)

  # create note
  note <- element_render(
    ttheme, "caption.note", tlabs$note, expand_y = TRUE
  )
  note_height <- grid::grobHeight(note)

  # put tables together -------------------------------------------------------
  table_pages <- mapply(
    build_page,
    page_data = data,
    table_header = list(table_header),
    ttheme = list(ttheme),
    ttheme_grid = list(ttheme_grid),
    SIMPLIFY = FALSE
  )

  # add caption to the last page
  final_page <- table_pages[[length(table_pages)]]

  # add spacer row
  final_page <- add_spacer_row(final_page, 25.5)

  # add source
  final_page <- gtable::gtable_add_rows(
    final_page, source_height, pos = -1
  )
  final_page <- gtable::gtable_add_grob(
    final_page, source, name = "source",
    t = -1, b = -1, l = min(pans$l), r = max(pans$r),
    clip = "off"
  )

  # add spacer row
  final_page <- add_spacer_row(final_page, 12.75)

  # add note
  final_page <- gtable::gtable_add_rows(
    final_page, note_height, pos = -1
  )
  final_page <- gtable::gtable_add_grob(
    final_page, note, name = "note",
    t = -1, b = -1, l = min(pans$l), r = max(pans$r),
    clip = "off"
  )

  table_pages[[length(table_pages)]] <- final_page

  # update table widths with widths specified in tcols
  table_pages <- update_widths(table_pages, tcols)

  # justify top and add margins
  table_pages <- lapply(table_pages, add_margins, ttheme = ttheme)
  table_pages <- lapply(table_pages, justify_top)

  # return gtable
  table_pages
}

#' Generate a ggtable grob
#'
#' @param table ggtable object.
#'
#' @export
ggtableGrob <- function(table) {
  built <- ggtable_build(table)
  ggtable_gtable(built)
}

# add page margins
add_margins <- function(table_gtable, ttheme) {

  table_gtable <- gtable::gtable_add_rows(
    table_gtable, ttheme$margin[1], pos = 0
  )
  table_gtable <- gtable::gtable_add_cols(
    table_gtable, ttheme$margin[2], pos = -1
  )
  table_gtable <- gtable::gtable_add_rows(
    table_gtable, ttheme$margin[3], pos = -1
  )
  table_gtable <- gtable::gtable_add_cols(
    table_gtable, ttheme$margin[4], pos = 0
  )

  table_gtable
}

# top justify the table
justify_top <- function(table_gtable) {

  height <- sum(table_gtable$heights)
  y_center <- grid::unit(1, "npc") - 0.5 * height
  table_gtable$vp <- grid::viewport(y = y_center)

  table_gtable
}

# build one page of the table
build_page <- function(page_data, table_header, ttheme, ttheme_grid) {

  panel_names <- names(page_data)

  panel_gtables <- mapply(
    build_panel,
    panel = page_data,
    panel_name = panel_names,
    ttheme = list(ttheme),
    ttheme_grid = list(ttheme_grid),
    SIMPLIFY = FALSE
  )

  if (length(panel_gtables) > 1) {
    non_final_panels <- panel_gtables[1:(length(panel_gtables) - 1)]
    non_final_panels <- lapply(non_final_panels, add_spacer_row, height = 12.75)
    panel_gtables[1:(length(panel_gtables) - 1)] <- non_final_panels
  }

  panel_gtable <- do.call(gridExtra::gtable_rbind, panel_gtables)

  page_gtable <- gridExtra::gtable_rbind(table_header, panel_gtable)

  page_gtable
}

# build one panel of the table
build_panel <- function(panel, panel_name, ttheme, ttheme_grid) {

  panel_gtable <- gtable_table(
    panel, name = "panel.data",
    fg_fun = ttheme_grid$core$fg_fun,
    bg_fun = ttheme_grid$core$bg_fun,
    fg_params = ttheme_grid$core$fg_params,
    bg_params = ttheme_grid$core$bg_params,
    padding = ttheme_grid$core$padding,
    fg_params_byrow = TRUE
  )

  pans <- panel_gtable$layout

  if (!identical(panel_name, "NO_PANEL_VAR")) {
    # create panel header
    panel_header <- element_render(
      ttheme, "panel.title", panel_name, expand_y = TRUE
    )
    panel_header_height <- grid::grobHeight(panel_header)

    # add panel header
    panel_gtable <- gtable::gtable_add_rows(
      panel_gtable, panel_header_height, pos = 0
    )
    panel_gtable <- gtable::gtable_add_grob(
      panel_gtable, panel_header, name = "panel.title",
      t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off"
    )

    panel_gtable <- gtable_add_border(panel_gtable)
  }

  panel_gtable
}

# add a blank spacer row to a table
# defaults to the last row on the table
add_spacer_row <- function(gtable, height, pos = -1) {

  gtable <- gtable::gtable_add_rows(
    gtable, grid::unit(height, "points"), pos = pos
  )

  gtable
}

# add a border line to the table
# defaults to a line across the page below the first row
gtable_add_border <- function(gtable, b = 1, l = 1, r = ncol(gtable)) {

  border <- grid::segmentsGrob(
    x0 = grid::unit(0, "npc"),
    y0 = grid::unit(0, "npc"),
    x1 = grid::unit(1, "npc"),
    y1 = grid::unit(0, "npc"),
    gp = grid::gpar(fill = NA, lwd = 1)
  )

  gtable <- gtable::gtable_add_grob(
    gtable,
    grobs = border,
    t = b,
    b = b,
    l = l,
    r = r
  )

  gtable
}
