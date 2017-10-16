#' Modify the components of a ttheme
#'
#' @param text All text elements (`element_text`).
#' @param rect All rectangle elements (`element_rect`).
#' @param title All title elements including the title and subtitle
#'   (`element_text`; inherits from `text`).
#' @param table.title The table title (`element_text`; inherits from `title`).
#' @param table.subtitle The table subtitle (`element_text`; inherits from
#'   `title`).
#' @param panel.title The title for each panel (`element_text`; inherits from
#'   `text`).
#' @param caption All caption elements including the source and the note
#'   (`element_text`; inherits from `text`).
#' @param caption.source The source line for the table (`element_text`; inherits
#'   from `caption`).
#' @param caption.note The note for the table (`element_text`; inherits from
#'   `caption`).
#' @param header The column headers (`element_text`; inherits from `text`).
#' @param header.main The column headers (`element_text`; inherits from `header`).
#' @param header.super The column super headers (`element_text`; inherits from `header`).
#' @param header.background The background for the column headers
#'   (`element_rect`; inherits from `rect`).
#' @param data The data contents of the table (`element_text`; inherits from
#'   `text`).
#' @param data.background The background for the data contents of the table
#'   (`element_rect`; inherits from `rect`).
#' @param margin The margins around the entire table (`unit` with the sizes of
#'   the top, right, bottom, and left margins).
#'
#' @export
ttheme <- function(text,
                   rect,
                   title,
                   table.title,
                   table.subtitle,
                   panel.title,
                   caption,
                   caption.source,
                   caption.note,
                   header,
                   header.main,
                   header.super,
                   header.background,
                   data,
                   data.background,
                   margin) {

  elements <- find_args()

  structure(
    elements,
    class = c("ttheme", "ggt")
  )
}

#' Modify the properties of the ttheme
#'
#' @param t1 The existing ttheme object.
#' @param t2 The ttheme object to add to `t1`.
#' @param t2name A name of the t2 object. This is used for printing informative
#'   error messages.
#'
#' @export
add_ttheme <- function(t1, t2, t2name) {

  if (!inherits(t2, "ttheme")) {
    stop("Cannot add ", t2name, " to a ttheme",
         call. = FALSE)
  }

  # iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]

    if (is.null(x) || inherits(x, "element_blank")) {
      # if x is null or element_blank, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y) ||
               is.logical(y) || inherits(y, "element_blank")) {
      # if y is null, a string vector, a numeric vector, or is element_blank,
      # replace x with y
      x <- y
    } else {
      # otherwise, merge x and y
      x <- merge_element(y, x)
    }

    # assign x to t1
    # this is like doing t1[[item]] <- x, except that it preserves nulls
    t1[item] <- list(x)
  }

  t1
}

#' Update the ttheme of a ggtable
#'
#' @param table A table to modify.
#' @param ttheme A new ttheme to add to the existing ttheme.
#'
#' @export
update_ttheme <- function(table, new_ttheme) {

  old_ttheme <- table$ttheme

  # add tthemes with add_ttheme
  old_ttheme + new_ttheme
}

#' Process a ttheme to use with `tableGrob`
#'
#' @param ttheme A ttheme to process.
#'
#' @export
ttheme_helper <- function(ttheme, tcols) {

  data <- ttheme_calc_element("data", ttheme)
  data.background <- ttheme_calc_element("data.background", ttheme)
  header <- ttheme_calc_element("header.main", ttheme)
  header.background <- ttheme_calc_element("header.background", ttheme)

  # find data justification and position
  hjust <- extract_tcol_feature(tcols, "hjust")
  x <- extract_tcol_feature(tcols, "x")

  # find header justification and position
  header_hjust <- extract_tcol_feature(tcols, "header_hjust")
  header_x <- extract_tcol_feature(tcols, "header_x")

  core <- list(fg_params = list(parse      = FALSE,
                                col        = data$colour,
                                fontsize   = data$size,
                                fontfamily = data$family,
                                hjust      = hjust,
                                x          = x),
               bg_params = list(fill = data.background$fill, col = NA),
               padding   = grid::unit(c(data$margin[[1]],
                                        data$margin[[2]]),
                                      attr(data$margin, "unit")))

  header <- list(fg_params = list(parse      = FALSE,
                                  col        = header$colour,
                                  fontsize   = header$size,
                                  fontfamily = header$family,
                                  hjust      = header_hjust,
                                  x          = header_x),
                 bg_params = list(fill = header.background$fill, col = NA),
                 padding   = grid::unit(c(header$margin[[1]],
                                          header$margin[[2]]),
                                        attr(header$margin, "unit")))

  ttheme_grid <- gridExtra::ttheme_default(
    core    = core,
    colhead = header
  )

  ttheme_grid
}
