# unexported function from the gridextra package
# updated with the option to recycle params by row
gtable_table <- function(d, widths, heights,
                         fg_fun = text_grob, fg_params = list(),
                         bg_fun = rect_grob, bg_params = list(),
                         padding = unit(c(4, 4), "mm"),
                         name = "table", vp = NULL,
                         fg_params_byrow = FALSE,
                         bg_params_byrow = FALSE){

  label_matrix <- as.matrix(d)

  nc <- ncol(label_matrix)
  nr <- nrow(label_matrix)
  n <- nc * nr

  # formatting parameters will be recycled iff
  # there are fewer elements than needed
  rep_ifshort <- function(x, n, nc, nr, byrow) {
    if (length(x) >= n) {
      return(x[1:n])
    } else if (byrow == TRUE) {  # first by row, then by col
      return(rep(rep(x, length.out = nc), length.out = n))
    } else {  # first by col, then by row
      return(rep(rep(x, length.out = nr), length.out = n))
    }
  }

  fg_params <- lapply(fg_params, rep_ifshort, n = n, nc = nc, nr = nr,
                      byrow = fg_params_byrow)
  bg_params <- lapply(bg_params, rep_ifshort, n = n, nc = nc, nr = nr,
                      byrow = bg_params_byrow)

  if (fg_params_byrow) {
    label_vector <- as.vector(t(label_matrix))
  } else {
    label_vector <- as.vector(label_matrix)
  }

  fg_params <- data.frame(fg_params,
                          label = label_vector,
                          stringsAsFactors = FALSE)

  bg_params <- data.frame(bg_params, stringsAsFactors = FALSE)

  labels <- do.call(mapply, c(fg_params, list(FUN = fg_fun,
                                              SIMPLIFY = FALSE)))
  bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun,
                                             SIMPLIFY = FALSE)))

  label_grobs <- matrix(labels, ncol = nc, byrow = fg_params_byrow)
  bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = bg_params_byrow)

  # some calculations of cell sizes
  if (missing(widths))
    widths <- col_widths(label_grobs) + padding[1]
  if (missing(heights))
    heights <- row_heights(label_grobs) + padding[2]

  # place labels in a gtable
  g <- gtable::gtable_matrix(paste0(name, "-fg"),
                             grobs = label_grobs,
                             widths = widths,
                             heights = heights, vp = vp)

  # add the background
  g <- gtable::gtable_add_grob(g, bkgds_grobs,
                               t = rep(seq_len(nr), length.out = n),
                               l = rep(seq_len(nc), each = nr), z = 0,
                               name = paste0(name, "-bg"))

  g
}

row_heights <- function(m){
  do.call(grid::unit.c, apply(m, 1, function(l)
    max(do.call(grid::unit.c, lapply(l, grid::grobHeight)))))
}

col_widths <- function(m){
  do.call(grid::unit.c, apply(m, 2, function(l)
    max(do.call(grid::unit.c, lapply(l, grid::grobWidth)))))
}
