# define a ttheme element
el_def <- function(class = NULL, inherit = NULL) {
  list(class = class, inherit = inherit)
}

# the inheritance structure for ttheme elements
.element_tree <- list(
  text = el_def("element_text"),
  rect = el_def("element_rect"),
  title = el_def("element_text", "text"),
  table.title = el_def("element_text", "title"),
  table.subtitle = el_def("element_text", "title"),
  panel.title = el_def("element_text", "text"),
  caption = el_def("element_text", "text"),
  caption.source = el_def("element_text", "caption"),
  caption.note = el_def("element_text", "caption"),
  header = el_def("element_text", "text"),
  header.main = el_def("element_text", "header"),
  header.super = el_def("element_text", "header"),
  header.background = el_def("element_rect", "rect"),
  data = el_def("element_text", "text"),
  data.background = el_def("element_rect", "rect"),
  margin = el_def("margin")
)

#' Calculate element properties by inheriting properties from its parents
#'
#' @param element The name of the ttheme element to calculate.
#' @param ttheme A ttheme object.
#'
#' @export
ttheme_calc_element <- function(element, ttheme) {
  # if element blank, do not inherit anything
  if (inherits(ttheme[[element]], "element_blank")) {
    return(ttheme[[element]])
  }

  # check that element is of the correct class
  if (!is.null(ttheme[[element]]) &&
      !inherits(ttheme[[element]], .element_tree[[element]]$class)) {
    stop(element, " should have class ", .element_tree[[element]]$class)
  }

  # get the name of the parent node
  pnames <- .element_tree[[element]]$inherit

  # if no parents, return this node
  if (is.null(pnames)) {
    # check that all the properties of this element are non-null
    nullprops <- vapply(ttheme[[element]], is.null, logical(1))
    if (any(nullprops)) {
      stop("ttheme element ", element, " has NULL property: ",
           paste(names(nullprops)[nullprops], collapse = ", "))
    }
    return(ttheme[[element]])
  }

  # calculate inheritance for parent objects
  parents <- lapply(pnames, ttheme_calc_element, ttheme)

  Reduce(combine_elements, parents, ttheme[[element]])
}

combine_elements <- function(e1, e2) {

  # if e2 is null, nothing to inherit
  if (is.null(e2) || inherits(e1, "element_blank"))  return(e1)

  # if e1 is null inherit everything from e2
  if (is.null(e1)) return(e2)

  # if e1 has any null properties, inherit them from e2
  nullprops <- vapply(e1[names(e2)], is.null, logical(1))
  e1[nullprops] <- e2[nullprops]

  e1
}

merge_element <- function(new, old) {
  UseMethod("merge_element")
}

merge_element.default <- function(new, old) {
  stop("No method for merging ", class(new)[1], " into ", class(old)[1],
       call. = FALSE)
}

merge_element.element <- function(new, old) {
  if (!inherits(new, class(old)[1])) {
    stop("Only elements of the same class can be merged",
         call. = FALSE)
  }

  # replace null properties of new with properties of old
  new_null <- vapply(new, is.null, logical(1))
  new_null <- names(new_null[new_null])

  # update null items in new with old items
  new[new_null] <- old[new_null]

  new
}

# given a ttheme object and element name, return a grob for the element
element_render <- function(ttheme, element, ..., name = NULL) {

  # get the element from the ttheme, calculating inheritance
  el <- ttheme_calc_element(element, ttheme)
  if (is.null(el)) {
    message("ttheme element ", element, " missing")
    return(ggplot2::zeroGrob())
  }

  grob <- ggplot2::element_grob(el, ...)
  grob$name <- grid::grobName(grob, paste(element, name, sep = "."))
  grob
}
