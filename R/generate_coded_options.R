#' Generate a compact specification of the options for a variable of type 'Coded
#' list'
#'
#' @description
#' The compact form is a single string with the value and label corresponding to
#' a given option separated with a comma, and options separated with a vertical
#' bar, e.g. "value1, Label 1 | value2, Label 2 | value3, Label 3"
#'
#' @param x A vector of labels
#' @param type_of_values Type of values to generate corresponding to each
#'   possible label. Either integers ("int"), strings ("string"), or take values
#'   from the element names of `x` ("names").
#'
#' @return
#' A string giving the possible values and corresponding labels for a given
#' variable, in compact format
#'
#' @examples
#' x <- c("Value #1", "Value #2", "Value #3")
#' names(x) = c("val1", "val2", "val3")
#'
#' # use specified names as values
#' generate_coded_options(x, type_of_values = "names")
#'
#' # generate integer values
#' generate_coded_options(x, type_of_values = "int")
#'
#' # generate string values
#' generate_coded_options(x, type_of_values = "string")
#'
#' @export generate_coded_options
generate_coded_options <- function(x, type_of_values = c("int", "string", "names")) {

  type_of_values <- match.arg(type_of_values, c("int", "string", "names"))

  x <- x[!is.na(x)]
  x <- x[!duplicated(x)]

  if (type_of_values == "int") {
    val <- seq_along(x)
  } else if (type_of_values == "string") {
    val <- labs_to_vals(x)
  } else {
    if (is.null(names(x))) {
      stop("If argument `factor_values` is 'names', `x` must be a named vector")
    }
    val <- names(x)
  }

  val_label <- paste(val, x, sep = ", ")

  choices <- paste(val_label, collapse = " | ")
  choices
}
