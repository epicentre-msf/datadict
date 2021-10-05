#' Convert an OCA style data dictionary into a long-form data.frame containing
#' values and labels of "Coded list" type variables
#'
#' @param dict A data frame reflecting a data dictionary to validate
#'
#' @return
#' A data.frame
#'
#' @importFrom dplyr `%>%` mutate filter select pull
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @export coded_options
coded_options <- function(dict) {

  dict %>%
    dplyr::filter(.data$type == "Coded list") %>%
    dplyr::mutate(choices_split = lapply(.data$choices, split_choices)) %>%
    tidyr::unnest("choices_split") %>%
    dplyr::mutate(
      value = vapply(.data$choices_split, split_value_label, "", i = 1),
      label = vapply(.data$choices_split, split_value_label, "", i = 2)
    ) %>%
    dplyr::select(-.data$choices, -.data$choices_split)
}



#' @noRd
split_choices <- function(x) {
  strsplit(x, "[[:space:]]*\\|[[:space:]]*")[[1]]
}


#' @noRd
#' @importFrom stringr str_split
split_value_label <- function(x, i) {
  stringr::str_split(x, "\\,[[:space:]]*", n = 2)[[1]][i]
}


