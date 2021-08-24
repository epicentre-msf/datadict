#' Generate a data dictionary template from a dataset
#'
#' @param x A data frame reflecting the dataset from which to generate a data
#'   dictionary template
#' @param factor_threshold An integer representing the maximum number of unique
#'   values within a dataset column for that column to be classified as a
#'   factor-type (i.e. coded list) variable. Columns that are not recognized as
#'   other data types (such as Numeric, Date, etc.) and have more than
#'   `factor_threshold` unique values will be specified as type "Free text".
#'   Defaults to 10.
#' @param factor_values Should values of factor-type (i.e. Coded list) variables
#'   be generated as integers ("int") or strings ("string"). Defaults to "int".
#'   ```
#'    label    | int | string
#'   -------------------------
#'    Yes      | 0   | yes
#'    No       | 1   | no
#'    Not sure | 2   | not_sure
#'   ```
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame representing a data dictionary
#' template formatted to the OCA data sharing standard
#'
#' @examples
#' # read example dataset
#' path_data <- system.file("extdata", package = "datadict")
#' dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
#'
#' # generate data dictionary template from dataset
#' dict_from_data(dat, factor_values = "string")
#'
#' @importFrom dplyr `%>%` mutate filter select group_by ungroup summarize tibble all_of n
#' @importFrom tidyr unnest
#' @importFrom rlang .data .env
#' @export dict_from_data
dict_from_data <- function(x,
                           factor_threshold = 10,
                           factor_values = c("int", "string")) {

  factor_values <- match.arg(factor_values, c("int", "string"))

  dict_prep <- dplyr::tibble(
    variable_name = names(x),
    short_label = NA_character_,
    type = classify_type(x, factor_threshold)
  )

  dict_coded <- dict_prep %>%
    dplyr::filter(.data$type == "Coded list") %>%
    dplyr::select(.data$variable_name)

  opts <- x %>%
    dplyr::select(dplyr::all_of(dict_coded$variable_name)) %>%
    lapply(function(x) sort(unique(x[!is.na(x)])))

  dict_coded_long <- dict_coded %>%
    dplyr::mutate(choices_label = unname(.env$opts)) %>%
    tidyr::unnest("choices_label") %>%
    dplyr::group_by(.data$variable_name) %>%
    dplyr::mutate(choices_value = as.character(seq_len(dplyr::n()) - 1L), .before = .data$choices_label) %>%
    dplyr::ungroup()

  if (factor_values == "string") {
    dict_coded_long <- dict_coded_long %>%
      dplyr::mutate(choices_value = labs_to_vals(.data$choices_label))
  }

  dict_coded_wide <- dict_coded_long %>%
    dplyr::mutate(name_lab = paste(.data$choices_value, .data$choices_label, sep = ", ")) %>%
    dplyr::group_by(.data$variable_name) %>%
    dplyr::summarize(choices = paste(.data$name_lab, collapse = " | "), .groups = "drop")

  dict_out <- dict_prep %>%
    dplyr::left_join(dict_coded_wide, by = "variable_name") %>%
    dplyr::mutate(origin = "Original")

  # return
  dict_out
}



#' @noRd
classify_type_ <- function(x, free_threshold) {

  if ("Date" %in% class(x)) {
    out <- "Date"
  } else if (any(grepl("POSIX", class(x)))) {
    out <- "Datetime"
  } else if ("integer" %in% class(x) | "numeric" %in% class(x)) {
    out <- "Numeric"
  } else if ("character" %in% class(x)) {
    if (length(unique(x[!is.na(x)])) > free_threshold) {
      out <- "Free text"
    } else {
      out <- "Coded list"
    }
  } else {
    out <- NA_character_
  }

  out
}


#' @noRd
classify_type <- function(x, free_threshold) {
  as.character(
    vapply(x, classify_type_, character(1), free_threshold = free_threshold)
  )
}


#' @noRd
#' @importFrom stringi stri_trans_general
labs_to_vals <- function(x) {
  x <- tolower(x)
  x <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", x)
  x <- gsub("[^[:alnum:]]+", "_", x)
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  x
}
