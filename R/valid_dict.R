#' Check that a data dictionary complies with the OCA data sharing standard
#'
#' @description
#' Includes the following checks:
#' - contains required columns
#' - no missing variable names
#' - no duplicated variable names
#' - no missing data types
#' - no non-valid data types
#' - no missing choices (for coded-list type variables)
#' - no non-valid choices (for coded-list type variables)
#'
#' @param dict A data frame reflecting a data dictionary to validate
#'
#' @return
#' `TRUE` if all checks pass, else throws an error
#'
#' @examples
#' # read example dataset
#' path_data <- system.file("extdata", package = "datadict")
#' dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
#'
#' # generate data dictionary template from dataset
#' dict <- dict_from_data(dat, factor_values = "string")
#'
#' # check for validity
#' valid_dict(dict)
#'
#' @importFrom dplyr `%>%` mutate filter select pull
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @export valid_dict
valid_dict <- function(dict) {

  cols_req <- c("variable_name", "type", "choices")
  if (!all(cols_req %in% names(dict))) {
    stop(
      "Dictionary must contain the following required columns: ",
      paste_collapse(cols_req),
      call. = FALSE
    )
  }
  if (any(is.na(dict$variable_name))) {
    rows_missing <- which(is.na(dict$variable_name))
    stop(
      "Dictionary has missing values in column `variable_name` at rows: ",
      paste_collapse(rows_missing, quote = FALSE),
      call. = FALSE
    )
  }
  if (any(duplicated(dict$variable_name))) {
    vars_duplicated <- unique(dict$variable_name[duplicated(dict$variable_name)])
    stop(
      "The following values of column `variable_name` are duplicated: ",
      paste_collapse(vars_duplicated),
      call. = FALSE
    )
  }
  if (any(is.na(dict$type))) {
    stop(
      "Dictionary has missing values in column `type` for variables: ",
      paste_collapse(dict$variable_name[is.na(dict$type)]),
      call. = FALSE
    )
  }
  type_valid <- c("Numeric", "Date", "Time", "Datetime", "Coded list", "Free text")
  if (any(!dict$type %in% type_valid)) {
    stop(
      "Dictionary has non-valid values in column `type`: ",
      paste_collapse(setdiff(dict$type, type_valid)),
      call. = FALSE
    )
  }
  if (any(is.na(dict$choices[dict$type == "Coded list"]))) {
    stop(
      "Dictionary has missing values in column `choices` for the following variables of type 'Coded list': ",
      paste_collapse(dict$variable_name[dict$type == "Coded list" & is.na(dict$choices)]),
      call. = FALSE
    )
  }

  non_parsable_choices <- coded_options(dict) %>%
    dplyr::filter(is.na(.data$value) | .data$value == "" | is.na(.data$label) | .data$label == "") %>%
    dplyr::pull(.data$variable_name) %>%
    unique()

  if (length(non_parsable_choices) > 0) {
    stop(
      "Dictionary has non-parsable values in column `choices` for variables: ",
      paste_collapse(non_parsable_choices),
      call. = FALSE
    )
  }

  return(TRUE)
}

