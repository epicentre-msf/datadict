#' Convert a REDCap data dictionary to the OCA data sharing standard
#'
#' @param x A data.frame reflecting a REDCap data dictionary
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame representing a data dictionary
#' formatted to the OCA data sharing standard
#'
#' @examples
#' # path to example REDCap data dictionary
#' path_data <- system.file("extdata", package = "datadict")
#' path_redcap_dict <- file.path(path_data, "REDCapDataDictionaryDemo.csv")
#'
#' # read dictionary
#' redcap_dict <- read.csv(path_redcap_dict)
#'
#' # convert to OCA format
#' dict_from_redcap(redcap_dict)
#'
#' @importFrom stats setNames
#' @importFrom dplyr `%>%` mutate filter select everything any_of across as_tibble if_else
#' @importFrom rlang .data
#' @importFrom janitor make_clean_names
#' @export dict_from_redcap
dict_from_redcap <- function(x) {

  # standardize names
  names(x) <- janitor::make_clean_names(names(x))

  names(x)[names(x) == "variable_field_name"] <- "field_name"
  names(x)[names(x) == "select_choices_or_calculations"] <- "choices"
  names(x)[names(x) == "choices_calculations_or_slider_labels"] <- "choices"
  names(x)[names(x) == "text_validation_type_or_show_slider_number"] <- "validation"
  names(x)[names(x) == "text_validation_min"] <- "validation_min"
  names(x)[names(x) == "text_validation_max"] <- "validation_max"
  names(x)[names(x) == "branching_logic_show_field_only_if"] <- "branching_logic"

  # remove rows of type descriptive, and extra cols returned by redcap pkg
  x_prep <- as_tibble(x) %>%
    filter(!.data$field_type %in% c("descriptive", "file")) %>%
    select(-any_of(c("field_name_orig", "field_label_orig"))) %>%
    mutate(
      choices = if_else(.data$field_type %in% "yesno", "0, No | 1, Yes", .data$choices)
    ) %>%
    empty_to_na()

  # standardize field types
  x_types <- x_prep %>%
    mutate(
      calculation = if_else(
        .data$field_type == "calc",
        .data$choices,
        NA_character_
      ),
      .after = "choices"
    ) %>%
    mutate(
      choices = if_else(
        .data$field_type == "calc",
        NA_character_,
        .data$choices
      ),
      field_type = case_when(
        grepl("^number|^integer", .data$validation) | .data$field_type %in% "calc" ~ "Numeric",
        grepl("^date", .data$validation) ~ "Date",
        grepl("^time", .data$validation) ~ "Time",
        grepl("^datetime", .data$validation) ~ "Datetime",
        .data$field_type == "radio" ~ "Coded list",
        .data$field_type == "yesno" ~ "Coded list",
        .data$field_type == "dropdown" ~ "Coded list",
        .data$field_type == "checkbox" ~ "Coded list",
        .data$field_type == "notes" ~ "Free text",
        .data$field_type == "text" ~ "Free text"
      )
    )

  # check for non-recognized field types
  if (any(is.na(x_types$field_type))) {

    fields_missing_type <- x_types$field_name[is.na(x_types$field_type)]

    warning(
      "Field type could not be identified for the following fields: ",
      "c(",
      paste(dQuote(fields_missing_type, q = FALSE), collapse = ", "),
      ")"
    )
  }

  # standardize colnames
  x_out <- x_types %>%
    mutate(
      origin = "original",
      status = "shared",
      indirect_identifier = NA_character_
    ) %>%
    select(
      "variable_name" = "field_name",
      "short_label" = "field_label",
      "type" = "field_type",
      "choices",
      "origin",
      "status",
      "form_or_group" = "form_name",
      "indirect_identifier",
      everything()
    ) %>%
    select(!"validation")

  # return
  x_out
}

