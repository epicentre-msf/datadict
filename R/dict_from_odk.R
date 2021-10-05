#' Convert an ODK template to the OCA data sharing dictionary standard
#'
#' @param survey A data.frame reflecting the 'survey' sheet within an ODK
#'   template
#' @param choices A data.frame reflecting the 'choices' sheet within an ODK
#'   template
#' @param column_labels (Optional) Name of the column in sheet 'survey' giving
#'   variable labels, e.g. "label::English". Defaults to `NULL` in which case
#'   the first column starting with prefix "label::" is used for the label
#'   column.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame representing a data dictionary
#' formatted to the OCA data sharing standard
#'
#' @examples
#' path_data <- system.file("extdata", package = "datadict")
#' path_odk_template <- file.path(path_data, "WHOVA2016_v1_5_3_ODK.xlsx")
#'
#' odk_survey <- readxl::read_xlsx(path_odk_template, sheet = "survey")
#' odk_choices <- readxl::read_xlsx(path_odk_template, sheet = "choices")
#'
#' dict_from_odk(odk_survey, odk_choices)
#'
#' @importFrom stats setNames
#' @importFrom dplyr `%>%` mutate filter select group_by summarize everything
#'   as_tibble add_row
#' @importFrom rlang .data
#' @importFrom janitor make_clean_names remove_empty
#' @export dict_from_odk
dict_from_odk <- function(survey,
                          choices,
                          column_labels = NULL) {

  # standardize names
  names(survey) <- janitor::make_clean_names(names(survey))
  names(choices) <- janitor::make_clean_names(names(choices))

  col_labels <- grep("label_", names(survey), value = TRUE)[1]

  # remove empty or non-relevant rows
  survey_clean <- dplyr::as_tibble(survey) %>%
    janitor::remove_empty("rows") %>%
    dplyr::filter(!grepl("(begin|end)(\\_+|[[:space:]]+)group", .data$type)) %>%
    dplyr::filter(!.data$type %in% c("start", "end", "note"))

  choices_clean <- choices %>%
    janitor::remove_empty("rows")

  # convert choices to OCA format (i.e. REDCap format)
  choices_prep <- choices_clean %>%
    dplyr::mutate(name_lab = paste(.data$name, .data[[col_labels]], sep = ", ")) %>%
    dplyr::group_by(.data$list_name) %>%
    dplyr::summarize(choices = paste(.data$name_lab, collapse = " | "), .groups = "drop") %>%
    dplyr::add_row(list_name = "checkbox", choices = c("0, Unchecked | 1, Checked"))

  # standardize field types
  survey_types <- survey_clean %>%
    expand_multichoice(., choices_prep, col_labels) %>%
    dplyr::mutate(
      list_name = stringr::str_extract(.data$type, stringr::regex("(?<=select_(one|multiple) ).*")),
      type = dplyr::case_when(
        grepl("^select_one", .data$type) ~ "Coded list",
        grepl("^select_multiple", .data$type) ~ "Coded list",
        .data$type == "date" ~ "Date",
        .data$type == "today" ~ "Date",
        .data$type == "integer" ~ "Numeric",
        .data$type == "decimal" ~ "Numeric",
        .data$type == "calculate" ~ "Numeric", # note these could really be logical
        .data$type == "text" ~ "Free text"
      )
    ) %>%
    dplyr::left_join(choices_prep, by = "list_name") %>%
    dplyr::select(-.data$list_name)

  # check for non-recognized field types
  if (any(is.na(survey_types$type))) {

    fields_missing_type <- survey_types$name[is.na(survey_types$type)]

    warning(
      "Field type could not be identified for the following fields: ",
      "c(",
      paste(dQuote(fields_missing_type, q = FALSE), collapse = ", "),
      ")"
    )
  }

  # standardize colnames
  survey_out <- survey_types %>%
    dplyr::mutate(
      origin = "Original",
    ) %>%
    dplyr::select(
      variable_name = .data$name,
      short_label   = .data[[col_labels]],
      type          = .data$type,
      choices       = .data$choices,
      origin        = .data$origin,
      dplyr::everything()
    )

  # return
  survey_out
}




#' @noRd
#' @importFrom dplyr `%>%` arrange mutate filter select bind_rows left_join
#' @importFrom rlang .data ensym `!!` `:=`
#' @importFrom tidyr unnest
#' @importFrom purrr map map_chr
#' @importFrom stringr str_extract regex
expand_multichoice <- function(x, choices, col_labels) {

  x$rowid <- seq_len(nrow(x))

  x_multi <- x %>%
    dplyr::mutate(list_name = stringr::str_extract(.data$type, stringr::regex("(?<=select_(one|multiple) ).*"))) %>%
    dplyr::filter(grepl("^select_multiple", .data$type)) %>%
    dplyr::left_join(choices, by = "list_name") %>%
    dplyr::mutate(choices = purrr::map(.data$choices, split_choices)) %>%
    tidyr::unnest("choices") %>%
    dplyr::mutate(
      checkbox_value = purrr::map_chr(.data$choices, ~ strsplit(.x, ", *")[[1]][1]),
      checkbox_label = purrr::map_chr(.data$choices, ~ strsplit(.x, ", *")[[1]][2]),
      name = paste(.data$name, .data$checkbox_value, sep = "___"), ### not sure of actual ODK format, no examples
      !!col_labels := paste(!!ensym(col_labels), .data$checkbox_label, sep = "/"),
      type = "select_multiple checkbox"
    )

  x %>%
    dplyr::filter(!grepl("^select_multiple", .data$type)) %>%
    dplyr::bind_rows(x_multi) %>%
    dplyr::arrange(.data$rowid) %>%
    dplyr::select(-c("rowid", "checkbox_value", "checkbox_label", "choices"))
}


