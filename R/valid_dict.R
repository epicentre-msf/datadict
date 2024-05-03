#' Check that a data dictionary complies with the OCA data sharing standard
#'
#' @description Includes the following checks:
#' - contains required columns (`variable_name`, `short_label`, `type`, `choices`, `origin`, `status`)
#' - required columns complete (no missing values)
#' - no duplicated values in column `variable_name`
#' - no non-valid values in columns `type`, `origin`, `status`, `indirect_identifier`
#' - for coded-list type variables:
#'   - no missing choices
#'   - no incorrectly formatted choices (expected format is "value1, Label 1 | value2, Label 2 | ...")
#'
#' @param dict A data frame reflecting a data dictionary to validate
#' @param verbose Logical indicating whether to give warning describing the
#'   checks that have failed. Defaults to TRUE.
#'
#' @return `TRUE` if all checks pass, `FALSE` if any checks fail
#'
#' @examples
#' # read example dataset
#' path_data <- system.file("extdata", package = "datadict")
#' dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
#'
#' # generate data dictionary template from dataset
#' dict <- dict_from_data(dat, factor_values = "string")
#'
#' # dictionary column 'indirect_identifier' must be manually specified (yes/no)
#' dict$indirect_identifier <- "no"
#'
#' # check for validity
#' valid_dict(dict)
#'
#' @importFrom dplyr `%>%` mutate filter select pull distinct group_by summarize
#' @importFrom dbc check_categorical
#' @importFrom rlang .data
#' @export valid_dict
valid_dict <- function(dict, verbose = TRUE) {


  ## prep check and msg outputs ------------------------------------------------
  checks <- c(
    req_cols              = as.logical(NA),
    no_missing_vals       = as.logical(NA),
    no_duplicated_varname = as.logical(NA),
    valid_categories      = as.logical(NA),
    no_missing_choices    = as.logical(NA),
    all_choices_parsable  = as.logical(NA)
  )

  msgs <- c(
    req_cols              = "Unable to test",
    no_missing_vals       = "Unable to test",
    no_duplicated_varname = "Unable to test",
    valid_categories      = "Unable to test",
    no_missing_choices    = "Unable to test",
    all_choices_parsable  = "Unable to test"
  )


  ## check that dict contains all required columns -----------------------------
  cols_req <- c("variable_name", "short_label", "type", "choices", "origin", "status")

  missing_req_cols <- setdiff(cols_req, names(dict))
  checks["req_cols"] <- length(missing_req_cols) == 0L

  msgs["req_cols"] <- ifelse(
    checks["req_cols"],
    "OK",
    paste0("- Required columns missing: ", paste_collapse(missing_req_cols))
  )


  ## check that all required cols complete (no missing values) -----------------
  # TODO: consider whether to add check for missing vals indirect_identifier column
  if (any(c("variable_name", "type", "origin", "status")  %in% names(dict))) {

    missing_vals <- dict %>%
      select(any_of(c("variable_name", "type", "origin", "status"))) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = everything()) %>%
      filter(.data$value %in% c("", NA_character_))

    checks["no_missing_vals"] <- nrow(missing_vals) == 0L

    msgs["no_missing_vals"] <- ifelse(
      checks["no_missing_vals"],
      "OK",
      paste0("- Missing values in column(s): ", paste_collapse(unique(missing_vals$name)))
    )
  }


  ## check that no duplicated values in var variable_name ----------------------
  if ("variable_name" %in% names(dict)) {

    vars_duplicated_varname <- unique(dict$variable_name[duplicated(dict$variable_name)])
    checks["no_duplicated_varname"] <- length(vars_duplicated_varname) == 0L

    msgs["no_duplicated_varname"] <- ifelse(
      checks["no_duplicated_varname"],
      "OK",
      paste0("- Duplicated values in column `variable_name`: ", paste_collapse(vars_duplicated_varname))
    )
  }


  ## check that values of type, origin status, and indirect_identifier are valid
  if (any(c("type", "origin", "status", "indirect_identifier")  %in% names(dict))) {

    non_valid_categories <- dbc::check_categorical(
      dict,
      dict_valid_categories,
      col_allowed_var = "var",
      col_allowed_value = "val"
    ) %>%
      group_by(across("variable")) %>%
      summarize(value = paste_collapse(unique(.data$value)), .groups = "drop") %>%
      mutate(msg = paste0("- Column `", .data$variable, "` has nonvalid value(s): ", .data$value))

    checks["valid_categories"] <- nrow(non_valid_categories) == 0L

    msgs["valid_categories"] <- ifelse(
      checks["valid_categories"],
      "OK",
      paste(non_valid_categories$msg, collapse = "\n   ")
    )
  }


  if ("type" %in% names(dict) & "choices" %in% names(dict)) {

    ## check no missing choices for vars of type "Coded list" ------------------
    rows_missing_choices <- which(is.na(dict$choices) & dict$type == "Coded list")
    checks["no_missing_choices"] <-  length(rows_missing_choices) == 0L

    msgs["no_missing_choices"] <- ifelse(
      checks["no_missing_choices"],
      "OK",
      paste0("- Missing `choices` in data frame row(s): ", paste_collapse(rows_missing_choices, quote = FALSE))
    )


    ## check that all Coded list choices are correctly formatted ---------------
    dict$temp_row_id <- seq_len(nrow(dict))

    choices <- coded_options(dict)

    nonvalid_choices <- choices$value %in% c("", NA_character_) | choices$label %in% c("", NA_character_)
    rows_nonvalid_choices <- unique(choices$temp_row_id[nonvalid_choices])
    rows_nonvalid_choices <- setdiff(rows_nonvalid_choices, rows_missing_choices) # exclude if choice is missing entirely (already captured)

    checks["all_choices_parsable"] <- length(rows_nonvalid_choices) == 0L

    msgs["all_choices_parsable"] <- ifelse(
      checks["all_choices_parsable"],
      "OK",
      paste0("- Incorrectly formatted `choices` in data frame row(s): ", paste_collapse(rows_nonvalid_choices, quote = FALSE))
    )
  }


  ## verbose explanation of checks ---------------------------------------------
  if (verbose) {
    msgs_out <- msgs[!msgs %in% "OK"]

    if (length(msgs_out) > 0L) {
      warning(paste(msgs_out, collapse = "\n"), call. = FALSE)
    }
  }


  ## return --------------------------------------------------------------------
  all(checks, na.rm = TRUE)
}

