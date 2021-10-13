#' Check that a dataset is consistent with its corresponding data dictionary
#'
#' @description
#' Includes the following checks:
#' - all variables in dataset defined in dictionary
#' - all variables defined in dictionary present in dataset
#' - variables of type 'Numeric' are valid numbers
#' - variables of type 'Date' are valid dates
#' - variables of type 'Time' are valid times
#' - variables of type 'Datetimes' are valid date-times
#' - variables of type 'Coded list' contain only allowed options
#'
#' @param data A data frame reflecting a dataset to be shared
#' @param dict A data frame reflecting the corresponding data dictionary
#' @param format_date Expected format for date variables. Defaults to
#'   "%Y-%m-%d".
#' @param format_time Expected format for date variables. Defaults to
#'   "%H:%M:%S".
#' @param format_datetime Expected format for date variables. Defaults to `NULL`
#'   to use defaults in [lubridate::as_datetime].
#' @param format_coded Expected format for coded-list variables, either "value"
#'   or "label". Defaults to "label.
#'
#' @return
#' NULL. Prints checks and their results to the console.
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
#' valid_data(dat, dict)
#'
#' @importFrom dplyr `%>%` mutate filter select anti_join everything
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data .env
#' @importFrom crayon make_style
#' @export valid_data
valid_data <- function(data,
                       dict,
                       format_date = "%Y-%m-%d",
                       format_time = "%H:%M:%S",
                       format_datetime = NULL,
                       format_coded = "label") {

  format_coded <- match.arg(format_coded, c("value", "label"))

  # define crayon colors
  white <- crayon::make_style("white")

  # check dictionary
  check_dict <- valid_dict(dict)

  # extra variables in data
  names_extra <- setdiff(names(data), dict$variable_name)

  msg_extra <- ifelse(
    length(names_extra) > 0,
    paste_collapse_c(names_extra),
    "OK"
  )

  message(white("Checking for columns present in `data` but not defined in `dict`:\n   ", msg_extra))

  # missing variables in data
  names_missing <- setdiff(dict$variable_name, names(data))

  msg_missing <- ifelse(
    length(names_missing) > 0,
    paste_collapse_c(names_missing),
    "OK"
  )

  message(white("Checking for columns defined in `dict` but not present in `data`:\n   ", msg_missing))

  # subset data and dict to shared vars
  data <- data[,names(data) %in% dict$variable_name, drop = FALSE]
  dict <- dict[dict$variable_name %in% names(data),]

  ### valid column types -------------------------------------------------------

  # valid numeric
  out_check_numeric <- check_class(data, dict, "Numeric")

  msg_numeric <- ifelse(
    !is.null(out_check_numeric) && nrow(out_check_numeric) > 0,
    paste_collapse_c(unique(out_check_numeric$variable_name)),
    "OK"
  )

  message(white("Checking for problems with variables of type 'Numeric':\n   ", msg_numeric))

  # valid date
  out_check_date <- check_class(data, dict, "Date", format_date = format_date)

  msg_date <- ifelse(
    !is.null(out_check_date) && nrow(out_check_date) > 0,
    paste_collapse_c(unique(out_check_date$variable_name)),
    "OK"
  )

  message(white("Checking for problems with variables of type 'Date':\n   ", msg_date))

  # valid time
  out_check_time <- check_class(data, dict, "Time", format_time = format_time)

  msg_time <- ifelse(
    !is.null(out_check_time) && nrow(out_check_time) > 0,
    paste_collapse_c(unique(out_check_time$variable_name)),
    "OK"
  )

  message(white("Checking for problems with variables of type 'Time':\n   ", msg_time))

  # valid datetime
  out_check_datetime <- check_class(data, dict, "Datetime", format_datetime = format_datetime)

  msg_datetime <- ifelse(
    !is.null(out_check_datetime) && nrow(out_check_datetime) > 0,
    paste_collapse_c(unique(out_check_datetime$variable_name)),
    "OK"
  )

  message(white("Checking for problems with variables of type 'Datetime':\n   ", msg_datetime))

  # valid coded list
  dict_coded <- coded_options(dict)

  vars_coded <- dict$variable_name[dict$type %in% "Coded list"]

  out_check_coded <- data %>%
    dplyr::select(dplyr::any_of(vars_coded)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable_name") %>%
    dplyr::anti_join(dict_coded, by = c("variable_name", "value" = .env$format_coded)) %>%
    filter(!is.na(.data$value)) %>%
    unique()

  msg_coded <- ifelse(
    nrow(out_check_coded) > 0,
    paste_collapse_c(unique(out_check_coded$variable_name)),
    "OK"
  )

  message(white("Checking for problems with variables of type 'Coded list':\n   ", msg_coded))
}



#' @noRd
check_numeric <- function(x) {
  xnum <- suppressWarnings(as.numeric(x))
  length(is.na(xnum)) == length(is.na(x))
}


#' Convert REDCap time variables to chron 'times' class
#'
#' @noRd
#' @importFrom chron times
parse_redcap_time <- function(x) {
  x <- gsub("(^\\d{2}:\\d{2}$)", "\\1:00", x)
  # line below to avoid weird behavior when testing for NA in a chron times
  # column that contains all NA (e.g. within apply)
  if (all(is.na(x))) x <- as.numeric(x)
  chron::times(x, format = c(times = "h:m:s"))
}


#' @noRd
#' @importFrom lubridate as_date as_datetime
#' @importFrom dplyr mutate filter select everything
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
check_class <- function(data, dict, type, format_date, format_time, format_datetime) {
  vars_focal <- dict$variable_name[dict$type == type]

  fn_class <- switch(
    type,
    "Numeric" = function(x) as.numeric(x),
    "Date" = function(x) lubridate::as_date(x, format = format_date),
    "Time" = function(x) as.POSIXct(x, format = format_time),
    "Datetime" = function(x) lubridate::as_datetime(x, format = format_datetime)
  )

  if (length(vars_focal) > 0) {
    data[, vars_focal, drop = FALSE] %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable_name") %>%
      dplyr::mutate(value_class = suppressWarnings(fn_class(.data$value))) %>%
      dplyr::filter(is.na(.data$value_class), !is.na(.data$value)) %>%
      dplyr::select(-.data$value_class) %>%
      unique()
  }
}

