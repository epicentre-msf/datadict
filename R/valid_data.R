#' Check that a dataset is consistent with its corresponding data dictionary
#'
#' @description
#' Includes the following checks:
#' - variables marked 'withheld' in dictionary contain only missing values
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
#' valid_data(dat, dict)
#'
#' @importFrom dplyr `%>%` mutate filter select anti_join everything
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export valid_data
valid_data <- function(data,
                       dict,
                       format_date = "%Y-%m-%d",
                       format_time = "%H:%M:%S",
                       format_datetime = NULL,
                       format_coded = "label",
                       verbose = TRUE) {


  format_coded <- match.arg(format_coded, c("value", "label"))

  ## prep check and msg outputs ------------------------------------------------
  checks <- c(
    withheld  = as.logical(NA),
    extra     = as.logical(NA),
    missing   = as.logical(NA),
    numeric   = as.logical(NA),
    date      = as.logical(NA),
    time      = as.logical(NA),
    datetime  = as.logical(NA),
    coded     = as.logical(NA)
  )

  msgs <- c(
    withheld  = "Unable to test",
    extra     = "Unable to test",
    missing   = "Unable to test",
    numeric   = "Unable to test",
    date      = "Unable to test",
    time      = "Unable to test",
    datetime  = "Unable to test",
    coded     = "Unable to test"
  )


  ## check dictionary ----------------------------------------------------------
  if (!valid_dict(dict, verbose = FALSE)) {
    stop("Dictionary does not pass all checks", call. = FALSE)
  }


  ## variable marked withheld with non-missing values --------------------------
  vars_withheld <- dict$variable_name[dict$status %in% "withheld"]
  vars_withheld_all_missing <- vapply(data[,vars_withheld], function(x) all(is.na(x)), FALSE)
  vars_withheld_not_all_missing <- names(which(!vars_withheld_all_missing))

  checks[["withheld"]] <- length(vars_withheld_not_all_missing) == 0L

  msgs[["withheld"]] <- ifelse(
    checks[["withheld"]],
    "OK",
    paste0("- Columns with status 'withheld' that contain non-missing values: ", paste_collapse(vars_withheld_not_all_missing))
  )


  ## Columns present in `data` but not defined in `dict` -----------------------
  names_extra <- setdiff(names(data), dict$variable_name)

  checks[["extra"]] <- length(names_extra) == 0L

  msgs[["extra"]] <- ifelse(
    checks[["extra"]],
    "OK",
    paste0("- Columns present in `data` but not defined in `dict`: ", paste_collapse(names_extra))
  )


  ## Columns defined in `dict` but not present in `data` -----------------------
  names_missing <- setdiff(dict$variable_name, names(data))

  checks[["missing"]] <- length(names_missing) == 0L

  msgs[["missing"]] <- ifelse(
    checks[["missing"]],
    "OK",
    paste0("- Columns defined in `dict` but not present in `data`: ", paste_collapse(names_missing))
  )


  ## for remainder of tests, subset data and dict to common vars ---------------
  data <- data[,names(data) %in% dict$variable_name, drop = FALSE]
  dict <- dict[dict$variable_name %in% names(data),]


  ## valid logical -------------------------------------------------------------
  out_check_logical <- check_class(data, dict, "Logical")

  checks[["logical"]] <- is.null(out_check_logical) || nrow(out_check_logical) == 0L

  msgs[["logical"]] <- ifelse(
    checks[["logical"]],
    "OK",
    paste0("- Variables of type 'Logical' contain nonvalid values: ", paste_collapse(unique(out_check_logical$variable_name)))
  )


  ## valid numeric -------------------------------------------------------------
  out_check_numeric <- check_class(data, dict, "Numeric")

  checks[["numeric"]] <- is.null(out_check_numeric) || nrow(out_check_numeric) == 0L

  msgs[["numeric"]] <- ifelse(
    checks[["numeric"]],
    "OK",
    paste0("- Variables of type 'Numeric' contain nonvalid values: ", paste_collapse(unique(out_check_numeric$variable_name)))
  )


  ## valid date ----------------------------------------------------------------
  out_check_date <- check_class(data, dict, "Date", format_date = format_date)

  checks[["date"]] <- is.null(out_check_date) || nrow(out_check_date) == 0L

  msgs[["date"]] <- ifelse(
    checks[["date"]],
    "OK",
    paste0("- Variables of type 'Date' contain nonvalid values: ", paste_collapse(unique(out_check_date$variable_name)))
  )


  ## valid time ----------------------------------------------------------------
  out_check_time <- check_class(data, dict, "Time", format_time = format_time)

  checks[["time"]] <- is.null(out_check_time) || nrow(out_check_time) == 0L

  msgs[["time"]] <- ifelse(
    checks[["time"]],
    "OK",
    paste0("- Variables of type 'Time' contain nonvalid values: ", paste_collapse(unique(out_check_time$variable_name)))
  )

  ## valid datetime ------------------------------------------------------------
  out_check_datetime <- check_class(data, dict, "Datetime", format_datetime = format_datetime)

  checks[["datetime"]] <- is.null(out_check_datetime) || nrow(out_check_datetime) == 0L

  msgs[["datetime"]] <- ifelse(
    checks[["datetime"]],
    "OK",
    paste0("- Variables of type 'Datetime' contain nonvalid values: ", paste_collapse(unique(out_check_datetime$variable_name)))
  )


  ## valid coded list ----------------------------------------------------------
  dict_coded <- coded_options(dict)

  vars_coded <- dict$variable_name[dict$type %in% "Coded list"]

  out_check_coded <- data %>%
    select(any_of(vars_coded)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols = everything(), names_to = "variable_name") %>%
    anti_join(dict_coded, by = c("variable_name", "value" = format_coded)) %>%
    filter(!is.na(.data$value)) %>%
    unique()

  checks[["coded"]] <- is.null(out_check_coded) || nrow(out_check_coded) == 0L

  msgs[["coded"]] <- ifelse(
    checks[["coded"]],
    "OK",
    paste0("- Variables of type 'Coded list' contain nonvalid values: ", paste_collapse(unique(out_check_coded$variable_name)))
  )


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
    "Logical" = function(x) as.logical(x),
    "Numeric" = function(x) as.numeric(x),
    "Date" = function(x) lubridate::as_date(x, format = format_date),
    "Time" = function(x) as.POSIXct(x, format = format_time),
    "Datetime" = function(x) lubridate::as_datetime(x, format = format_datetime)
  )

  if (length(vars_focal) > 0) {
    data[, vars_focal, drop = FALSE] %>%
      mutate(across(everything(), as.character)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "variable_name") %>%
      mutate(value_class = suppressWarnings(fn_class(.data$value))) %>%
      filter(is.na(.data$value_class), !is.na(.data$value)) %>%
      select(!any_of("value_class")) %>%
      unique()
  }
}

