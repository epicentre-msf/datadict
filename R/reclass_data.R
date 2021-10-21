#' Reclass the columns of a dataset to match the variable types specified in the
#' corresponding data dictionary
#'
#' @param data A data frame reflecting a dataset to be shared
#' @param dict A data frame reflecting the corresponding data dictionary
#' @param format_date Expected format for date variables. Defaults to
#'   "%Y-%m-%d".
#' @param format_time Expected format for date variables. Defaults to
#'   "%H:%M:%S".
#' @param format_datetime Expected format for date variables. Defaults to `NULL`
#'   to use defaults in [lubridate::as_datetime].
#'
#' @return
#' The dataset with columns reclassed to match the data dictionary
#'
#' @importFrom dplyr `%>%`
#' @export reclass_data
reclass_data <- function(data,
                         dict,
                         format_date = "%Y-%m-%d",
                         format_time = "%H:%M:%S",
                         format_datetime = NULL) {

  check_dict <- valid_dict(dict)

  data %>%
    reclass(dict, "Numeric", format_date, format_time, format_datetime) %>%
    reclass(dict, "Date", format_date, format_time, format_datetime) %>%
    reclass(dict, "Time", format_date, format_time, format_datetime) %>%
    reclass(dict, "Datetime", format_date, format_time, format_datetime)
}



#' @noRd
#' @importFrom lubridate as_date as_datetime
reclass <- function(data, dict, type, format_date, format_time, format_datetime) {

  fn_class <- switch(
    type,
    "Numeric" = function(x) as.numeric(x),
    "Date" = function(x) lubridate::as_date(x, format = format_date),
    "Time" = function(x) as.POSIXct(x, format = format_time),
    "Datetime" = function(x) lubridate::as_datetime(x)
  )

  vars_focal <- dict$variable_name[dict$type == type]
  vars_focal <- intersect(vars_focal, names(data))

  out <- data

  for (var in vars_focal) {
    data[[var]] <- suppressWarnings(fn_class(data[[var]]))
  }

  return(data)
}

