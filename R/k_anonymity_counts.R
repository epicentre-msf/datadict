#' Count the number of observations across unique combinations of indirect
#' identifiers within a dataset
#'
#' @description
#' Given a dataset and set of one or more variables that may be indirect
#' identifiers, the function returns a table of counts of the number of
#' observations corresponding to each unique combination of those variables
#' (i.e. k), optionally filtered to those combinations that do not meet the
#' user-specific threshhold of k-anonymity.
#'
#' @inheritParams k_anonymity
#'
#' @param threshold Integer threshold indicating the minimum acceptable value of
#'   k. Combinations with values of k below the threshold will be flagged and
#'   returned. A return with 0 rows indicates that no combinations have values
#'   of k below the threshold.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame containing counts of unique
#' combinations of the variables specified in argument `vars`. If argument
#' `threshold` is specified, only the combinations with counts lower than the
#' threshold are returned, if any (i.e. combinations that do not meet the
#' specified value of k-anonymity).
#'
#' @examples
#' # read example dataset
#' path_data <- system.file("extdata", package = "datadict")
#' dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
#'
#' # display combinations of gender and age_cat with k < 5
#' k_anonymity_counts(dat, vars = c("gender", "age_cat"), threshold = 5)
#'
#' @importFrom dplyr `%>%` count across all_of arrange filter
#' @importFrom rlang .data
#' @export k_anonymity_counts
k_anonymity_counts <- function(x, vars, threshold = NULL) {

  if (!all(vars %in% names(x))) {
    stop(
      "The following variables do no exist in the dataset: ",
      paste_collapse(setdiff(vars, names(x))),
      call. = FALSE
    )
  }

  df_counts <- x %>%
    count(across(all_of(vars)), name = "k") %>%
    arrange(.data$k)

  if (!is.null(threshold)) {
    df_counts <- df_counts %>%
      filter(.data$k < threshold)
  }

  return(df_counts)
}

