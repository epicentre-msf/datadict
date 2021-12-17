#' Check the k-anonymity of one or more variables in a dataset
#'
#' @description
#' Given a dataset and set of one or more variables of interest (e.g. variables
#' that are potential indirect identifiers), the function returns the
#' combination(s) of those variables yielding counts (i.e. "k") less than a
#' user-specified minimum acceptable threshold value.
#'
#' @param x A data frame
#' @param vars A character vector containing the name(s) of the variable(s) in
#'   `x` to be included in the k-anonymity calculation
#' @param threshold Integer threshold indicating the minimum acceptable value of
#'   k. Combinations with values of k below the threshold will be flagged and
#'   returned. A return with 0 rows indicates that no combinations have values
#'   of k below the threshold.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame containing counts of the
#' variables specified in argument `vars` with counts (i.e. values of k) less
#' than `threshold`.
#'
#' @examples
#' # read example dataset
#' path_data <- system.file("extdata", package = "datadict")
#' dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
#'
#' # check whether there are combinations of gender and age_cat with k < 5
#' k_anonymity(dat, vars = c("gender", "age_cat"), threshold = 5)
#'
#' @importFrom dplyr `%>%` count across all_of arrange filter
#' @importFrom rlang .data .env
#' @export k_anonymity
k_anonymity <- function(x, vars, threshold) {

  x %>%
    dplyr::count(dplyr::across(dplyr::all_of(.env$vars)), name = "k") %>%
    dplyr::arrange(.data$k) %>%
    dplyr::filter(.data$k < threshold)
}

