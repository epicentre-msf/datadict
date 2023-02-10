#' Check the k-anonymity of one or more variables in a dataset
#'
#' @description
#' Given a dataset and set of one or more variables of interest (e.g. variables
#' that are potential indirect identifiers), the function returns the minimum
#' observed value of k in the dataset, corresponding to the unique combination
#' of identifying variables with the fewest observations.
#'
#' @param x A data frame
#' @param vars A character vector containing the name(s) of the variable(s) in
#'   `x` to be included in the k-anonymity calculation
#'
#' @return
#' The minimum observed value of k in the dataset, corresponding to the
#' unique combination of identifying variables with the fewest observations
#'
#' @examples
#' # read example dataset
#' path_data <- system.file("extdata", package = "datadict")
#' dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
#'
#' # find minimum observed k for potential indirect identifiers gender and age_cat
#' k_anonymity(dat, vars = c("gender", "age_cat"))
#'
#' @importFrom dplyr `%>%` count across all_of
#' @export k_anonymity
k_anonymity <- function(x, vars) {

  if (!all(vars %in% names(x))) {
    stop(
      "The following variables do no exist in the dataset: ",
      paste_collapse(setdiff(vars, names(x))),
      call. = FALSE
    )
  }

  df_k <- x %>%
    count(across(all_of(vars)), name = "k")

  min(df_k$k)
}

