
#' Standardize column names
#' @noRd
#' @importFrom stringr str_to_sentence
# standardize_colnames <- function(x) {
#   stringr::str_to_sentence(gsub("_+", " ", x))
# }


#' @noRd
empty_to_na <- function(x) {
  for (j in  seq_len(ncol(x))) {
    if (is.character(x[[j]])) {
      x[[j]][x[[j]] == ""] <- NA_character_
    }
  }
  x
}

