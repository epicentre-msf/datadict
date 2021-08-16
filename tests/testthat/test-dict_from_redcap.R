
test_that("dict_from_redcap works as expected", {

  path_data <- system.file("extdata", package = "datadict")

  path_dict_raw <- file.path(path_data, "dict_redcap_raw.csv")
  path_dict_pkg <- file.path(path_data, "dict_redcap_pkg.csv")

  dict_raw <- read.csv(path_dict_raw)
  dict_pkg <- read.csv(path_dict_pkg)

  x1 <- dict_from_redcap(dict_raw)
  x2 <- dict_from_redcap(dict_pkg)

  # expect type tibble
  expect_s3_class(x1, "tbl_df")
  expect_s3_class(x2, "tbl_df")

  # expect has required cols
  cols_req <- c("variable_name", "form_or_group", "short_label", "origin", "type", "choices")
  expect_true(all(cols_req %in% names(x1)))
  expect_true(all(cols_req %in% names(x2)))

  # expect valid data types
  types_valid <- c("Numeric", "Date", "Time", "Datetime", "Coded list", "Free text")
  expect_true(all(x1$type %in% types_valid))
  expect_true(all(x2$type %in% types_valid))

})

