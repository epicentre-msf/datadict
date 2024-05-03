
test_that("dict_from_data works as expected", {

  # read example ll
  path_data <- system.file("extdata", package = "datadict")
  path_linelist <- file.path(path_data, "linelist_cleaned.xlsx")
  ll <- readxl::read_xlsx(path_linelist)

  # default args
  x1 <- dict_from_data(ll)

  # expect type tibble
  expect_s3_class(x1, "tbl_df")

  # expect has required cols
  cols_req <- c("variable_name", "short_label", "origin", "type", "choices")
  expect_true(all(cols_req %in% names(x1)))

  # expect valid data types
  types_valid <- c("Logical", "Numeric", "Date", "Time", "Datetime", "Coded list", "Free text")
  expect_true(all(x1$type %in% types_valid))

  # test arg factor_values
  x2 <- dict_from_data(ll, factor_values = "string")

  expect_equal(x1$choices[x1$variable_name == "outcome"], "0, Death | 1, Recover")
  expect_equal(x2$choices[x2$variable_name == "outcome"], "death, Death | recover, Recover")

  # test arg factor_threshold
  x3 <- dict_from_data(ll, factor_threshold = 3)

  expect_equal(x1$type[x1$variable_name == "age_cat"], "Coded list")
  expect_equal(x3$type[x3$variable_name == "age_cat"], "Free text")

})

