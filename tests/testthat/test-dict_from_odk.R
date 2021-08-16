
test_that("dict_from_odk works as expected", {

  path_data <- system.file("extdata", package = "datadict")
  path_odk_template <- file.path(path_data, "WHOVA2016_v1_5_3_ODK.xlsx")
  odk_survey <- readxl::read_xlsx(path_odk_template, sheet = "survey")
  odk_choices <- readxl::read_xlsx(path_odk_template, sheet = "choices")

  x1 <- dict_from_odk(odk_survey, odk_choices)

  # expect type tibble
  expect_s3_class(x1, "tbl_df")

  # expect has required cols
  cols_req <- c("variable_name", "short_label", "origin", "type", "choices")
  expect_true(all(cols_req %in% names(x1)))

  # expect valid data types
  types_valid <- c("Numeric", "Date", "Time", "Datetime", "Coded list", "Free text")
  expect_true(all(x1$type %in% types_valid))

})

