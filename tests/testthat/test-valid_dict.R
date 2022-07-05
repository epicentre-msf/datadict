
test_that("valid_dict works as expected", {

  # read example dataset
  path_data <- system.file("extdata", package = "datadict")
  dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))

  # generate data dictionary template from dataset
  d1 <- d2 <- d3 <- d4 <- d5 <-  d6 <- d7 <- d8 <- d9 <- d10 <- dict_from_data(dat, factor_values = "string")

  # returns TRUE when valid
  expect_true(valid_dict(d1))

  # missing value in column 'variable_name'
  d3$variable_name[5] <- NA_character_
  expect_error(valid_dict(d3))

  # duplicated value in column 'variable_name'
  d4$variable_name[c(3, 4)] <- "blah"
  expect_error(valid_dict(d4))

  # duplicated value in column 'type'
  d5$type[5] <- NA_character_
  expect_error(valid_dict(d5))

  # non-valid value in column 'type'
  d6$type[5] <- "Blah"
  expect_error(valid_dict(d6))

  # missing value in column 'choices'
  d7$choices[d7$type == "Coded list"][1] <- NA_character_
  expect_error(valid_dict(d7))

  # non-valid value in column 'choices'
  d8$choices[d8$type == "Coded list"][1] <- "1, | "
  expect_error(valid_dict(d8))

  # non-valid value in column 'origin'
  d9$origin[4] <- "blah"
  expect_error(valid_dict(d9))

  # non-valid value in column 'status'
  d10$status[5] <- NA_character_
  expect_error(valid_dict(d10))

})

