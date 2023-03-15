
test_that("valid_dict works as expected", {

  # read example dataset
  path_data <- system.file("extdata", package = "datadict")
  dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))

  # generate data dictionary template from dataset
  d1 <- dict_from_data(dat, factor_values = "string")
  d1$indirect_identifier <- rep("no", nrow(d1))

  d2 <- d3 <- d4 <- d5 <-  d6 <- d7 <- d8 <- d9 <- d10 <- d11 <- d12 <- d13 <- d14 <- d1

  # returns TRUE when valid
  expect_true(valid_dict(d1))

  # missing value in column 'variable_name'
  d3$variable_name[5] <- NA_character_
  expect_false(valid_dict(d3, verbose = FALSE))
  expect_warning(valid_dict(d3, verbose = TRUE))

  # duplicated value in column 'variable_name'
  d4$variable_name[c(3, 4)] <- "blah"
  expect_false(valid_dict(d4, verbose = FALSE))
  expect_warning(valid_dict(d4, verbose = TRUE))

  # missing value in column 'type'
  d5$type[5:7] <- NA_character_
  expect_false(valid_dict(d5, verbose = FALSE))
  expect_warning(valid_dict(d5, verbose = TRUE))

  # non-valid value in column 'type'
  d6$type[5] <- "Blah"
  expect_false(valid_dict(d6, verbose = FALSE))
  expect_warning(valid_dict(d6, verbose = TRUE))

  # missing value in column 'choices'
  d7$choices[d7$type == "Coded list"][1] <- NA_character_
  expect_false(valid_dict(d7, verbose = FALSE))
  expect_warning(valid_dict(d7, verbose = TRUE))

  # non-valid value in column 'choices'
  d8$choices[d8$type == "Coded list"][1] <- "1, | "
  expect_false(valid_dict(d8, verbose = FALSE))
  expect_warning(valid_dict(d8, verbose = TRUE))

  # missing value in column 'origin'
  d9$origin[11] <- NA_character_
  expect_false(valid_dict(d9, verbose = FALSE))
  expect_warning(valid_dict(d9, verbose = TRUE))

  # non-valid value in column 'origin'
  d10$origin[11] <- "blah"
  expect_false(valid_dict(d10, verbose = FALSE))
  expect_warning(valid_dict(d10, verbose = TRUE))

  # missing value in column 'status'
  d11$status[5] <- NA_character_
  expect_false(valid_dict(d11, verbose = FALSE))
  expect_warning(valid_dict(d11, verbose = TRUE))

  # non-valid value in column 'status'
  d12$status[5] <- "blah"
  expect_false(valid_dict(d12, verbose = FALSE))
  expect_warning(valid_dict(d12, verbose = TRUE))

  # missing value in column 'indirect_identifier'
  d13$indirect_identifier[9] <- NA_character_
  expect_false(valid_dict(d13, verbose = FALSE))
  expect_warning(valid_dict(d13, verbose = TRUE))

  # non-valid value in column 'indirect_identifier'
  d14$indirect_identifier[9] <- "blah"
  expect_false(valid_dict(d14, verbose = FALSE))
  expect_warning(valid_dict(d14, verbose = TRUE))

})

