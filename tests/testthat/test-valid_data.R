
test_that("valid_data works as expected", {

  # read example dataset
  path_data <- system.file("extdata", package = "datadict")
  dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))
  dict <- dict_from_data(dat)

  # generate data dictionary template from dataset
  dat1 <- dat2 <- dat3 <- dat4 <- dat5 <- dat6 <- dat
  dict1 <- dict2 <- dict3 <- dict4 <- dict5 <- dict6 <- dict

  # returns TRUE when valid
  expect_true(valid_data(dat, dict))

  # withheld variable with non-missing values
  dict1$status[3] <- "withheld"

  expect_false(valid_data(dat1, dict1, verbose = FALSE))
  expect_warning(valid_data(dat1, dict1, verbose = TRUE))

  # columns present in `data` but not defined in `dict`
  dat2$blah <- "test"

  expect_false(valid_data(dat2, dict2, verbose = FALSE))
  expect_warning(valid_data(dat2, dict2, verbose = TRUE))

  # columns defined in `dict` but not present in `data`
  dat3$outcome <- NULL
  dat3$lon <- NULL

  expect_false(valid_data(dat3, dict3, verbose = FALSE))
  expect_warning(valid_data(dat3, dict3, verbose = TRUE))

  # nonvalid numeric
  dat4$age <- as.character(dat4$age)
  dat4$age[3] <- "3 months"
  dat4$age[8] <- "unsure"

  expect_false(valid_data(dat4, dict4, verbose = FALSE))
  expect_warning(valid_data(dat4, dict4, verbose = TRUE))

  # nonvalid date
  dat5$date_outcome <- as.character(dat5$date_outcome)
  dat5$date_outcome[2] <- "feb 2020?"

  expect_false(valid_data(dat5, dict5, verbose = FALSE))
  expect_warning(valid_data(dat5, dict5, verbose = TRUE))

  # nonvalid time (TODO)
  # nonvalid datetime (TODO)

  # nonvalid coded list
  dat6$outcome[4] <- "blahh"
  dat6$age_unit[5] <- "2"

  expect_false(valid_data(dat6, dict6, verbose = FALSE))
  expect_warning(valid_data(dat6, dict6, verbose = TRUE))

})

