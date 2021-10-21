
test_that("generate_coded_options works as expected", {

  x <- c("Label 1", "Label 2", "Label 3")
  names(x) = c("v1", "v2", "v3")

  expect_equal(
    generate_coded_options(x, type_of_values = "int"),
    "1, Label 1 | 2, Label 2 | 3, Label 3"
  )

  expect_equal(
    generate_coded_options(x, type_of_values = "string"),
    "label_1, Label 1 | label_2, Label 2 | label_3, Label 3"
  )

  expect_equal(
    generate_coded_options(x, type_of_values = "names"),
    "v1, Label 1 | v2, Label 2 | v3, Label 3"
  )

  expect_error(
    generate_coded_options(unname(x), type_of_values = "names")
  )
})

