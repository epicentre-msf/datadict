
test_that("k_anonymity and k_anonymity_counts work as expected", {

  ll <- data.frame(
    id = 1:10,
    site      = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b"),
    sex       = c("m", "m", "f", "m", "f", "f", "m", "m", "f", "f"),
    age_group = c("1", "1", "2", "2", "2", "2", "1", "2", "2", "1")
  )

  # k_anonymity
  x1 <- k_anonymity(ll, vars = c("site", "sex", "age_group"))
  expect_equal(x1, 1L)

  x2 <- k_anonymity(ll, vars = c("site", "sex"))
  expect_equal(x2, 2L)

  x3 <- k_anonymity(ll, vars = c("site"))
  expect_equal(x3, 4L)

  expect_error(k_anonymity(ll, vars = c("does_not_exist")))

  # k_anonymity_counts
  y1 <- k_anonymity_counts(ll, vars = c("site", "sex", "age_group"))
  expect_is(y1, "data.frame")
  expect_named(y1, c("site", "sex", "age_group", "k"))

})

