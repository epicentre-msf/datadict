
test_that("k_anonymity works as expected", {

  set.seed(340691)

  ll <- data.frame(
    id = 1:100,
    site = sample(1:2, 100, replace = TRUE),
    sex = sample(c("M", "F"), 100, replace = TRUE),
    age_group = sample(c("0-5", "6-14", "15+"), 100, replace = TRUE)
  )

  x1 <- k_anonymity(ll, vars = c("site", "sex", "age_group"), threshold = 10)
  expect_is(x1, "data.frame")
  expect_named(x1, c("site", "sex", "age_group", "k"))
  expect_true(all(x1$k <= 10))

  x2 <- k_anonymity(ll, vars = c("sex", "age_group"), threshold = 15)
  expect_named(x2, c("sex", "age_group", "k"))
  expect_true(all(x2$k <= 15))
})

