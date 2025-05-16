# tests/testthat/test-middle-point.R

test_that("middle_point calculates correct midpoint", {
  expect_equal(middle_point(2, 4), 3)
  expect_equal(middle_point(0, 10), 5)
  expect_equal(middle_point(-2, 2), 0)
})

test_that("middle_point returns correct type", {
  result <- middle_point(2, 4)
  expect_type(result, "double")
})

test_that("middle_point is symmetric", {
  # Should give same result regardless of argument order
  expect_equal(middle_point(2, 4), middle_point(4, 2))
})

test_that("middle_point works with equal inputs", {
  expect_equal(middle_point(5, 5), 5)
})



test_that("bi_exponential_knorm returns expected type", {
  result <- bi_exponential_knorm(2, 40)
  expect_type(result, "double")
})

test_that("bi_exponential_knorm is always positive", {
  result1 <- bi_exponential_knorm(2, 40)
  result2 <- bi_exponential_knorm(1, 1)

  expect_true(result1 > 0)
  expect_true(result2 > 0)
})

test_that("bi_exponential_knorm gives different results for different inputs", {
  result1 <- bi_exponential_knorm(2, 40)
  result2 <- bi_exponential_knorm(40, 2)

  expect_false(result1 == result2)
})

test_that("bi_exponential_knorm works with equal inputs", {
  result <- bi_exponential_knorm(1, 1)
  expect_type(result, "double")
})
