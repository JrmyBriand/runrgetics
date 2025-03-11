test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("cost_running works", {
  expect_equal(cost_running(acceleration = c(0, 0, 0), velocity = c(0, 0, 0)), c(3.8, 3.8, 3.8))
})

test_that("cost_running returns a vector", {
  expect_type(cost_running(acceleration = c(2, 4, 5), velocity = c(2, 4, 5)), "double")
})

test_that("cost_running returns a vector of the same length as the input", {
  expect_equal(length(cost_running(acceleration = c(2, 4, 5), velocity = c(2, 4, 5))), 3)
})


test_that("cost_running_sprint works", {
  expect_equal(cost_running_sprint(acceleration = c(0, 0, 0), velocity = c(0, 0, 0)), c(3.8, 3.8, 3.8))
})

test_that("cost_running_sprint returns a vector", {
  expect_type(cost_running_sprint(acceleration = c(2, 4, 5), velocity = c(2, 4, 5)), "double")
})


test_that("cost_running_sprint returns a vector of the same length as the input", {
  expect_equal(length(cost_running_sprint(acceleration = c(2, 4, 5), velocity = c(2, 4, 5))), 3)
})


test_that("cost_running_sprint negative acceleration returns a negative acceleration", {
  expect_equal(cost_running_sprint(acceleration = -1, velocity = 1), 3.81)
})
