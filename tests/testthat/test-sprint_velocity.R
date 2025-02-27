

test_that("find_time_velocity works with typical sprint data", {
  splits <- c(0, 1.88, 2.88, 3.78, 4.64, 5.47, 6.29, 7.10, 7.92, 8.74, 9.58)
  reaction_time <- 0.173

  result <- find_time_velocity(splits, reaction_time)

  expect_type(result, "double")
  expect_length(result, length(splits))
  expect_equal(result[1], reaction_time)
  expect_true(all(result >= reaction_time))
  expect_true(all(diff(result) >= 0)) # Times should be monotonically increasing
})

test_that("find_terminal_velocity calculates correct terminal velocity", {
  splits <- c(8.74, 9.58)
  velocity <- c(12.17, 11.96)
  distance <- c(90, 100)
  reaction_time <- 0.146

  result <- find_terminal_velocity(splits, velocity, distance, reaction_time)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(!is.na(result))
  expect_true(is.finite(result))
})


