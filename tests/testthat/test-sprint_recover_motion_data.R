# tests/testthat/test-sprint-recover.R

test_that("sprint_recover_motion returns expected structure", {
  # Create simple power series
  power_series <- rep(50, 100) # 1 second of constant power at dt = 0.01

  result <- sprint_recover_motion(power_series, cost_running_flat = 3.8)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("time", "distance", "velocity", "acceleration", "power"))

  # Check lengths
  expect_equal(nrow(result), length(power_series))

  # Check types
  expect_type(result$time, "double")
  expect_type(result$distance, "double")
  expect_type(result$velocity, "double")
  expect_type(result$acceleration, "double")
  expect_type(result$power, "double")
})

test_that("sprint_recover_distance returns expected type", {
  # Create simple power series
  power_series <- rep(50, 100)

  result <- sprint_recover_distance(power_series, cost_running_flat = 3.8)

  # Check return type
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gte(result, 0) # Distance should be non-negative
})

test_that("sprint_modeled_distance_percentage_error returns expected type", {
  # Create mock sprint power data
  mock_data <- tibble::tibble(
    time = seq(0, 0.99, by = 0.01),
    power = rep(50, 100),
    power_mod = rep(55, 100) # Slightly different power for testing
  )

  result <- sprint_modeled_distance_percentage_error(mock_data, cost_running_flat = 3.8)

  # Check return type
  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("sprint_recover_motion handles zero power", {
  # Test with zero power
  power_series <- rep(0, 100)

  result <- sprint_recover_motion(power_series, cost_running_flat = 3.8)

  # Should have near-zero velocity and acceleration (allowing for numerical precision)
  expect_equal(sum(result$velocity), 0, tolerance = 1e-3)
  expect_equal(sum(result$acceleration), 0, tolerance = 1e-3)
})

test_that("functions respect dt parameter", {
  power_series <- rep(50, 200)
  dt1 <- 0.01
  dt2 <- 0.02

  # Test with different dt
  result1 <- sprint_recover_motion(power_series, dt = dt1)
  result2 <- sprint_recover_motion(power_series, dt = dt2)

  # Check that time vectors are different
  expect_false(identical(result1$time, result2$time))

  # Check that time steps are correct
  expect_equal(diff(result1$time)[1], dt1)
  expect_equal(diff(result2$time)[1], dt2)
})
