# test-energy-total.R
library(tibble)

test_that("energy_total calculates the integral correctly for both power types", {
  # Test with the example from the documentation
  test_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 1, 2, 1, 0),
    power_mod = c(0, 1, 2, 1, 0) # Adding modeled power
  )

  # Expected result using trapezoidal rule: (0+1)/2 + (1+2)/2 + (2+1)/2 + (1+0)/2 = 0.5 + 1.5 + 1.5 + 0.5 = 4
  expect_equal(energy_total(test_data, type = "power"), 4)
  expect_equal(energy_total(test_data, type = "power bioenergetic model"), 4)
})

test_that("energy_total handles unsorted data for both power types", {
  # Test with unsorted data
  unsorted_data <- tibble::tibble(
    time = c(2, 0, 4, 1, 3),
    power = c(2, 0, 0, 1, 1),
    power_mod = c(2, 0, 0, 1, 1)
  )

  # The function should sort this internally and produce the same result as sorted data
  sorted_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 1, 2, 1, 0),
    power_mod = c(0, 1, 2, 1, 0)
  )

  expect_equal(
    energy_total(unsorted_data, type = "power"),
    energy_total(sorted_data, type = "power")
  )
  expect_equal(
    energy_total(unsorted_data, type = "power bioenergetic model"),
    energy_total(sorted_data, type = "power bioenergetic model")
  )
})

test_that("energy_total works with constant power for both power types", {
  # Test with constant power
  constant_data <- tibble::tibble(
    time = c(0, 1, 2, 3),
    power = c(5, 5, 5, 5),
    power_mod = c(5, 5, 5, 5)
  )

  # Expected: 5 * (3-0) = 15
  expect_equal(energy_total(constant_data, type = "power"), 15)
  expect_equal(energy_total(constant_data, type = "power bioenergetic model"), 15)
})

test_that("energy_total handles single point data for both power types", {
  # Test with a single data point (should return 0)
  single_point <- tibble::tibble(
    time = 1,
    power = 10,
    power_mod = 10
  )

  # With only one point, the integral should be 0
  expect_equal(energy_total(single_point, type = "power"), 0)
  expect_equal(energy_total(single_point, type = "power bioenergetic model"), 0)
})

test_that("energy_total handles invalid power type", {
  test_data <- tibble::tibble(
    time = c(0, 1),
    power = c(0, 1),
    power_mod = c(0, 1)
  )

  # Should error with invalid type
  expect_error(energy_total(test_data, type = "invalid"))
})


# test-energy-functions.R

test_that("energy_aerobic calculates the integral correctly", {
  # Test with the example from the documentation
  test_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power_aerobic = c(0, 1, 2, 1, 0)
  )

  # Expected result using trapezoidal rule: (0+1)/2 + (1+2)/2 + (2+1)/2 + (1+0)/2 = 0.5 + 1.5 + 1.5 + 0.5 = 4
  expect_equal(energy_aerobic(test_data), 4)

  # Test with unsorted data
  unsorted_data <- tibble::tibble(
    time = c(2, 0, 4, 1, 3),
    power_aerobic = c(2, 0, 0, 1, 1)
  )
  expect_equal(energy_aerobic(unsorted_data), 4)

  # Test with constant power
  constant_data <- tibble::tibble(
    time = c(0, 1, 2, 3),
    power_aerobic = c(5, 5, 5, 5)
  )
  expect_equal(energy_aerobic(constant_data), 15)
})

test_that("energy_anaerobic calculates the integral correctly", {
  # Test with the example from the documentation
  test_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power_anaerobic = c(0, 1, 2, 1, 0)
  )

  expect_equal(energy_anaerobic(test_data), 4)

  # Test with unsorted data
  unsorted_data <- tibble::tibble(
    time = c(2, 0, 4, 1, 3),
    power_anaerobic = c(2, 0, 0, 1, 1)
  )
  expect_equal(energy_anaerobic(unsorted_data), 4)

  # Test with constant power
  constant_data <- tibble::tibble(
    time = c(0, 1, 2, 3),
    power_anaerobic = c(5, 5, 5, 5)
  )
  expect_equal(energy_anaerobic(constant_data), 15)
})

test_that("energy_lactic calculates the integral correctly", {
  # Test with the example from the documentation
  test_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power_lactic = c(0, 1, 2, 1, 0)
  )

  expect_equal(energy_lactic(test_data), 4)

  # Test with unsorted data
  unsorted_data <- tibble::tibble(
    time = c(2, 0, 4, 1, 3),
    power_lactic = c(2, 0, 0, 1, 1)
  )
  expect_equal(energy_lactic(unsorted_data), 4)

  # Test with constant power
  constant_data <- tibble::tibble(
    time = c(0, 1, 2, 3),
    power_lactic = c(5, 5, 5, 5)
  )
  expect_equal(energy_lactic(constant_data), 15)
})

test_that("energy_alactic calculates the integral correctly", {
  # Test with the example from the documentation
  test_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power_alactic = c(0, 1, 2, 1, 0)
  )

  expect_equal(energy_alactic(test_data), 4)

  # Test with unsorted data
  unsorted_data <- tibble::tibble(
    time = c(2, 0, 4, 1, 3),
    power_alactic = c(2, 0, 0, 1, 1)
  )
  expect_equal(energy_alactic(unsorted_data), 4)

  # Test with constant power
  constant_data <- tibble::tibble(
    time = c(0, 1, 2, 3),
    power_alactic = c(5, 5, 5, 5)
  )
  expect_equal(energy_alactic(constant_data), 15)
})

test_that("all energy functions handle single point data", {
  # Test with a single data point (should return 0)
  single_point_aerobic <- tibble::tibble(
    time = 1,
    power_aerobic = 10
  )
  expect_equal(energy_aerobic(single_point_aerobic), 0)

  single_point_anaerobic <- tibble::tibble(
    time = 1,
    power_anaerobic = 10
  )
  expect_equal(energy_anaerobic(single_point_anaerobic), 0)

  single_point_lactic <- tibble::tibble(
    time = 1,
    power_lactic = 10
  )
  expect_equal(energy_lactic(single_point_lactic), 0)

  single_point_alactic <- tibble::tibble(
    time = 1,
    power_alactic = 10
  )
  expect_equal(energy_alactic(single_point_alactic), 0)
})

test_that("all energy functions handle additional columns", {
  # Test that functions work when additional columns are present
  data_with_extra_cols <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power_aerobic = c(0, 1, 2, 1, 0),
    power_anaerobic = c(0, 2, 4, 2, 0),
    power_lactic = c(0, 1.5, 3, 1.5, 0),
    power_alactic = c(0, 0.5, 1, 0.5, 0),
    extra_col = c("a", "b", "c", "d", "e")
  )

  expect_equal(energy_aerobic(data_with_extra_cols), 4)
  expect_equal(energy_anaerobic(data_with_extra_cols), 8)
  expect_equal(energy_lactic(data_with_extra_cols), 6)
  expect_equal(energy_alactic(data_with_extra_cols), 2)
})

library(testthat)
library(tibble)

# Mock the underlying energy functions that are called by the percentage functions
# These aren't defined in the provided code, so we'll mock them for testing
energy_total <- function(data) {
  # Simple implementation - sum of power * time intervals
  sum(data$power * c(diff(data$time), 0))
}

energy_alactic <- function(data) {
  # Simple implementation - sum of alactic power * time intervals
  sum(data$power_alactic * c(diff(data$time), 0))
}

energy_lactic <- function(data) {
  # Simple implementation - sum of lactic power * time intervals
  sum(data$power_lactic * c(diff(data$time), 0))
}

energy_aerobic <- function(data) {
  # Simple implementation - sum of aerobic power * time intervals
  sum(data$power_aerobic * c(diff(data$time), 0))
}

energy_anaerobic <- function(data) {
  # Simple implementation - sum of anaerobic power * time intervals
  sum(data$power_anaerobic * c(diff(data$time), 0))
}

# Test for alactic_energy_percentage
test_that("alactic_energy_percentage calculates correctly", {
  # Create test data
  test_data <- tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 10, 20, 10, 0),
    power_alactic = c(0, 5, 10, 5, 0)
  )

  # Test function
  result <- alactic_energy_percentage(test_data)

  # Should be 50% since power_alactic is half of power
  expect_equal(result, 50)

  # Test with all zeros
  zero_data <- tibble(
    time = c(0, 1, 2),
    power = c(0, 0, 0),
    power_alactic = c(0, 0, 0)
  )

  # Should return NA or 0 for all zeros (depends on implementation)
  expect_true(is.na(alactic_energy_percentage(zero_data)) ||
    alactic_energy_percentage(zero_data) == 0)
})

# Test for lactic_energy_percentage
test_that("lactic_energy_percentage calculates correctly", {
  # Create test data
  test_data <- tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 10, 20, 10, 0),
    power_lactic = c(0, 2, 4, 2, 0)
  )

  # Test function
  result <- lactic_energy_percentage(test_data)

  # Should be 20% since power_lactic is 1/5 of power
  expect_equal(result, 20)
})

# Test for anaerobic_energy_percentage
test_that("anaerobic_energy_percentage calculates correctly", {
  # Create test data
  test_data <- tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 10, 20, 10, 0),
    power_anaerobic = c(0, 7, 14, 7, 0)
  )

  # Test function
  result <- anaerobic_energy_percentage(test_data)

  # Should be 70% since power_anaerobic is 7/10 of power
  expect_equal(result, 70)
})

# Test for aerobic_energy_percentage
test_that("aerobic_energy_percentage calculates correctly", {
  # Create test data
  test_data <- tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 10, 20, 10, 0),
    power_aerobic = c(0, 3, 6, 3, 0)
  )

  # Test function
  result <- aerobic_energy_percentage(test_data)

  # Should be 30% since power_aerobic is 3/10 of power
  expect_equal(result, 30)
})

# Test for all energy pathways summing to 100%
test_that("energy percentages sum to 100%", {
  # Create test data with all energy pathways
  test_data <- tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 10, 20, 10, 0),
    power_alactic = c(0, 3, 6, 3, 0),
    power_lactic = c(0, 2, 4, 2, 0),
    power_aerobic = c(0, 5, 10, 5, 0),
    power_anaerobic = c(0, 5, 10, 5, 0) # alactic + lactic
  )

  # Calculate all percentages
  alactic_pct <- alactic_energy_percentage(test_data)
  lactic_pct <- lactic_energy_percentage(test_data)
  aerobic_pct <- aerobic_energy_percentage(test_data)
  anaerobic_pct <- anaerobic_energy_percentage(test_data)

  # Sum should be 100%
  expect_equal(aerobic_pct + anaerobic_pct, 100, tolerance = 1e-10)

  # Anaerobic should equal sum of alactic and lactic
  expect_equal(anaerobic_pct, alactic_pct + lactic_pct, tolerance = 1e-10)
})
