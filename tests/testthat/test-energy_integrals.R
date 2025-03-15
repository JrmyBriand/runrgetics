# test-energy-total.R

test_that("energy_total calculates the integral correctly", {
  # Test with the example from the documentation
  test_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 1, 2, 1, 0)
  )

  # Expected result using trapezoidal rule: (0+1)/2 + (1+2)/2 + (2+1)/2 + (1+0)/2 = 0.5 + 1.5 + 1.5 + 0.5 = 4
  expect_equal(energy_total(test_data), 4)
})

test_that("energy_total handles unsorted data", {
  # Test with unsorted data
  unsorted_data <- tibble::tibble(
    time = c(2, 0, 4, 1, 3),
    power = c(2, 0, 0, 1, 1)
  )

  # The function should sort this internally and produce the same result as sorted data
  sorted_data <- tibble::tibble(
    time = c(0, 1, 2, 3, 4),
    power = c(0, 1, 2, 1, 0)
  )

  expect_equal(energy_total(unsorted_data), energy_total(sorted_data))
})

test_that("energy_total works with constant power", {
  # Test with constant power
  constant_data <- tibble::tibble(
    time = c(0, 1, 2, 3),
    power = c(5, 5, 5, 5)
  )

  # Expected: 5 * (3-0) = 15
  expect_equal(energy_total(constant_data), 15)
})

test_that("energy_total handles single point data", {
  # Test with a single data point (should return 0)
  single_point <- tibble::tibble(
    time = 1,
    power = 10
  )

  # With only one point, the integral should be 0
  expect_equal(energy_total(single_point), 0)
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
