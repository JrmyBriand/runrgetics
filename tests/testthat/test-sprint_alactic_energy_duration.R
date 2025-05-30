# tests/testthat/test-sprint-alactic.R

test_that("sprint_alactic_energy_duration returns expected structure", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 1, by = 0.1),
    power_alactic = rep(50, 11),
    power = rep(100, 11)
  )

  result <- sprint_alactic_energy_duration(mock_data)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("duration", "alactic_energy", "alactic_power"))
  expect_equal(nrow(result), 1)
})

test_that("sprint_alactic_energy_duration_graubner_nixdorf returns expected structure", {
  result <- sprint_alactic_energy_duration_graubner_nixdorf()

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("duration", "alactic_energy", "alactic_power", "athlete_sex"))

  # Check athlete_sex parameter works
  result_female <- sprint_alactic_energy_duration_graubner_nixdorf(athlete_sex = "female")
  expect_true(all(result_female$athlete_sex == "female"))
})

test_that("sprint_alactic_duration_model returns expected type", {
  duration <- c(1, 2, 3)
  alactic_capacity <- 350

  result <- sprint_alactic_duration_model(duration, alactic_capacity)

  expect_type(result, "double")
  expect_length(result, length(duration))
})

test_that("sprint_alactic_duration_model_fit returns expected class", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    duration = c(1, 2, 3),
    alactic_power = c(300, 200, 100)
  )

  result <- sprint_alactic_duration_model_fit(mock_data)

  expect_s3_class(result, "nls")
})

test_that("sprint_alactic_duration_model_fit_rse returns expected type", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    duration = c(1, 2, 3),
    alactic_power = c(300, 200, 100)
  )

  result <- sprint_alactic_duration_model_fit_rse(mock_data)

  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("sprint_alactic_capacity returns expected type", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    duration = c(1, 2, 3),
    alactic_power = c(300, 200, 100)
  )

  result <- sprint_alactic_capacity(mock_data)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)  # Alactic capacity should be positive
})

# tests/testthat/test-find-max-al.R

test_that("find_max_al returns expected type", {
  duration <- 20
  alactic_capacity <- 350

  result <- find_max_al(duration, alactic_capacity)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_gte(result, 0)  # Should be non-negative
})

test_that("find_max_al works with different durations", {
  alactic_capacity <- 350

  result1 <- find_max_al(10, alactic_capacity)
  result2 <- find_max_al(20, alactic_capacity)

  # Different durations should give different results
  expect_false(identical(result1, result2))
})

test_that("find_max_al works with different capacities", {
  duration <- 20

  result1 <- find_max_al(duration, alactic_capacity = 250)
  result2 <- find_max_al(duration, alactic_capacity = 350)

  # Different capacities should give different results
  expect_false(identical(result1, result2))
  # Higher capacity should give higher power
  expect_gt(result2, result1)
})

test_that("find_max_al respects distribution parameters", {
  duration <- 20
  alactic_capacity <- 350

  result1 <- find_max_al(duration, alactic_capacity, mu_al = 1.75, sigma_al = 1.5)
  result2 <- find_max_al(duration, alactic_capacity, mu_al = 2.0, sigma_al = 2.0)

  # Different parameters should give different results
  expect_false(identical(result1, result2))
})

test_that("find_max_al handles zero capacity", {
  # Zero capacity should return very small or zero power
  result <- find_max_al(20, 0)
  expect_lt(result, 1)  # Should be very small or zero
})
