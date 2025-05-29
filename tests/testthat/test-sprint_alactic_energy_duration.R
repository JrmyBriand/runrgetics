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
