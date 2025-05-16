# tests/testthat/test-sprint-bioenergetic.R

test_that("sprint_bioenergetic_model returns expected type", {
  time <- seq(0, 30, by = 0.01)
  result <- sprint_bioenergetic_model(time, 100, 50)
  expect_type(result, "double")
  expect_length(result, length(time))
})

test_that("sprint_bioenergetic_model handles different output options", {
  time <- c(1, 2, 3)
  max_alactic <- 100
  max_lactic <- 50

  total <- sprint_bioenergetic_model(time, max_alactic, max_lactic, output = "total power")
  alactic <- sprint_bioenergetic_model(time, max_alactic, max_lactic, output = "alactic power")
  lactic <- sprint_bioenergetic_model(time, max_alactic, max_lactic, output = "lactic power")
  aerobic <- sprint_bioenergetic_model(time, max_alactic, max_lactic, output = "aerobic power")

  expect_equal(length(total), 3)
  expect_equal(length(alactic), 3)
  expect_equal(length(lactic), 3)
  expect_equal(length(aerobic), 3)
})

test_that("sprint_bioenergetic_model outputs are non-negative", {
  time <- seq(1, 10, by = 1)
  result <- sprint_bioenergetic_model(time, 100, 50)
  expect_true(all(result >= 0))
})

# tests/testthat/test-sprint-bioenergetic-model-fit.R

test_that("sprint_bioenergetic_model_fit returns expected class", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10 # Simple mock power values
  )

  fit <- sprint_bioenergetic_model_fit(mock_data)

  expect_s3_class(fit, "nls")
  expect_named(coef(fit), c("maximal_alactic_power", "maximal_lactic_power"))
})

test_that("sprint_bioenergetic_model_max_al returns expected type", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  result <- sprint_bioenergetic_model_max_al(mock_data)

  expect_type(result, "double")
  expect_length(result, 1) # should be a single value
})

test_that("sprint_bioenergetic_model_max_la returns expected type", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  result <- sprint_bioenergetic_model_max_la(mock_data)

  expect_type(result, "double")
  expect_length(result, 1) # should be a single value
})

# tests/testthat/test-sprint-bioenergetic-model-data.R

test_that("sprint_bioenergetic_model_data returns expected structure", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  result <- sprint_bioenergetic_model_data(mock_data)

  # Check that it's a tibble
  expect_s3_class(result, "tbl_df")

  # Check that all expected columns exist
  expected_columns <- c(
    "time", "velocity", "acceleration", "distance", "power",
    "power_mod", "power_alactic", "power_lactic",
    "power_anaerobic", "power_aerobic"
  )

  expect_true(all(expected_columns %in% names(result)))

  # Check that number of rows matches input
  expect_equal(nrow(result), nrow(mock_data))
})

test_that("sprint_bioenergetic_model_data power columns are numeric", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  result <- sprint_bioenergetic_model_data(mock_data)

  # Check that power columns are numeric
  expect_type(result$power_mod, "double")
  expect_type(result$power_alactic, "double")
  expect_type(result$power_lactic, "double")
  expect_type(result$power_anaerobic, "double")
  expect_type(result$power_aerobic, "double")
})

test_that("sprint_bioenergetic_model_data anaerobic power is sum of alactic and lactic", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  result <- sprint_bioenergetic_model_data(mock_data)

  # Check that anaerobic power equals alactic + lactic
  expect_equal(
    result$power_anaerobic,
    result$power_alactic + result$power_lactic
  )
})
