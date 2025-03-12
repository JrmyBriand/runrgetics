

test_that("sprint_model_data runs without errors", {
  # Create simple test data
  mean_velocity_splits <- c(0, 5.77, 9.99)
  time_splits <- c(0, 1.88, 2.88)
  distance <- c(0, 10, 20)
  reaction_time <- 0.146
  maximal_velocity <- 12.34

  # Skip test if any dependency functions are not available
  skip_if_not(exists("find_time_velocity"))
  skip_if_not(exists("find_terminal_velocity"))
  skip_if_not(exists("find_tau"))

  # Just test that the function runs without errors
  expect_error(
    result <- sprint_model_data(
      mean_velocity_splits = mean_velocity_splits,
      time_splits = time_splits,
      distance = distance,
      reaction_time = reaction_time,
      maximal_velocity = maximal_velocity
    ),
    NA  # Expect no error
  )
})

test_that("sprint_model_data returns a tibble with correct columns", {
  # Skip test if function or dependencies aren't available
  skip_if_not(exists("sprint_model_data"))

  # Use example from function documentation
  result <- sprint_model_data(
    mean_velocity_splits = c(0, 5.77, 9.99),
    time_splits = c(0, 1.88, 2.88),
    distance = c(0, 10, 20),
    reaction_time = 0.146,
    maximal_velocity = 12.34
  )

  # Basic structure tests
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("time", "distance", "velocity", "acceleration",
                    "cost_running", "power") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("sprint_model_data works with NA maximal_velocity", {
  # Skip if function isn't available
  skip_if_not(exists("sprint_model_data"))

  # Basic test with NA maximal_velocity
  expect_error(
    result <- sprint_model_data(
      mean_velocity_splits = c(0, 5.77, 9.99),
      time_splits = c(0, 1.88, 2.88),
      distance = c(0, 10, 20),
      reaction_time = 0.146,
      maximal_velocity = NA
    ),
    NA  # Expect no error
  )
})
