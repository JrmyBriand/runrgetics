test_that("sprint_motion_model_data runs without errors", {
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
    result <- sprint_motion_model_data(
      mean_velocity_splits = mean_velocity_splits,
      time_splits = time_splits,
      distance = distance,
      reaction_time = reaction_time,
      maximal_velocity = maximal_velocity
    ),
    NA # Expect no error
  )
})

test_that("sprint_motion_model_data returns a tibble with correct columns", {
  # Skip test if function or dependencies aren't available
  skip_if_not(exists("sprint_motion_model_data"))

  # Use example from function documentation
  result <- sprint_motion_model_data(
    mean_velocity_splits = c(0, 5.77, 9.99),
    time_splits = c(0, 1.88, 2.88),
    distance = c(0, 10, 20),
    reaction_time = 0.146,
    maximal_velocity = 12.34
  )

  # Basic structure tests
  expect_true(tibble::is_tibble(result))
  expect_true(all(c(
    "time", "distance", "velocity", "acceleration",
    "cost_running", "power"
  ) %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("sprint_motion_model_data works with NA maximal_velocity", {
  # Skip if function isn't available
  skip_if_not(exists("sprint_motion_model_data"))

  # Basic test with NA maximal_velocity
  expect_error(
    result <- sprint_motion_model_data(
      mean_velocity_splits = c(0, 5.77, 9.99),
      time_splits = c(0, 1.88, 2.88),
      distance = c(0, 10, 20),
      reaction_time = 0.146,
      maximal_velocity = NA
    ),
    NA # Expect no error
  )
})


test_that("sprint_maximum_metabolic_power works correctly for both power types", {
  # Test case 1: Basic data with obvious maximum
  basic_data <- tibble::tibble(
    time = c(1, 2, 3),
    velocity = c(1, 2, 3),
    acceleration = c(1, 1, 1),
    distance = c(1, 3, 6),
    cost_of_running = c(1, 2, 3),
    power = c(10, 20, 30),
    power_mod = c(15, 25, 35)
  )

  expect_equal(sprint_maximum_metabolic_power(basic_data, type = "power"), 30)
  expect_equal(sprint_maximum_metabolic_power(basic_data, type = "power bioenergetic model"), 35)

  # Test case 2: Data with maximum not at the end
  unordered_data <- tibble::tibble(
    time = c(1, 2, 3),
    velocity = c(1, 2, 3),
    acceleration = c(1, 1, 1),
    distance = c(1, 3, 6),
    cost_of_running = c(1, 2, 3),
    power = c(20, 40, 10),
    power_mod = c(25, 45, 15)
  )

  expect_equal(sprint_maximum_metabolic_power(unordered_data, type = "power"), 40)
  expect_equal(sprint_maximum_metabolic_power(unordered_data, type = "power bioenergetic model"), 45)

  # Test case 3: All same values
  constant_data <- tibble::tibble(
    time = c(1, 2, 3),
    velocity = c(1, 1, 1),
    acceleration = c(0, 0, 0),
    distance = c(1, 2, 3),
    cost_of_running = c(5, 5, 5),
    power = c(25, 25, 25),
    power_mod = c(30, 30, 30)
  )

  expect_equal(sprint_maximum_metabolic_power(constant_data, type = "power"), 25)
  expect_equal(sprint_maximum_metabolic_power(constant_data, type = "power bioenergetic model"), 30)
})

