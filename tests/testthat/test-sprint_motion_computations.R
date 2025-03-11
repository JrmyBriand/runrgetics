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


test_that("acc_velocity_model calculates correct velocities", {
  # Test basic functionality
  expect_equal(
    acc_velocity_model(time = 5, tau = 1, maximal_velocity = 10),
    10 * (1 - exp(-5 / 1))
  )

  # Test with reaction time
  expect_equal(
    acc_velocity_model(time = 5, tau = 1, maximal_velocity = 10, reaction_time = 1),
    10 * (1 - exp(-(5 - 1) / 1))
  )

  # Test vectorized input
  times <- c(1, 2, 3)
  expected <- 10 * (1 - exp(-times / 1))
  expect_equal(
    acc_velocity_model(times, tau = 1, maximal_velocity = 10),
    expected
  )

  # Test that velocity approaches maximal_velocity as time increases
  large_time <- 100
  expect_lt(
    abs(acc_velocity_model(large_time, tau = 1, maximal_velocity = 10) - 10),
    0.01
  )
})

test_that("acc_distance_model calculates correct distances", {
  # Test basic functionality
  expect_equal(
    acc_distance_model(time = 5, tau = 1, maximal_velocity = 10),
    10 * (5 - 1 * (1 - exp(-5 / 1)))
  )

  # Test vectorized input
  times <- c(1, 2, 3)
  expected <- 10 * (times - 1 * (1 - exp(-times / 1)))
  expect_equal(
    acc_distance_model(times, tau = 1, maximal_velocity = 10),
    expected
  )

  # Test that distance approximates maximal_velocity * time for large times
  large_time <- 100
  expect_lt(
    abs(acc_distance_model(large_time, tau = 1, maximal_velocity = 10) - (10 * large_time - 10)),
    0.01
  )
})

test_that("dec_velocity_model calculates correct velocities", {
  # Test basic functionality
  expect_equal(
    dec_velocity_model(time = 8, maximal_velocity = 10, time_maximal_velocity = 6, decel_rate = 0.5),
    10 - 0.5 * (8 - 6)
  )

  # Test vectorized input
  times <- c(7, 8, 9)
  expected <- 10 - 0.5 * (times - 6)
  expect_equal(
    dec_velocity_model(times, maximal_velocity = 10, time_maximal_velocity = 6, decel_rate = 0.5),
    expected
  )

  # Test that velocity becomes zero at the expected time
  time_zero_velocity <- 6 + (10 / 0.5)
  expect_equal(
    dec_velocity_model(time_zero_velocity, maximal_velocity = 10, time_maximal_velocity = 6, decel_rate = 0.5),
    0
  )
})

test_that("dec_distance_model calculates correct distances", {
  # Test basic functionality
  time <- 8
  maximal_velocity <- 10
  time_maximal_velocity <- 6
  distance_maximal_velocity <- 60
  decel_rate <- 0.5

  v_decel <- dec_velocity_model(time, maximal_velocity, time_maximal_velocity, decel_rate)
  expected <- (maximal_velocity + v_decel) * (time - time_maximal_velocity) / 2 + distance_maximal_velocity

  expect_equal(
    dec_distance_model(time, maximal_velocity, time_maximal_velocity, distance_maximal_velocity, decel_rate),
    expected
  )

  # Test vectorized input
  times <- c(7, 8, 9)
  v_decels <- dec_velocity_model(times, maximal_velocity, time_maximal_velocity, decel_rate)
  expected <- (maximal_velocity + v_decels) * (times - time_maximal_velocity) / 2 + distance_maximal_velocity

  expect_equal(
    dec_distance_model(times, maximal_velocity, time_maximal_velocity, distance_maximal_velocity, decel_rate),
    expected
  )
})

test_that("velocity_sprint_model combines acceleration and deceleration models correctly", {
  # Test acceleration phase
  time <- 3
  time_maximal_velocity <- 5
  maximal_velocity <- 10
  tau <- 1.5
  fitted_maximal_velocity <- 10
  decel_rate <- 0.1

  expect_equal(
    velocity_sprint_model(time, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate),
    acc_velocity_model(time, tau, fitted_maximal_velocity)
  )

  # Test deceleration phase
  time <- 7

  expect_equal(
    velocity_sprint_model(time, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate),
    dec_velocity_model(time, maximal_velocity, time_maximal_velocity, decel_rate)
  )

  # Test at transition point
  time <- time_maximal_velocity

  acc_value <- acc_velocity_model(time, tau, fitted_maximal_velocity)
  decc_value <- dec_velocity_model(time, maximal_velocity, time_maximal_velocity, decel_rate)

  # Verify smooth transition (values should be approximately equal at transition point)
  expect_equal(
    velocity_sprint_model(time, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate),
    acc_value
  )

  # Check if acceleration velocity approaches maximal_velocity at transition point
  expect_lt(
    abs(acc_value - maximal_velocity),
    1
  )
})


test_that("velocity_sprint_model handles vector inputs correctly", {
  # Setup test parameters
  times <- seq(1, 10, by = 0.5)
  time_maximal_velocity <- 5
  maximal_velocity <- 10
  tau <- 1.5
  fitted_maximal_velocity <- 10
  decel_rate <- 0.1

  # Get results
  results <- velocity_sprint_model(times, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate)

  # Test length of results matches input
  expect_equal(length(results), length(times))

  # Test acceleration phase
  acc_times <- times[times <= time_maximal_velocity]
  acc_results <- results[times <= time_maximal_velocity]
  expected_acc <- acc_velocity_model(acc_times, tau, fitted_maximal_velocity, reaction_time = 0)
  expect_equal(acc_results, expected_acc)

  # Test deceleration phase
  decel_times <- times[times > time_maximal_velocity]
  decel_results <- results[times > time_maximal_velocity]
  expected_decel <- dec_velocity_model(decel_times, maximal_velocity, time_maximal_velocity, decel_rate)
  expect_equal(decel_results, expected_decel)

  # Test single value inputs (edge cases)
  # At transition point
  expect_equal(
    velocity_sprint_model(time_maximal_velocity, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate),
    acc_velocity_model(time_maximal_velocity, tau, fitted_maximal_velocity, reaction_time = 0)
  )

  # Well before transition
  expect_equal(
    velocity_sprint_model(1, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate),
    acc_velocity_model(1, tau, fitted_maximal_velocity, reaction_time = 0)
  )

  # Well after transition
  expect_equal(
    velocity_sprint_model(8, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate),
    dec_velocity_model(8, maximal_velocity, time_maximal_velocity, decel_rate)
  )
})

test_that("acceleration_sprint_model handles vector inputs correctly", {
  # Setup test parameters
  times <- seq(1, 10, by = 0.5)
  time_maximal_velocity <- 5
  fitted_maximal_velocity <- 10
  tau <- 1.5
  decel_rate <- 0.1

  # Get results
  results <- acceleration_sprint_model(times, time_maximal_velocity, fitted_maximal_velocity, tau, decel_rate)

  # Test length of results matches input
  expect_equal(length(results), length(times))

  # Test acceleration phase
  acc_times <- times[times <= time_maximal_velocity]
  acc_results <- results[times <= time_maximal_velocity]
  expected_acc <- (fitted_maximal_velocity - fitted_maximal_velocity * (1 - exp(-acc_times / tau))) / tau
  expect_equal(acc_results, expected_acc)

  # Test deceleration phase
  decel_times <- times[times > time_maximal_velocity]
  decel_results <- results[times > time_maximal_velocity]
  expect_equal(decel_results, rep(-decel_rate, length(decel_times)))

  # Test single value inputs (edge cases)
  # At transition point
  expect_equal(
    acceleration_sprint_model(time_maximal_velocity, time_maximal_velocity, fitted_maximal_velocity, tau, decel_rate),
    (fitted_maximal_velocity - fitted_maximal_velocity * (1 - exp(-time_maximal_velocity / tau))) / tau
  )

  # Well after transition
  expect_equal(
    acceleration_sprint_model(8, time_maximal_velocity, fitted_maximal_velocity, tau, decel_rate),
    -decel_rate
  )
})

test_that("distance_sprint_model handles vector inputs correctly", {
  # Setup test parameters
  times <- seq(1, 10, by = 0.5)
  time_maximal_velocity <- 5
  maximal_velocity <- 10
  distance_maximal_velocity <- 40
  fitted_maximal_velocity <- 10
  tau <- 1.5
  decel_rate <- 0.1

  # Get results
  results <- distance_sprint_model(times, time_maximal_velocity, maximal_velocity, distance_maximal_velocity, fitted_maximal_velocity, tau, decel_rate)

  # Test length of results matches input
  expect_equal(length(results), length(times))

  # Test acceleration phase
  acc_times <- times[times <= time_maximal_velocity]
  acc_results <- results[times <= time_maximal_velocity]
  expected_acc <- acc_distance_model(acc_times, tau, fitted_maximal_velocity)
  expect_equal(acc_results, expected_acc)

  # Test deceleration phase
  decel_times <- times[times > time_maximal_velocity]
  decel_results <- results[times > time_maximal_velocity]
  expected_decel <- dec_distance_model(decel_times, maximal_velocity, time_maximal_velocity, distance_maximal_velocity, decel_rate)
  expect_equal(decel_results, expected_decel)

  # Test single value inputs (edge cases)
  # At transition point
  expect_equal(
    distance_sprint_model(time_maximal_velocity, time_maximal_velocity, maximal_velocity, distance_maximal_velocity, fitted_maximal_velocity, tau, decel_rate),
    acc_distance_model(time_maximal_velocity, tau, fitted_maximal_velocity)
  )

  # Well before transition
  expect_equal(
    distance_sprint_model(2, time_maximal_velocity, maximal_velocity, distance_maximal_velocity, fitted_maximal_velocity, tau, decel_rate),
    acc_distance_model(2, tau, fitted_maximal_velocity)
  )

  # Well after transition
  expect_equal(
    distance_sprint_model(8, time_maximal_velocity, maximal_velocity, distance_maximal_velocity, fitted_maximal_velocity, tau, decel_rate),
    dec_distance_model(8, maximal_velocity, time_maximal_velocity, distance_maximal_velocity, decel_rate)
  )
})
