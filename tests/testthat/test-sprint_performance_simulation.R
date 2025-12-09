test_that("sprint_race_time_error_function returns distance close to 100m for Bolt parameters", {
  total_time <- 9.58
  reaction_time <- 0.146
  performance_running_time <- total_time - reaction_time

  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_race_time_error_function(performance_running_time, alactic_capacity, lactic_capacity)

  expect_type(result, "double")
  expect_true(result > 0)
  expect_true(abs(result - 100) < 5) # Should be close to 100m
})

test_that("sprint_race_time_error_function returns larger distance for longer time", {
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result_short <- sprint_race_time_error_function(10, alactic_capacity, lactic_capacity)
  result_long <- sprint_race_time_error_function(20, alactic_capacity, lactic_capacity)

  expect_true(result_long > result_short)
})

test_that("sprint_race_time_error_function respects model parameters", {
  time_performance <- 10
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result_default <- sprint_race_time_error_function(
    time_performance,
    alactic_capacity,
    lactic_capacity
  )

  result_higher_aerobic <- sprint_race_time_error_function(
    time_performance,
    alactic_capacity,
    lactic_capacity,
    maximal_aerobic_power = 30
  )

  expect_type(result_default, "double")
  expect_type(result_higher_aerobic, "double")
})

test_that("sprint_time_perf_simulation returns tibble with correct structure", {
  target_distances <- c(60, 100, 200)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_time_perf_simulation(target_distances, alactic_capacity, lactic_capacity)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(target_distances))
  expect_true("Distance" %in% names(result))
  expect_true("Optimal_Time" %in% names(result))
  expect_true("Actual_Time" %in% names(result))
})

test_that("sprint_time_perf_simulation returns increasing times for increasing distances", {
  target_distances <- c(60, 100, 200, 400)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_time_perf_simulation(target_distances, alactic_capacity, lactic_capacity)

  expect_true(all(diff(result$Optimal_Time) > 0))
  expect_true(all(diff(result$Actual_Time) > 0))
})

test_that("sprint_time_perf_simulation Actual_Time includes reaction time", {
  target_distances <- c(100)
  alactic_capacity <- 328
  lactic_capacity <- 1460
  reaction_time <- 0.15

  result <- sprint_time_perf_simulation(
    target_distances,
    alactic_capacity,
    lactic_capacity,
    reaction_time = reaction_time
  )

  expect_equal(result$Actual_Time - result$Optimal_Time, reaction_time)
})

test_that("sprint_time_perf_simulation works with different reaction times", {
  target_distances <- c(100)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result_fast <- sprint_time_perf_simulation(
    target_distances,
    alactic_capacity,
    lactic_capacity,
    reaction_time = 0.10
  )

  result_slow <- sprint_time_perf_simulation(
    target_distances,
    alactic_capacity,
    lactic_capacity,
    reaction_time = 0.20
  )

  expect_equal(result_fast$Optimal_Time, result_slow$Optimal_Time)
  expect_true(result_slow$Actual_Time > result_fast$Actual_Time)
})

test_that("sprint_time_perf_simulation_briand_table returns tinytable", {
  target_distances <- c(60, 100, 200)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_time_perf_simulation_briand_table(
    target_distances,
    alactic_capacity,
    lactic_capacity
  )

  expect_s4_class(result, "tinytable")
})

test_that("sprint_time_perf_simulation_briand_table has correct structure", {
  target_distances <- c(100)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_time_perf_simulation_briand_table(
    target_distances,
    alactic_capacity,
    lactic_capacity
  )

  expect_s4_class(result, "tinytable")
  expect_equal(nrow(result), 1)
})

test_that("sprint_time_perf_simulation handles edge case with single distance",
{
  target_distances <- c(100)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_time_perf_simulation(target_distances, alactic_capacity, lactic_capacity)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("sprint_time_perf_simulation produces reasonable times for known performances", {
  # Using Bolt-like parameters, 100m should be around 9-10 seconds

  target_distances <- c(100)
  alactic_capacity <- 328
  lactic_capacity <- 1460

  result <- sprint_time_perf_simulation(
    target_distances,
    alactic_capacity,
    lactic_capacity,
    reaction_time = 0.15
  )

  expect_true(result$Actual_Time > 9)
  expect_true(result$Actual_Time < 11)
})
