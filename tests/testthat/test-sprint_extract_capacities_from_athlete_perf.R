# tests/testthat/test-sprint-simulation.R

test_that("sprint_simulation_metrics returns expected structure", {
  # Test with basic inputs
  result <- sprint_simulation_metrics(
    time_performance = 10,
    distance = 100
  )

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("time_performance", "distance", "distance_estimated",
                         "distance_error", "total_energy", "alactic_energy",
                         "lactic_energy", "aerobic_energy"))
  expect_equal(nrow(result), 1)

  # Check types
  expect_type(result$time_performance, "double")
  expect_type(result$distance, "double")
  expect_type(result$total_energy, "double")
})

test_that("sprint_simulation_metrics handles different inputs", {
  # Test with different time and distance
  result1 <- sprint_simulation_metrics(10, 100)
  result2 <- sprint_simulation_metrics(20, 200)

  # Results should be different
  expect_false(identical(result1$total_energy, result2$total_energy))
  expect_false(identical(result1$distance_estimated, result2$distance_estimated))
})

test_that("sprint_simulation_metrics_briand_article returns expected output", {
  result <- sprint_simulation_metrics_briand_article()

  # Check that it returns something
  expect_true(!is.null(result))

  # Check that it's a tinytable object
  expect_true("tinytable" %in% class(result))
})

test_that("sprint_simulation_metrics_briand_article handles different capacities", {
  result1 <- sprint_simulation_metrics_briand_article(
    male_alactic_capacity = 328,
    male_lactic_capacity = 1460
  )

  result2 <- sprint_simulation_metrics_briand_article(
    male_alactic_capacity = 350,
    male_lactic_capacity = 1500
  )

  # Results should be different objects
  expect_false(identical(result1, result2))
})

test_that("sprint_simulation_metrics gives reasonable values", {
  result <- sprint_simulation_metrics(10, 100)

  # All energies should be positive
  expect_true(all(c(result$total_energy,
                    result$alactic_energy,
                    result$lactic_energy,
                    result$aerobic_energy) > 0))

  # Total energy should be sum of components
  expect_equal(
    result$total_energy,
    sum(c(result$alactic_energy, result$lactic_energy, result$aerobic_energy)),
    tolerance = 1
  )
})
