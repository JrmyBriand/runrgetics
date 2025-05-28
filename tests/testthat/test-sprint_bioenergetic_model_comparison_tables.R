# tests/testthat/test-sprint-bioenergetic-model-metrics.R

test_that("sprint_bioenergetic_model_gof_metrics returns expected structure", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 1, by = 0.1),
    power = rep(50, 11),
    power_mod = rep(55, 11)
  )

  result <- sprint_bioenergetic_model_gof_metrics(mock_data, event = "Test Event")

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_named(result, c("event", "adjusted_Rsquared", "energy_total",
                         "energy_percent_diff", "estimated_max_power",
                         "percentage_difference_max_power",
                         "distance_modeled_recovered", "distance_percent_diff"))
})

test_that("sprint_briand_article_gof_table works with test data", {


  # Just check that the function runs without error
  expect_error(sprint_briand_article_gof_table(), NA)
})

test_that("sprint_energy_contributions returns expected structure", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 1, by = 0.1),
    power = rep(50, 11),
    power_mod = rep(55, 11),
    power_alactic = rep(20, 11),
    power_lactic = rep(20, 11),
    power_aerobic = rep(15, 11)
  )

  result <- sprint_energy_contributions(mock_data, "Test Event")

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_named(result, c("event", "alactic_energy", "lactic_energy",
                         "aerobic_energy", "total_energy",
                         "alactic_percentage", "lactic_percentage",
                         "aerobic_percentage"))
})

test_that("sprint_energy_cont_briand_article_table works with test data", {


  # Just check that the function runs without error
  expect_error(sprint_energy_cont_briand_article_table(), NA)
})
