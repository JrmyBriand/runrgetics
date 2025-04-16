# tests/testthat/test-sprint_model_functions.R

# Basic tests for parameter estimation functions
test_that("sprint_alactic_model_optimal_params returns expected structure", {
  result <- sprint_alactic_model_optimal_params()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("event", "pal_max", "mu", "sigma"))
  expect_type(result$pal_max, "double")
})

test_that("sprint_lactic_model_optimal_params returns expected structure", {
  result <- sprint_lactic_model_optimal_params()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("event", "p_la_max", "k1", "k2"))
  expect_type(result$p_la_max, "double")
})

# Basic tests for table output functions
test_that("sprint_alactic_model_optimal_params_table returns tinytable", {
  result <- sprint_alactic_model_optimal_params_table()
  expect_true(inherits(result, "tinytable"))
})

test_that("sprint_lactic_model_optimal_params_table returns tinytable", {
  result <- sprint_lactic_model_optimal_params_table()
  expect_true(inherits(result, "tinytable"))
})

# Basic tests for model fitting functions
test_that("fit_alactic_power_params works with valid data", {
  test_data <- data.frame(
    time = seq(0.1, 10, by = 0.1),
    power_alactic = 160 * exp(-(log(seq(0.1, 10, by = 0.1)) + 0.4)^2 / 2)
  )

  result <- fit_alactic_power_params(test_data)
  expect_type(result, "list")
  expect_named(result, c("pal_max", "sigma", "mu"))
  expect_type(result$pal_max, "double")
})

test_that("fit_lactic_power_params works with valid data", {
  test_data <- data.frame(
    time = seq(0.1, 10, by = 0.1),
    power_lactic = 60 * (1 - exp(-2.75 * seq(0.1, 10, by = 0.1))) *
      exp(-seq(0.1, 10, by = 0.1) / 35)
  )

  result <- fit_lactic_power_params(test_data)
  expect_type(result, "list")
  expect_named(result, c("p_la_max", "k1", "k2"))
  expect_type(result$p_la_max, "double")
})

# Basic tests for GoF metrics functions
test_that("sprint_alactic_model_gof_metrics returns expected structure", {
  result <- sprint_alactic_model_gof_metrics()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("event", "aic", "bic", "r_squared", "rse"))
})

test_that("sprint_lactic_model_gof_metrics returns expected structure", {
  result <- sprint_lactic_model_gof_metrics()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("event", "aic", "bic", "r_squared", "rse"))
})

# Basic tests for comparison functions
test_that("sprint_alactic_model_gof_comp returns expected structure", {
  result <- sprint_alactic_model_gof_comp()
  expect_s3_class(result, "tbl_df")
  expect_true("event" %in% names(result))
})

test_that("sprint_lactic_model_gof_comp returns expected structure", {
  result <- sprint_lactic_model_gof_comp()
  expect_s3_class(result, "tbl_df")
  expect_true("event" %in% names(result))
})

# Basic tests for comparison table functions
test_that("sprint_alactic_model_gof_comp_table returns tinytable", {
  result <- sprint_alactic_model_gof_comp_table()
  expect_true(inherits(result, "tinytable"))
})

test_that("sprint_lactic_model_gof_comp_table returns tinytable", {
  result <- sprint_lactic_model_gof_comp_table()
  expect_true(inherits(result, "tinytable"))
})
