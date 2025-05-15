# tests/testthat/test-sprint-models.R

test_that("sprint_velocity_least_squares_model works with valid inputs", {
  # Mock data
  time <- seq(0, 10, by = 1)
  velocity <- c(0, 5, 8, 9.5, 10.2, 10.5, 10.6, 10.7, 10.7, 10.8, 10.8)
  reaction_time <- 0.15

  model <- sprint_velocity_least_squares_model(time, velocity, reaction_time)

  # Basic checks
  expect_s3_class(model, "nls")
  expect_true(all(coef(model) > 0))  # coefficients should be positive
  expect_named(coef(model), c("tau", "vf"))
})

test_that("diagnose_velocity_model_acc returns expected structure", {
  # Mock data
  mean_velocity_splits <- c(5, 8, 9.5, 10.2, 10.5)
  time_splits <- c(1, 2, 3, 4, 5)
  distance <- c(20, 40, 60, 80, 100)
  reaction_time <- 0.15

  result <- diagnose_velocity_model_acc(
    mean_velocity_splits,
    time_splits,
    distance,
    reaction_time
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("RMSE", "RSE", "plots"))
  expect_type(result$RMSE, "double")
  expect_type(result$RSE, "double")
  expect_length(result$plots, 4)

  # Check plots
  expect_s3_class(result$plots$observed_vs_fitted, "ggplot")
  expect_s3_class(result$plots$residuals_vs_fitted, "ggplot")
  expect_s3_class(result$plots$qq_plot, "ggplot")
  expect_s3_class(result$plots$residual_hist, "ggplot")
})

test_that("diagnose_sprint_model returns expected structure", {
  # Mock data
  mean_velocity_splits <- c(5, 8, 9.5, 10.2, 10.5)
  time_splits <- c(1, 2, 3, 4, 5)
  distance <- c(20, 40, 60, 80, 100)
  reaction_time <- 0.15
  maximal_velocity <- 11.0

  result <- diagnose_sprint_model(
    mean_velocity_splits,
    time_splits,
    distance,
    reaction_time,
    maximal_velocity
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("residuals_df", "RMSE", "RSE", "plots"))
  expect_s3_class(result$residuals_df, "tbl_df")
  expect_type(result$RMSE, "double")
  expect_type(result$RSE, "double")

  # Check plots
  expect_length(result$plots, 3)
  expect_s3_class(result$plots$observed_vs_fitted, "ggplot")
  expect_s3_class(result$plots$residuals_vs_fitted, "ggplot")
  expect_s3_class(result$plots$qq_plot, "ggplot")
})
