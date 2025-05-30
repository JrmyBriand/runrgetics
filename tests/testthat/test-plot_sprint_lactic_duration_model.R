# tests/testthat/test-plot-sprint-lactic.R

test_that("plot_sprint_lactic_duration returns a ggplot object", {
  # Create mock data
  mock_data <- tibble::tibble(
    duration = c(5, 10, 15, 30, 70),
    lactic_energy = c(200, 300, 900, 1000, 1400)
  )

  plot <- plot_sprint_lactic_duration(mock_data)

  expect_s3_class(plot, "ggplot")
})

test_that("get_lactic_model_data returns expected structure", {
  # Create mock data
  mock_data <- tibble::tibble(
    duration = c(5, 10, 15, 30, 70),
    lactic_energy = c(200, 300, 900, 1000, 1400)
  )

  result <- get_lactic_model_data(mock_data, sex_label = "male")

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("duration", "energy", "sex"))
  expect_equal(result$sex[1], "male")
  expect_equal(nrow(result), 600)  # As specified in the function
})

test_that("plot_sprint_lactic_duration_briand_article returns a ggplot object", {
  plot <- plot_sprint_lactic_duration_briand_article()

  expect_s3_class(plot, "ggplot")
})

test_that("plotting functions handle custom aesthetics", {
  mock_data <- tibble::tibble(
    duration = c(5, 10, 15, 30, 70),
    lactic_energy = c(200, 300, 900, 1000, 1400)
  )

  # Test with custom aesthetics
  plot <- plot_sprint_lactic_duration(
    mock_data,
    linetype = "dashed",
    line_color = "red",
    point_color = "blue",
    point_shape = 17
  )

  expect_s3_class(plot, "ggplot")
})

test_that("get_lactic_model_data handles different sex labels", {
  mock_data <- tibble::tibble(
    duration = c(5, 10, 15, 30, 70),
    lactic_energy = c(200, 300, 900, 1000, 1400)
  )

  result_male <- get_lactic_model_data(mock_data, sex_label = "male")
  result_female <- get_lactic_model_data(mock_data, sex_label = "female")

  expect_equal(result_male$sex[1], "male")
  expect_equal(result_female$sex[1], "female")
})

test_that("model functions respect time constants", {
  mock_data <- tibble::tibble(
    duration = c(5, 10, 15, 30, 70),
    lactic_energy = c(200, 300, 900, 1000, 1400)
  )

  result1 <- get_lactic_model_data(mock_data, "male", k1 = 20, k2 = 2000)
  result2 <- get_lactic_model_data(mock_data, "male", k1 = 30, k2 = 3000)

  # Results should be different with different time constants
  expect_false(identical(result1$energy, result2$energy))
})
