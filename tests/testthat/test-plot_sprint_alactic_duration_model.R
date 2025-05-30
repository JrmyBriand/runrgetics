# tests/testthat/test-plot-sprint-alactic.R

test_that("plot_sprint_alactic_duration returns a ggplot object", {
  # Create mock data
  mock_data <- tibble::tibble(
    duration = c(2, 4, 6, 8),
    alactic_power = c(350, 320, 300, 290)
  )

  plot <- plot_sprint_alactic_duration(mock_data)

  expect_s3_class(plot, "ggplot")
})

test_that("get_alactic_model_data returns expected structure", {
  # Create mock data
  mock_data <- tibble::tibble(
    duration = c(2, 4, 6, 8),
    alactic_power = c(350, 320, 300, 290)
  )

  result <- get_alactic_model_data(mock_data, sex_label = "male")

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("duration", "power", "sex"))
  expect_equal(result$sex[1], "male")
  expect_equal(nrow(result), 600)  # As specified in the function
})

test_that("plot_sprint_alactic_duration_briand_article returns a ggplot object", {
  plot <- plot_sprint_alactic_duration_briand_article()

  expect_s3_class(plot, "ggplot")
})

test_that("plotting functions handle custom aesthetics", {
  mock_data <- tibble::tibble(
    duration = c(2, 4, 6, 8),
    alactic_power = c(350, 320, 300, 290)
  )

  # Test with custom aesthetics
  plot <- plot_sprint_alactic_duration(
    mock_data,
    linetype = "dashed",
    line_color = "red",
    point_color = "blue",
    point_shape = 17
  )

  expect_s3_class(plot, "ggplot")
})

test_that("get_alactic_model_data handles different sex labels", {
  mock_data <- tibble::tibble(
    duration = c(2, 4, 6, 8),
    alactic_power = c(350, 320, 300, 290)
  )

  result_male <- get_alactic_model_data(mock_data, sex_label = "male")
  result_female <- get_alactic_model_data(mock_data, sex_label = "female")

  expect_equal(result_male$sex[1], "male")
  expect_equal(result_female$sex[1], "female")
})


