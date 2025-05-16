# tests/testthat/test-plot-sprint-bioenergetic.R

test_that("plot_sprint_bioenergetic_model returns a ggplot object", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  plot <- plot_sprint_bioenergetic_model(mock_data)

  # Check that it returns a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("plot_sprint_bioenergetic_model has expected aesthetics", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  plot <- plot_sprint_bioenergetic_model(mock_data)

  # Check that x and y labels exist
  expect_true("Time (s)" %in% plot$labels$x)
  expect_true("Power (W/kg)" %in% plot$labels$y)
})

test_that("plot_sprint_bioenergetic_model respects scale_max_power", {
  # Create minimal mock data
  mock_data <- tibble::tibble(
    time = seq(0, 10, by = 1),
    velocity = seq(0, 10, by = 1),
    acceleration = rep(1, 11),
    distance = cumsum(velocity),
    power = velocity * 10
  )

  test_max_power <- 100
  plot <- plot_sprint_bioenergetic_model(mock_data, scale_max_power = test_max_power)

  # Extract y-axis limits from the plot
  y_limits <- plot$scales$get_scales("y")$limits

  # Check that max y limit matches scale_max_power
  expect_equal(y_limits[2], test_max_power)
})
