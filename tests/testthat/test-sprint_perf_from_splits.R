# tests/testthat/test-sprint-performance.R

test_that("sprint_perf_from_split returns expected structure", {
  # Create mock data
  time_splits <- c(2, 4, 6, 8, 10)
  distance <- c(20, 40, 60, 80, 100)
  reaction_time <- 0.15

  result <- sprint_perf_from_split(time_splits, distance, reaction_time)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("distance", "performance"))
  expect_equal(nrow(result), 1)

  # Check values
  expect_equal(result$distance, 100)  # Should be last distance
  expect_equal(result$performance, 10 - 0.15)  # Should be last time minus reaction time
})

test_that("sprint_perf_from_split handles zero reaction time", {
  time_splits <- c(2, 4, 6)
  distance <- c(20, 40, 60)

  result <- sprint_perf_from_split(time_splits, distance)

  expect_equal(result$performance, 6)  # Should equal last time split
})

test_that("graubner_nixdorf_perf_from_splits returns expected structure", {
  result <- graubner_nixdorf_perf_from_splits()

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("distance", "performance", "sex", "event"))

  # Check sex values
  expect_true(all(result$sex %in% c("male", "female")))

  # Check event values
  expected_events <- c("Men's 100 m", "Men's 200 m", "Men's 400 m",
                       "Women's 100 m", "Women's 200 m", "Women's 400 m")
  expect_true(all(result$event %in% expected_events))
})

test_that("graubner_nixdorf_perf_from_splits returns valid performances", {
  result <- graubner_nixdorf_perf_from_splits()

  # All performances should be positive
  expect_true(all(result$performance > 0))

  # All distances should be either 100, 200, or 400
  expect_true(all(result$distance %in% c(100, 200, 400)))

  # Group by event and check times
  event_summary <- result |>
    dplyr::group_by(event) |>
    dplyr::summarise(mean_perf = mean(performance))

  # 400m events should have longer times than 200m events
  m400_times <- event_summary$mean_perf[grepl("400", event_summary$event)]
  m200_times <- event_summary$mean_perf[grepl("200", event_summary$event)]
  expect_true(all(m400_times > m200_times))
})
