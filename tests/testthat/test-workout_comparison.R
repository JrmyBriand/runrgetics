# Tests for multi-sprint workout comparison (Task 3, group 3).

gpexe_ten200 <- function() subset(ten_200_sprints_paired, source == "gpexe")

test_that("compare_workout_sprints returns one row per sprint with expected columns", {
  cmp <- compare_workout_sprints(gpexe_ten200())
  expect_s3_class(cmp, "tbl_df")
  expect_equal(nrow(cmp), 10L)
  expect_true(all(c("sprint_id", "start_time", "duration", "distance",
                    "peak_speed", "mean_speed", "peak_external_power",
                    "mean_external_power", "peak_metabolic_power",
                    "mean_metabolic_power") %in% names(cmp)))
})

test_that("per-sprint summaries are physically sensible", {
  cmp <- compare_workout_sprints(gpexe_ten200())
  expect_true(all(cmp$peak_speed >= cmp$mean_speed))
  expect_true(all(cmp$peak_metabolic_power >= cmp$mean_metabolic_power))
  expect_true(all(cmp$distance > 0))
  expect_true(all(abs(cmp$distance - 200) < 25))
})

test_that("compare_workout_sprints accepts pre-detected sprints", {
  g <- gpexe_ten200()
  sprints <- detect_sprints(g)
  cmp <- compare_workout_sprints(g, sprints = sprints)
  expect_equal(nrow(cmp), nrow(sprints))
})

test_that("plot_workout_sprints returns a ggplot for each signal/axis", {
  g <- gpexe_ten200()
  sprints <- detect_sprints(g)
  expect_s3_class(plot_workout_sprints(g, sprints = sprints), "ggplot")
  expect_s3_class(plot_workout_sprints(g, sprints = sprints,
                                       signal = "metabolic_power", x = "time"), "ggplot")
  expect_error(plot_workout_sprints(g, sprints = sprints, signal = "heartrate"))
})

test_that("plot_workout_sprints honours sprint_ids and rejects unknown ids", {
  g <- gpexe_ten200()
  sprints <- detect_sprints(g)
  expect_s3_class(plot_workout_sprints(g, sprints = sprints, sprint_ids = c(1, 5, 9)), "ggplot")
  expect_error(plot_workout_sprints(g, sprints = sprints, sprint_ids = 999), "match")
})

test_that("workout functions validate motion_data", {
  sprints <- detect_sprints(gpexe_ten200())
  expect_error(compare_workout_sprints(data.frame(x = 1:5), sprints = sprints), "velocity")
})
