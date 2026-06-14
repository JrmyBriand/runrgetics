# Tests for sprint detection (Task 3, group 2).

gpexe_ten200 <- function() subset(ten_200_sprints_paired, source == "gpexe")

test_that("detect_sprints finds the 10 sprints of the 10x200 m workout", {
  s <- detect_sprints(gpexe_ten200())
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 10L)
  expect_true(all(c("sprint_id", "start_time", "end_time", "start_index",
                    "end_index", "duration", "peak_speed", "mean_speed",
                    "distance", "peak_acceleration") %in% names(s)))
})

test_that("detected efforts respect the thresholds and look like 200 m reps", {
  s <- detect_sprints(gpexe_ten200())
  expect_true(all(s$peak_speed > 5))            # above the speed threshold
  expect_true(all(s$duration >= 2))             # above min_duration
  expect_true(all(s$end_time > s$start_time))
  expect_true(all(abs(s$distance - 200) < 25))  # ~200 m efforts
})

test_that("threshold and metric arguments work", {
  s_default <- detect_sprints(gpexe_ten200())
  s_high <- detect_sprints(gpexe_ten200(), threshold = 100)  # impossibly high speed
  expect_equal(nrow(s_high), 0L)
  # acceleration metric runs and detects efforts
  s_acc <- detect_sprints(gpexe_ten200(), metric = "acceleration")
  expect_gt(nrow(s_acc), 0L)
  # power metric uses the gpexe power_w_kg column
  s_pow <- detect_sprints(gpexe_ten200(), metric = "power")
  expect_gt(nrow(s_pow), 0L)
})

test_that("detect_sprints validates its inputs", {
  expect_error(detect_sprints(list(time = 1, velocity = 1)), "data frame")
  expect_error(detect_sprints(data.frame(time = 1:5)), "velocity")
  expect_error(detect_sprints(data.frame(time = 1:5, velocity = 1:5), threshold = -1),
               "positive")
  expect_error(
    detect_sprints(data.frame(time = 1:5, velocity = 1:5), metric = "power"),
    "power"
  )
})
