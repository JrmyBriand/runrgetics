# Tests for the watch motion filter (Task 2).

# helper: watch and gpexe subsets of a paired dataset
watch_rows <- function(d) d[d$source == "polar_stryd", ]
gpexe_rows <- function(d) d[d$source == "gpexe", ]

test_that("gps_speed returns one speed per sample with a leading NA", {
  w <- watch_rows(sprint_mix_paired)
  v <- gps_speed(w$time, w$latitude, w$longitude)
  expect_length(v, nrow(w))
  expect_true(is.na(v[1]))
  expect_true(all(v[-1] >= 0, na.rm = TRUE))
  expect_error(gps_speed(1:3, 1:2, 1:3), "same length")
})

test_that("filter_watch_motion returns the expected tidy structure", {
  w <- watch_rows(sprint_mix_paired)
  f <- filter_watch_motion(w)
  expect_s3_class(f, "tbl_df")
  expect_true(all(c("time", "velocity", "acceleration", "distance",
                    "latitude", "longitude") %in% names(f)))
  expect_true(all(f$velocity >= 0))             # speed cannot be negative
  expect_true(all(diff(f$distance) >= -1e-8))   # cumulative distance is non-decreasing
  expect_false(anyNA(f$velocity))
})

test_that("filtering reduces jitter and acceleration error vs gpexe", {
  d <- sprint_mix_paired
  g <- gpexe_rows(d); w <- watch_rows(d)
  raw  <- motion_agreement(w$time, w$velocity, g$time, g$velocity)
  f    <- filter_watch_motion(w)
  filt <- motion_agreement(f$time, f$velocity, g$time, g$velocity)
  expect_lt(filt$jitter_watch, raw$jitter_watch)   # smoother
  expect_lt(filt$accel_rmse,   raw$accel_rmse)     # cleaner derivative
})

test_that("the filter is zero-phase (no time lag) on a synthetic peak", {
  t <- seq(0, 60, by = 1)
  v <- pmax(0, 5 - abs(t - 30) / 3)   # triangular pulse peaking at t = 30
  f <- filter_watch_motion(data.frame(time = t, velocity = v))
  peak_time <- f$time[which.max(f$velocity)]
  expect_equal(peak_time, 30, tolerance = 2)
})

test_that("all filter families run and preserve the output shape", {
  w <- watch_rows(sprint_mix_paired)
  for (m in c("butterworth", "savitzky_golay", "moving_average")) {
    f <- filter_watch_motion(w, method = m)
    expect_s3_class(f, "tbl_df")
    expect_true(all(f$velocity >= 0))
  }
})

test_that("gps speed_source works and needs position", {
  w <- watch_rows(sprint_mix_paired)
  f <- filter_watch_motion(w, speed_source = "gps")
  expect_true(all(f$velocity >= 0))
  expect_error(
    filter_watch_motion(data.frame(time = 1:10, velocity = 1:10), speed_source = "gps"),
    "latitude"
  )
})

test_that("filter_watch_motion validates its inputs", {
  expect_error(filter_watch_motion(list(time = 1:10)), "data frame")
  expect_error(filter_watch_motion(data.frame(speed = 1:10)), "time")
  expect_error(filter_watch_motion(data.frame(time = 1:10)), "velocity")
  expect_error(filter_watch_motion(data.frame(time = 1:3, velocity = 1:3)), "at least 5")
  expect_error(
    filter_watch_motion(data.frame(time = c(3, 1, 2, 4, 5), velocity = 1:5)),
    "sorted"
  )
})

test_that("motion_agreement is zero for identical signals", {
  g <- gpexe_rows(sprint_mix_paired)
  m <- motion_agreement(g$time, g$velocity, g$time, g$velocity)
  expect_s3_class(m, "tbl_df")
  expect_equal(m$speed_rmse, 0, tolerance = 1e-8)
  expect_equal(m$distance_pct, 0, tolerance = 1e-8)
})

test_that("tune_watch_filter returns scored grid, baseline and a better best", {
  tuned <- tune_watch_filter(sprint_mix_paired, methods = "butterworth",
                             speed_sources = "reported", cutoffs = c(0.15, 0.20),
                             orders = 2)
  expect_named(tuned, c("results", "baseline", "best"))
  expect_s3_class(tuned$results, "tbl_df")
  expect_equal(nrow(tuned$best), 1L)
  base_combined <- tuned$baseline$speed_rmse[1] + tuned$baseline$accel_rmse[1]
  expect_lt(tuned$best$combined_rmse, base_combined)   # filtering improves on raw
  expect_error(tune_watch_filter(mtcars), "source")
})
