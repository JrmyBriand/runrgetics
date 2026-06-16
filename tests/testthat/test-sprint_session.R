# Tests for the high-level session analysis helpers and dashboard launcher (Task 6).

gpexe_ten200 <- function() subset(ten_200_sprints_paired, source == "gpexe")

write_gpexe <- function(motion, file) {
  utils::write.table(
    data.frame("time (s)" = motion$time,
               "speed (km/h)" = motion$velocity * 3.6,
               check.names = FALSE),
    file, sep = ";", dec = ".", row.names = FALSE, qmethod = "double")
}

test_that("read_gpexe_csv parses a gpexe export to time + velocity (m/s)", {
  f <- system.file("extdata", "sprint_mix_gpexe.csv.gz", package = "runrgetics")
  skip_if(!nzchar(f))
  md <- read_gpexe_csv(f)
  expect_s3_class(md, "tbl_df")
  expect_true(all(c("time", "velocity") %in% names(md)))
  expect_gt(nrow(md), 0)
  expect_true(all(md$velocity >= 0))
  expect_lt(max(md$velocity), 15)  # m/s, not km/h
})

test_that("read_gpexe_csv errors on a missing file and a non-gpexe CSV", {
  expect_error(read_gpexe_csv(tempfile()), "not found")
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = 4:6), bad, row.names = FALSE)
  expect_error(read_gpexe_csv(bad), "gpexe")
})

test_that("read_gpexe_csv round-trips a written gpexe file", {
  g <- gpexe_ten200()
  f <- tempfile(fileext = ".csv")
  write_gpexe(g, f)
  md <- read_gpexe_csv(f)
  expect_equal(nrow(md), nrow(g))
  expect_equal(md$velocity, g$velocity, tolerance = 1e-6)
})

test_that("analyze_sprint_session returns sprints, workout and bioenergetics", {
  res <- analyze_sprint_session(gpexe_ten200(), maximal_aerobic_power = 27)
  expect_equal(nrow(res$sprints), 10L)
  expect_equal(nrow(res$workout), 10L)
  expect_equal(nrow(res$bioenergetics$per_sprint), 10L)
  expect_equal(res$maximal_aerobic_power, 27)
  pct <- res$bioenergetics$per_sprint
  expect_true(all(pct$pct_alactic >= 0 & pct$pct_alactic <= 100))
})

test_that("analyze_sprint_session validates its inputs", {
  expect_error(analyze_sprint_session(data.frame(x = 1:5)), "velocity")
  expect_error(analyze_sprint_session(gpexe_ten200(), maximal_aerobic_power = -1),
               "maximal_aerobic_power")
})

test_that("batch_sprint_analysis writes per-session outputs and a run log", {
  g <- gpexe_ten200()
  sprints <- detect_sprints(g)
  lo <- max(1, sprints$start_index[1] - 50)
  hi <- min(nrow(g), sprints$end_index[2] + 50)
  f <- tempfile(fileext = ".csv")
  write_gpexe(g[lo:hi, ], f)

  out <- tempfile("sls_out")
  log <- batch_sprint_analysis(f, output_dir = out, maximal_aerobic_power = 20,
                               labels = "session1")

  expect_equal(nrow(log), 1L)
  expect_equal(log$status, "ok")
  expect_equal(log$n_sprints, 2L)
  expect_true(file.exists(file.path(out, "run_log.csv")))
  expect_true(file.exists(file.path(out, "session1", "sprints.csv")))
  expect_true(file.exists(file.path(out, "session1", "bioenergetics.csv")))
  expect_true(file.exists(file.path(out, "session1", "speed_distance.png")))
})

test_that("batch_sprint_analysis records errors without aborting", {
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3), bad, row.names = FALSE)
  out <- tempfile("sls_err")
  log <- batch_sprint_analysis(bad, output_dir = out)
  expect_equal(log$status, "error")
  expect_true(nzchar(log$message))
})

test_that("launch_sprint_dashboard errors on a missing data directory", {
  expect_error(launch_sprint_dashboard("/no/such/dir/xyz"))
})
