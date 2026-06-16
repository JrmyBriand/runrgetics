# Tests for training bioenergetic analysis (Task 3, group 4).

gpexe_ten200 <- function() subset(ten_200_sprints_paired, source == "gpexe")
one_sprint <- function() {
  g <- gpexe_ten200()
  sp <- detect_sprints(g)
  g[sp$start_index[1]:sp$end_index[1], ]
}

test_that("trim_sprint_deceleration cuts the abrupt brake, keeps the plateau", {
  t <- seq(0, 30, by = 0.2)
  v <- ifelse(t < 3, 7 * t / 3,
              ifelse(t < 25, 7, pmax(0, 7 - 1.5 * (t - 25))))  # ramp, plateau, brake (-1.5)
  trimmed <- trim_sprint_deceleration(data.frame(time = t, velocity = v),
                                      decel_threshold = 1.0)
  expect_lt(max(trimmed$time), 30)   # the brake was cut
  expect_gt(max(trimmed$time), 23)   # the plateau was kept
  expect_error(trim_sprint_deceleration(data.frame(time = 1:5)), "velocity")
})

test_that("analyze_sprint_bioenergetics returns sensible contributions", {
  res <- analyze_sprint_bioenergetics(one_sprint(), maximal_aerobic_power = 27)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_true(all(c("maximal_alactic_power", "maximal_lactic_power",
                    "energy_total", "pct_alactic", "pct_lactic", "pct_aerobic")
                  %in% names(res)))
  expect_equal(res$pct_alactic + res$pct_lactic + res$pct_aerobic, 100, tolerance = 1e-6)
  expect_gt(res$energy_total, 0)
  expect_gt(res$maximal_alactic_power, 0)
  expect_gt(res$maximal_lactic_power, 0)
})

test_that("higher MAP increases the aerobic contribution", {
  one <- one_sprint()
  lo <- analyze_sprint_bioenergetics(one, maximal_aerobic_power = 15)
  hi <- analyze_sprint_bioenergetics(one, maximal_aerobic_power = 30)
  expect_gt(hi$pct_aerobic, lo$pct_aerobic)
})

test_that("trim_sprint_launch drops the pre-launch lead-in", {
  t <- seq(0, 30, by = 0.2)
  v <- ifelse(t < 2, 0.1, ifelse(t < 5, (t - 2) * 2.5, 7.5))  # flat lead-in, ramp, plateau
  trimmed <- trim_sprint_launch(data.frame(time = t, velocity = v), launch_accel = 0.5)
  expect_gt(min(trimmed$time), 0)        # leading flat removed
  expect_lt(nrow(trimmed), length(t))
  expect_error(trim_sprint_launch(data.frame(time = 1:5)), "velocity")
})

test_that("plot_sprint_bioenergetics returns a ggplot", {
  g <- subset(ten_200_sprints_paired, source == "gpexe")
  expect_s3_class(plot_sprint_bioenergetics(g, sprint_id = 1, maximal_aerobic_power = 27),
                  "ggplot")
  expect_error(plot_sprint_bioenergetics(g, sprint_id = 999), "not found")
})

test_that("sprint_bioenergetic_model_fit estimates mu when fit_mu = TRUE", {
  md <- tibble::tibble(time = seq(0.1, 10, by = 0.1))
  md$power <- sprint_bioenergetic_model(md$time, 100, 50, maximal_aerobic_power = 25)
  expect_false("mu" %in% names(stats::coef(
    sprint_bioenergetic_model_fit(md, maximal_aerobic_power = 25))))
  expect_true("mu" %in% names(stats::coef(
    sprint_bioenergetic_model_fit(md, maximal_aerobic_power = 25, fit_mu = TRUE))))
})

test_that("sprint_bioenergetic_model_fit estimates sigma when fit_sigma = TRUE", {
  md <- tibble::tibble(time = seq(0.1, 10, by = 0.1))
  md$power <- sprint_bioenergetic_model(md$time, 100, 50, sigma = 0.6, maximal_aerobic_power = 25)
  expect_false("sigma" %in% names(stats::coef(
    sprint_bioenergetic_model_fit(md, maximal_aerobic_power = 25))))
  expect_true("sigma" %in% names(stats::coef(
    sprint_bioenergetic_model_fit(md, maximal_aerobic_power = 25, fit_sigma = TRUE))))
})

test_that("fit_mu / fit_sigma control the alactic peak and width", {
  g <- subset(ten_200_sprints_paired, source == "gpexe")
  sp <- detect_sprints(g)
  fixed  <- analyze_training_bioenergetics(g, sprints = sp, maximal_aerobic_power = 27,
                                           fit_mu = FALSE, fit_sigma = FALSE)
  fitted <- analyze_training_bioenergetics(g, sprints = sp, maximal_aerobic_power = 27,
                                           fit_mu = TRUE, fit_sigma = TRUE)
  expect_true(all(c("mu", "sigma") %in% names(fitted$per_sprint)))
  expect_true(all(fixed$per_sprint$mu == -0.4))       # held at the fixed values
  expect_true(all(fixed$per_sprint$sigma == 1))
  expect_false(all(fitted$per_sprint$mu == -0.4))     # fitted away
  expect_false(all(fitted$per_sprint$sigma == 1))
})

test_that("analyze_training_bioenergetics summarises the whole workout", {
  res <- analyze_training_bioenergetics(gpexe_ten200(), maximal_aerobic_power = 27)
  expect_named(res, c("per_sprint", "summary"))
  expect_equal(nrow(res$per_sprint), 10L)
  expect_equal(res$summary$n_sprints, 10L)
  expect_equal(res$summary$maximal_aerobic_power, 27)
  ps <- res$per_sprint
  expect_equal(ps$pct_alactic + ps$pct_lactic + ps$pct_aerobic,
               rep(100, nrow(ps)), tolerance = 1e-6)
})
