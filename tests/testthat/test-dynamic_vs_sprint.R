# Tests for the dynamic-vs-sprint comparison layer (run on the real 200 m gpexe
# sprint). VLamax is supplied to skip tuning and keep the tests fast.

gpexe_ten200 <- function() subset(ten_200_sprints_paired, source == "gpexe")

test_that("compare_bioenergetic_models returns a tidy two-model partition", {
  cmp <- compare_bioenergetic_models(gpexe_ten200(), sprint_id = 1,
                                     maximal_aerobic_power = 27,
                                     vo2max = 78.75, vlamax = 0.5)
  expect_s3_class(cmp, "tbl_df")
  expect_named(cmp, c("model", "pathway", "energy_j_kg", "pct"))
  expect_setequal(unique(cmp$model), c("dynamic", "sprint"))
  expect_setequal(as.character(unique(cmp$pathway)), c("alactic", "lactic", "aerobic"))
  # each model's shares sum to 100 %
  shares <- as.numeric(tapply(cmp$pct, cmp$model, sum))
  expect_equal(shares, c(100, 100), tolerance = 1e-6)
  expect_true(all(cmp$energy_j_kg >= 0))
  expect_true(is.finite(attr(cmp, "vlamax")))
  expect_true(is.finite(attr(cmp, "rmse")))
})

test_that("compare_bioenergetic_models tunes VLamax when not supplied", {
  cmp <- compare_bioenergetic_models(gpexe_ten200(), sprint_id = 1,
                                     maximal_aerobic_power = 27, vo2max = 78.75,
                                     vlamax_range = c(0.1, 1.5))
  vlamax <- attr(cmp, "vlamax")
  expect_true(vlamax > 0.1 && vlamax < 1.5)
  expect_gt(attr(cmp, "rmse"), 0)
})

test_that("plot_bioenergetic_comparison returns a ggplot and errors on bad sprint", {
  g <- gpexe_ten200()
  expect_s3_class(
    plot_bioenergetic_comparison(g, sprint_id = 1, vo2max = 78.75, vlamax = 0.5),
    "ggplot")
  expect_error(
    plot_bioenergetic_comparison(g, sprint_id = 999, vo2max = 78.75, vlamax = 0.5),
    "not found")
})

test_that("compare_bioenergetic_models validates vo2max", {
  expect_error(
    compare_bioenergetic_models(gpexe_ten200(), vo2max = -1, vlamax = 0.5),
    "positive")
})
