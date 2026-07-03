# Tests for the exported dynamic-bioenergetics interface.

test_that("simulate_dynamic_bioenergetics returns the comparable series shape", {
  t <- seq(0, 20, by = 0.5)
  demand <- 25 * (1 - exp(-t / 3))
  sim <- simulate_dynamic_bioenergetics(demand, t, vo2max = 78.75, vlamax = 0.5)
  expect_s3_class(sim, "tbl_df")
  expect_named(sim, c("time", "measured", "alactic", "lactic", "aerobic", "total"))
  expect_equal(nrow(sim), length(t))
  expect_equal(sim$total, sim$alactic + sim$lactic + sim$aerobic, tolerance = 1e-9)
  expect_equal(sim$measured, demand)
  # integrated energies are attached and consistent with the series
  expect_equal(attr(sim, "energy_total"),
               attr(sim, "energy_alactic") + attr(sim, "energy_lactic") +
                 attr(sim, "energy_aerobic"), tolerance = 1e-9)
  expect_gt(attr(sim, "energy_total"), 0)
})

test_that("simulate_dynamic_bioenergetics validates its inputs", {
  t <- seq(0, 5, by = 0.5)
  demand <- rep(10, length(t))
  expect_error(simulate_dynamic_bioenergetics(demand, t[-1], vo2max = 78, vlamax = 0.5),
               "same length")
  expect_error(simulate_dynamic_bioenergetics(demand, rev(t), vo2max = 78, vlamax = 0.5),
               "increasing")
  expect_error(simulate_dynamic_bioenergetics(demand, t, vo2max = -1, vlamax = 0.5),
               "positive")
  expect_error(simulate_dynamic_bioenergetics(demand, t, vo2max = 78, vlamax = 0.5,
                                              m_active = 0), "m_active")
})

test_that("higher VLamax raises the glycolytic contribution", {
  t <- seq(0, 20, by = 0.5)
  demand <- 30 * (1 - exp(-t / 3))
  lo <- simulate_dynamic_bioenergetics(demand, t, vo2max = 78.75, vlamax = 0.3)
  hi <- simulate_dynamic_bioenergetics(demand, t, vo2max = 78.75, vlamax = 1.0)
  expect_gt(attr(hi, "energy_lactic"), attr(lo, "energy_lactic"))
})

test_that("tune_dynamic_vlamax minimises RMSE against the demand", {
  t <- seq(0, 20, by = 0.5)
  demand <- 28 * (1 - exp(-t / 3)) + 10 * exp(-t / 5)
  rng <- c(0.2, 1.2)
  tuned <- tune_dynamic_vlamax(demand, t, vo2max = 78.75, vlamax_range = rng)
  expect_true(tuned$vlamax > rng[1] && tuned$vlamax < rng[2])
  expect_gte(tuned$rmse, 0)
  # the tuned RMSE is no worse than at the search-range endpoints (it is a minimiser)
  rmse_at <- function(v) {
    sim <- simulate_dynamic_bioenergetics(demand, t, vo2max = 78.75, vlamax = v)
    sqrt(mean((sim$total - demand)^2))
  }
  expect_lte(tuned$rmse, rmse_at(rng[1]) + 1e-8)
  expect_lte(tuned$rmse, rmse_at(rng[2]) + 1e-8)
})

test_that("tune_dynamic_vlamax validates the search range", {
  t <- seq(0, 5, by = 0.5)
  demand <- rep(10, length(t))
  expect_error(tune_dynamic_vlamax(demand, t, vo2max = 78, vlamax_range = c(1, 0.5)),
               "increasing")
  expect_error(tune_dynamic_vlamax(demand, t, vo2max = 78, vlamax_range = c(-1, 1)),
               "positive")
})
