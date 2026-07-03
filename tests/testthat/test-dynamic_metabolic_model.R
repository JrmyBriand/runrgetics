# Tests for the ported dynamic (Mader-style) metabolic model. The parity test
# pins the ported engine to reference values produced by the ORIGINAL MuscleEdot
# implementation (simulate_metabolism() + analyze_solution()) on a fixed input,
# so the port cannot drift from the source model. Reference generated once with
# VO2max = 78.75, VLamax = 0.5, SC = 25, SA = 7, M_active = 0.25, pcr_0 = 23,
# lab_0 = 1.1, over a constant 15 W/kg demand at 1 Hz for 21 s.

test_that("ported engine reproduces MuscleEdot reference output (parity)", {
  t <- seq(0, 20, by = 1)
  demand <- rep(15, length(t))
  sim <- simulate_dynamic_bioenergetics(demand, t, vo2max = 78.75, vlamax = 0.5,
                                        sc = 25, sa = 7, m_active = 0.25,
                                        pcr_0 = 23, initial_bla = 1.1)

  idx <- c(1, 2, 5, 10, 15, 21)
  ref_aerobic <- c(0, 0.011379983, 0.27408773, 2.0930907, 5.5709324, 9.9458588)
  ref_lactic  <- c(0.00017068764, 0.00055798397, 0.0061315644,
                   0.070526391, 0.30046953, 0.72506511)
  ref_alactic <- c(14.991082, 14.961063, 14.599928, 12.481523, 8.6410531, 0)

  expect_equal(sim$aerobic[idx], ref_aerobic, tolerance = 1e-4)
  expect_equal(sim$lactic[idx],  ref_lactic,  tolerance = 1e-4)
  expect_equal(sim$alactic[idx], ref_alactic, tolerance = 1e-4)

  # Integrated energies (J/kg) from the original engine
  expect_equal(attr(sim, "energy_aerobic"), 71.1142503, tolerance = 1e-4)
  expect_equal(attr(sim, "energy_lactic"),  4.09013566, tolerance = 1e-4)
  expect_equal(attr(sim, "energy_alactic"), 216.412455, tolerance = 1e-4)
})

test_that("regulation rate laws are monotone / saturating as expected", {
  # Oxidative phosphorylation increases with ADP and saturates at VO2max
  expect_lt(oxphos_rate(0.05, vo2max_muscle = 300),
            oxphos_rate(0.20, vo2max_muscle = 300))
  expect_lt(oxphos_rate(10, vo2max_muscle = 300), 300)
  # Glycolysis is inhibited by low pH (acidosis)
  hi_pH <- glycolysis_rate(0.2, 0.04, 7.2, vlamax_muscle = 2)
  lo_pH <- glycolysis_rate(0.2, 0.04, 6.6, vlamax_muscle = 2)
  expect_gt(hi_pH, lo_pH)
  # Lactate oxidation increases with lactate
  expect_lt(lactate_oxidation_rate(1, 2), lactate_oxidation_rate(1, 6))
})

test_that("Lohman phosphates round-trip via GP", {
  phos <- lohman_phosphates_from_gp(gp = 25, vo2 = 0.5, lam = 2,
                                    vo2max_muscle = 300, sc = 25, sa = 7)
  expect_true(all(is.finite(unlist(phos))))
  expect_gt(phos$pcr, 0)
  expect_lt(phos$pcr, 25)
  expect_gt(phos$atp, phos$adp)     # ATP >> ADP at rest-like state
  expect_gt(phos$adp, phos$amp)
})

test_that("alactic power is zero unless PCr is depleting", {
  expect_equal(alactic_power(0.5, 1), 0)     # PCr rising
  expect_equal(alactic_power(NA, 1), 0)      # no successor (last point)
  expect_gt(alactic_power(-0.5, 1), 0)       # PCr depleting -> positive power
})
