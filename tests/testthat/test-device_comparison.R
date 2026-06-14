# Tests for device comparison (Task 3, group 1).

test_that("compare_devices returns one row per channel with the expected columns", {
  res <- compare_devices(sprint_mix_paired)
  expect_s3_class(res, "tbl_df")
  expect_setequal(res$channel,
                  c("speed", "acceleration", "distance",
                    "external_power", "metabolic_power"))
  expect_true(all(c("rmse", "bias", "cor", "watch_mean", "ref_mean", "ratio", "n")
                  %in% names(res)))
})

test_that("filtered watch speed agrees closely with gpexe", {
  res <- compare_devices(sprint_mix_paired)
  speed <- res[res$channel == "speed", ]
  expect_gt(speed$cor, 0.9)            # strong correlation
  expect_lt(abs(speed$ratio - 1), 0.1) # within 10% in magnitude
})

test_that("compare_devices validates its input", {
  expect_error(compare_devices(mtcars), "source")
  expect_error(compare_devices(data.frame(source = "gpexe", time = 1:3, velocity = 1)),
               "must be present")
})

test_that("plot_device_comparison returns a ggplot and rejects unknown signals", {
  p <- plot_device_comparison(sprint_mix_paired, signal = "speed")
  expect_s3_class(p, "ggplot")
  expect_error(plot_device_comparison(sprint_mix_paired, signal = "heartrate"))
})

test_that("shared plot style helpers behave", {
  expect_length(runrgetics_pal(4), 4)
  expect_true(all(grepl("^#", runrgetics_pal(4))))
  expect_s3_class(theme_runrgetics(), "theme")
  expect_s3_class(scale_colour_runrgetics(), "Scale")
})
