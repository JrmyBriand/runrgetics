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

test_that("compare_sprint_power_sources has 4 panels; default reference is gpexe", {
  d <- compare_sprint_power_sources(ten_200_sprints_paired, sprint_id = 1, body_mass = 67)
  expect_s3_class(d, "tbl_df")
  expect_setequal(levels(d$panel),
                  c("Speed (m/s)", "Acceleration (m/s^2)",
                    "Metabolic power (W/kg)", "External power (W/kg)"))
  expect_setequal(as.character(unique(d$source)), c("watch", "gpexe", "stryd"))
  # Stryd appears only in the external-power panel
  expect_true(all(as.character(d$panel[d$source == "stryd"]) == "External power (W/kg)"))
  # the GPS-derived comparison is still available
  d_gps <- compare_sprint_power_sources(ten_200_sprints_paired, sprint_id = 1,
                                        comparison = "gps")
  expect_setequal(as.character(unique(d_gps$source)), c("watch", "gps", "stryd"))
})

test_that("the Stryd channel scales inversely with body_mass", {
  s <- function(bm) {
    d <- compare_sprint_power_sources(ten_200_sprints_paired, sprint_id = 1, body_mass = bm)
    mean(d$value[d$source == "stryd"], na.rm = TRUE)
  }
  expect_equal(s(80), s(67) * 67 / 80, tolerance = 1e-6)
})

test_that("compare_sprint_power_sources / plot validate and return", {
  expect_error(compare_sprint_power_sources(mtcars), "source")
  expect_error(compare_sprint_power_sources(ten_200_sprints_paired, body_mass = -1), "positive")
  expect_s3_class(plot_sprint_power_sources(ten_200_sprints_paired, sprint_id = 1), "ggplot")
})

test_that("shared plot style helpers behave", {
  expect_length(runrgetics_pal(4), 4)
  expect_true(all(grepl("^#", runrgetics_pal(4))))
  expect_s3_class(theme_runrgetics(), "theme")
  expect_s3_class(scale_colour_runrgetics(), "Scale")
})
