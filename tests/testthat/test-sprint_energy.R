# Tests for cumulative energy by pathway (Task 3 follow-up).

gpexe_ten200 <- function() subset(ten_200_sprints_paired, source == "gpexe")

test_that("sprint_energy_data returns cumulative energy per pathway", {
  g <- gpexe_ten200()
  ed <- sprint_energy_data(g, sprint_id = 1, maximal_aerobic_power = 27)
  expect_s3_class(ed, "tbl_df")
  expect_setequal(levels(ed$pathway), c("total", "alactic", "lactic", "aerobic"))
  expect_true(all(c("sprint_id", "time", "distance", "cumulative_energy") %in% names(ed)))
  # cumulative energy is non-decreasing and starts at 0
  for (p in levels(ed$pathway)) {
    e <- ed$cumulative_energy[ed$pathway == p]
    expect_equal(e[1], 0, tolerance = 1e-8)
    expect_true(all(diff(e) >= -1e-8))
  }
})

test_that("the total pathway equals the sum of alactic + lactic + aerobic", {
  ed <- sprint_energy_data(gpexe_ten200(), sprint_id = 1, maximal_aerobic_power = 27)
  wide <- tapply(ed$cumulative_energy, ed$pathway, function(x) x)
  total <- ed$cumulative_energy[ed$pathway == "total"]
  parts <- ed$cumulative_energy[ed$pathway == "alactic"] +
    ed$cumulative_energy[ed$pathway == "lactic"] +
    ed$cumulative_energy[ed$pathway == "aerobic"]
  expect_equal(total, parts, tolerance = 1e-6)
})

test_that("energy plots return ggplots", {
  g <- gpexe_ten200()
  sp <- detect_sprints(g)
  expect_s3_class(plot_sprint_energy(g, sprint_id = 1, sprints = sp), "ggplot")
  expect_s3_class(plot_workout_energy(g, sprints = sp), "ggplot")
  expect_error(sprint_energy_data(g, sprint_id = 999, sprints = sp), "not found")
})

test_that("plot_workout_energy honours sprint_ids and rejects unknown ids", {
  g <- gpexe_ten200()
  sp <- detect_sprints(g)
  expect_s3_class(plot_workout_energy(g, sprints = sp, sprint_ids = c(1, 2)), "ggplot")
  expect_error(plot_workout_energy(g, sprints = sp, sprint_ids = 999), "match")
})
