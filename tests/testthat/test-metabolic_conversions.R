# tests/testthat/test-unit-conversions.R

test_that("lactate conversion functions work correctly for males", {
  # Test lactate to energy
  acc_lactate <- 10
  energy <- convert_acc_lactate_to_lactic_energy(acc_lactate, sex = "male")
  expect_type(energy, "double")
  expect_gt(energy, 0)

  # Test energy to lactate (reverse conversion)
  acc_lactate_back <- convert_lactic_energy_to_acc_lactate(energy, sex = "male")
  expect_equal(acc_lactate, acc_lactate_back)
})

test_that("lactate conversion functions work correctly for females", {
  # Test lactate to energy
  acc_lactate <- 10
  energy <- convert_acc_lactate_to_lactic_energy(acc_lactate, sex = "female")
  expect_type(energy, "double")
  expect_gt(energy, 0)

  # Test energy to lactate (reverse conversion)
  acc_lactate_back <- convert_lactic_energy_to_acc_lactate(energy, sex = "female")
  expect_equal(acc_lactate, acc_lactate_back)
})

test_that("lactate conversion functions give different results for males and females", {
  acc_lactate <- 10
  male_energy <- convert_acc_lactate_to_lactic_energy(acc_lactate, sex = "male")
  female_energy <- convert_acc_lactate_to_lactic_energy(acc_lactate, sex = "female")
  expect_false(male_energy == female_energy)
})

test_that("aerobic power conversion functions work correctly", {
  # Test power to VO2
  aerobic_power <- 20
  vo2 <- convert_aerobic_power_to_vo2(aerobic_power)
  expect_type(vo2, "double")
  expect_gt(vo2, 0)

  # Test VO2 to power (reverse conversion)
  power_back <- convert_vo2_to_aerobic_power(vo2)
  expect_equal(aerobic_power, power_back, tolerance = 1e-10)
})

test_that("conversion functions handle zero values", {
  expect_equal(convert_acc_lactate_to_lactic_energy(0), 0)
  expect_equal(convert_lactic_energy_to_acc_lactate(0), 0)
  expect_equal(convert_aerobic_power_to_vo2(0), 0)
  expect_equal(convert_vo2_to_aerobic_power(0), 0)
})

test_that("conversion functions handle vector inputs", {
  values <- c(0, 5, 10)

  # Test all functions with vector inputs
  expect_length(convert_acc_lactate_to_lactic_energy(values), 3)
  expect_length(convert_lactic_energy_to_acc_lactate(values), 3)
  expect_length(convert_aerobic_power_to_vo2(values), 3)
  expect_length(convert_vo2_to_aerobic_power(values), 3)
})

