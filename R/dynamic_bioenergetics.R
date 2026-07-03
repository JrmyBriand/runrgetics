# Exported interface to the dynamic (Mader-style) bioenergetic model: simulate a
# sprint's metabolic-power demand into aerobic / glycolytic / alactic power and
# calibrate VLamax against the observed di Prampero demand.

#' Validate a (power_demand, time) input pair
#' @noRd
validate_power_series <- function(power_demand, time) {
  if (!is.numeric(power_demand) || !is.numeric(time)) {
    stop("`power_demand` and `time` must be numeric vectors.", call. = FALSE)
  }
  if (length(power_demand) != length(time)) {
    stop("`power_demand` and `time` must have the same length.", call. = FALSE)
  }
  if (length(time) < 2L) {
    stop("`power_demand` and `time` must have at least two elements.", call. = FALSE)
  }
  if (any(!is.finite(time)) || any(!is.finite(power_demand))) {
    stop("`power_demand` and `time` must be finite.", call. = FALSE)
  }
  if (any(diff(time) <= 0)) {
    stop("`time` must be strictly increasing.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Simulate a sprint's bioenergetics with the dynamic metabolic model
#'
#' Integrates a measured metabolic-power demand (e.g. the di Prampero sprint cost
#' of running x velocity) through the dynamic Mader-style 5-state ODE model ported
#' from MuscleEdot, and partitions the response into instantaneous aerobic
#' (oxidative phosphorylation), glycolytic (lactic) and alactic (phosphocreatine)
#' power. The output columns mirror the sprint bioenergetic model
#' ([analyze_sprint_bioenergetics()]) so the two decompositions are directly
#' comparable. Time-integrated energies (J/kg) and the VLamax used are attached as
#' attributes.
#'
#' @param power_demand Numeric vector of metabolic-power demand (W/kg), the series
#'   the model reproduces (the same di Prampero demand the sprint bioenergetic
#'   model fits).
#' @param time Numeric vector of time points (s), strictly increasing, the same
#'   length as `power_demand`.
#' @param vo2max Maximal oxygen uptake (mL/kg/min, systemic). For the dynamic
#'   model use the measured value over-estimated by ~5%.
#' @param vlamax Maximal glycolytic rate (mmol/kg/s, systemic).
#' @param sc Creatine pool size (mmol/kg). Default 25 (MuscleEdot default).
#' @param sa Adenylate pool size (mmol/kg). Default 7 (MuscleEdot default).
#' @param m_active Active muscle mass fraction. Default 0.25 (MuscleEdot default).
#' @param pcr_0 Initial phosphocreatine concentration (mmol/kg). Default 23.
#' @param initial_bla Initial blood lactate (mmol/L). Default 1.1.
#'
#' @returns A [tibble][tibble::tibble] with columns `time` (s), `measured`
#'   (input demand, W/kg), and `alactic`, `lactic`, `aerobic`, `total`
#'   (modelled power, W/kg). Attributes `energy_alactic`, `energy_lactic`,
#'   `energy_aerobic`, `energy_total` (J/kg) and `vlamax` are attached.
#' @export
#'
#' @examples
#' # Drive the dynamic model with a metabolic-power demand profile (W/kg)
#' t <- seq(0, 20, by = 0.2)
#' demand <- 25 * (1 - exp(-t / 3))
#' sim <- simulate_dynamic_bioenergetics(demand, t, vo2max = 78.75, vlamax = 0.5)
#' head(sim)
#' attr(sim, "energy_total")
simulate_dynamic_bioenergetics <- function(power_demand, time, vo2max, vlamax,
                                           sc = 25, sa = 7, m_active = 0.25,
                                           pcr_0 = 23, initial_bla = 1.1) {
  validate_power_series(power_demand, time)
  check_positive(vo2max, "vo2max")
  check_positive(vlamax, "vlamax")
  check_positive(sc, "sc")
  check_positive(sa, "sa")
  check_positive(pcr_0, "pcr_0")
  check_positive(initial_bla, "initial_bla")
  if (!is.numeric(m_active) || length(m_active) != 1L || is.na(m_active) ||
      m_active <= 0 || m_active > 1) {
    stop("`m_active` must be a single number in (0, 1].", call. = FALSE)
  }

  sol <- run_dynamic_metabolism(time, power_demand, vo2max, vlamax, sc, sa,
                                m_active, pcr_0 = pcr_0, lab_0 = initial_bla)
  part <- partition_dynamic_solution(sol, vo2max, vlamax, sc, sa, m_active)

  out <- tibble::tibble(
    time = time - min(time),
    measured = power_demand,
    alactic = part$power_alactic,
    lactic = part$power_la,
    aerobic = part$power_vo2,
    total = part$power_total
  )

  tt <- out$time
  attr(out, "energy_alactic") <- pracma::trapz(tt, out$alactic)
  attr(out, "energy_lactic") <- pracma::trapz(tt, out$lactic)
  attr(out, "energy_aerobic") <- pracma::trapz(tt, out$aerobic)
  attr(out, "energy_total") <- attr(out, "energy_alactic") +
    attr(out, "energy_lactic") + attr(out, "energy_aerobic")
  attr(out, "vlamax") <- vlamax
  out
}

#' Calibrate the dynamic model's VLamax to a metabolic-power demand
#'
#' Fixes VO2max (and the other parameters) and finds the VLamax that minimises the
#' RMSE between the dynamic model's total metabolic power and the observed
#' di Prampero power demand over the effort, by 1-D optimisation
#' ([stats::optimize()]).
#'
#' @inheritParams simulate_dynamic_bioenergetics
#' @param vlamax_range Numeric length-2 search interval for VLamax
#'   (mmol/kg/s, systemic). Default `c(0.1, 1.5)`.
#'
#' @returns A list with `vlamax` (the tuned value, mmol/kg/s) and `rmse` (the
#'   minimised RMSE between total modelled power and `power_demand`, W/kg).
#' @export
#'
#' @examples
#' t <- seq(0, 20, by = 0.5)
#' demand <- 25 * (1 - exp(-t / 3)) + 8 * exp(-t / 6)
#' tune_dynamic_vlamax(demand, t, vo2max = 78.75, vlamax_range = c(0.2, 1.0))
tune_dynamic_vlamax <- function(power_demand, time, vo2max, sc = 25, sa = 7,
                                m_active = 0.25, pcr_0 = 23, initial_bla = 1.1,
                                vlamax_range = c(0.1, 1.5)) {
  validate_power_series(power_demand, time)
  check_positive(vo2max, "vo2max")
  if (!is.numeric(vlamax_range) || length(vlamax_range) != 2L ||
      any(!is.finite(vlamax_range)) || vlamax_range[1] <= 0 ||
      vlamax_range[2] <= vlamax_range[1]) {
    stop("`vlamax_range` must be two increasing positive numbers.", call. = FALSE)
  }

  objective <- function(vlamax) {
    sol <- run_dynamic_metabolism(time, power_demand, vo2max, vlamax, sc, sa,
                                  m_active, pcr_0 = pcr_0, lab_0 = initial_bla)
    part <- partition_dynamic_solution(sol, vo2max, vlamax, sc, sa, m_active)
    rmse(part$power_total, power_demand)
  }

  opt <- stats::optimize(objective, interval = vlamax_range)
  list(vlamax = opt$minimum, rmse = opt$objective)
}
