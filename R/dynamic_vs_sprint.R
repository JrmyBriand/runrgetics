# Compare the dynamic (Mader-style ODE) bioenergetic model with the sprint
# bioenergetic model on the SAME sprint: both are fitted / driven by the identical
# di Prampero metabolic-power demand, then their aerobic / glycolytic / alactic
# partitions are placed side by side.

#' @importFrom utils globalVariables
utils::globalVariables(c("pathway", "energy_j_kg", "pct", "model"))

#' Fit both bioenergetic models to one sprint (shared internal)
#'
#' Extracts the sprint, builds the di Prampero demand via the sprint bioenergetic
#' series, fits the sprint model, tunes/runs the dynamic model on the same demand,
#' and returns both aligned series plus the tuned VLamax and RMSE.
#' @noRd
dynamic_vs_sprint_fit <- function(motion_data, sprint_id = 1, sprints = NULL,
                                  maximal_aerobic_power = 27, vo2max = 78.75,
                                  vlamax = NULL, vlamax_range = c(0.1, 1.5),
                                  sc = 25, sa = 7, m_active = 0.25,
                                  pcr_0 = 23, initial_bla = 1.1,
                                  trim = TRUE, decel_threshold = 1.0,
                                  launch_accel = 0.5, cost_running_flat = 3.6,
                                  slope_equation = "extended",
                                  mu = 0.5, sigma = 0.5, k1 = 2.75, k2 = 35,
                                  fit_mu = TRUE, fit_sigma = TRUE, fit_k2 = TRUE) {
  validate_motion_data(motion_data)
  check_positive(maximal_aerobic_power, "maximal_aerobic_power")
  check_positive(vo2max, "vo2max")

  if (is.null(sprints)) sprints <- detect_sprints(motion_data)
  series <- workout_sprint_series(motion_data, sprints,
                                  cost_running_flat = cost_running_flat,
                                  slope_equation = slope_equation)
  s <- series[series$sprint_id == sprint_id, , drop = FALSE]
  if (nrow(s) == 0) stop("`sprint_id` not found in the detected sprints.", call. = FALSE)

  sprint_series <- sprint_bioenergetic_series(
    s, maximal_aerobic_power = maximal_aerobic_power, trim = trim,
    decel_threshold = decel_threshold, launch_accel = launch_accel,
    cost_running_flat = cost_running_flat, slope_equation = slope_equation,
    mu = mu, sigma = sigma, k1 = k1, k2 = k2,
    fit_mu = fit_mu, fit_sigma = fit_sigma, fit_k2 = fit_k2)
  if (all(is.na(sprint_series$total))) {
    stop("The sprint bioenergetic model fit failed for this sprint.", call. = FALSE)
  }

  demand <- sprint_series$measured
  time <- sprint_series$time

  rmse_val <- NA_real_
  if (is.null(vlamax)) {
    tuned <- tune_dynamic_vlamax(demand, time, vo2max = vo2max, sc = sc, sa = sa,
                                 m_active = m_active, pcr_0 = pcr_0,
                                 initial_bla = initial_bla,
                                 vlamax_range = vlamax_range)
    vlamax <- tuned$vlamax
    rmse_val <- tuned$rmse
  }

  dynamic_series <- simulate_dynamic_bioenergetics(
    demand, time, vo2max = vo2max, vlamax = vlamax, sc = sc, sa = sa,
    m_active = m_active, pcr_0 = pcr_0, initial_bla = initial_bla)
  if (is.na(rmse_val)) rmse_val <- rmse(dynamic_series$total, demand)

  list(sprint_series = sprint_series, dynamic_series = dynamic_series,
       vlamax = vlamax, rmse = rmse_val, sprint_id = sprint_id)
}

#' Compare the dynamic and sprint bioenergetic partitions of a sprint
#'
#' Runs both bioenergetic models on the same sprint, driven by the identical
#' di Prampero metabolic-power demand: the sprint bioenergetic model
#' ([analyze_sprint_bioenergetics()]) and the dynamic Mader-style ODE model
#' ([simulate_dynamic_bioenergetics()]). VO2max is fixed; the dynamic model's
#' VLamax is tuned to the demand ([tune_dynamic_vlamax()]) unless supplied.
#' Returns a tidy table of each pathway's energy (J/kg) and percentage share for
#' both models.
#'
#' @param motion_data A workout data frame with `time` (s) and `velocity` (m/s).
#' @param sprint_id Which detected sprint to compare (its `sprint_id`).
#' @param sprints Optional sprint table from [detect_sprints()]; if `NULL`
#'   (default), sprints are detected with [detect_sprints()].
#' @param maximal_aerobic_power Maximal aerobic power, MAP (W/kg), for the sprint
#'   bioenergetic model. Default 27.
#' @param vo2max Dynamic-model VO2max (mL/kg/min, systemic; use the measured value
#'   over-estimated by ~5%). Default 78.75.
#' @param vlamax Dynamic-model VLamax (mmol/kg/s, systemic). If `NULL` (default)
#'   it is tuned to the demand over `vlamax_range`.
#' @param vlamax_range Search interval for the VLamax tuning. Default `c(0.1, 1.5)`.
#' @param sc,sa,m_active,pcr_0,initial_bla Dynamic-model parameters, passed to
#'   [simulate_dynamic_bioenergetics()].
#' @param ... Sprint bioenergetic model arguments passed through to
#'   [analyze_sprint_bioenergetics()] (e.g. `trim`, `cost_running_flat`, `mu`).
#'
#' @returns A tidy [tibble][tibble::tibble] with columns `model`
#'   (`"dynamic"` / `"sprint"`), `pathway` (`"alactic"` / `"lactic"` /
#'   `"aerobic"`), `energy_j_kg` (J/kg) and `pct` (% of that model's total).
#'   The tuned `vlamax` and the dynamic-vs-demand `rmse` are attached as attributes.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' # supply VLamax to skip tuning (fast); omit `vlamax` to tune it to the demand
#' cmp <- compare_bioenergetic_models(gpexe, sprint_id = 1,
#'                                    maximal_aerobic_power = 27,
#'                                    vo2max = 78.75, vlamax = 0.5)
#' cmp
#' \donttest{
#' # tuning VLamax to the di Prampero demand (slower):
#' cmp_tuned <- compare_bioenergetic_models(gpexe, sprint_id = 1, vo2max = 78.75)
#' attr(cmp_tuned, "vlamax")
#' }
compare_bioenergetic_models <- function(motion_data, sprint_id = 1, sprints = NULL,
                                        maximal_aerobic_power = 27, vo2max = 78.75,
                                        vlamax = NULL, vlamax_range = c(0.1, 1.5),
                                        sc = 25, sa = 7, m_active = 0.25,
                                        pcr_0 = 23, initial_bla = 1.1, ...) {
  fit <- dynamic_vs_sprint_fit(
    motion_data, sprint_id = sprint_id, sprints = sprints,
    maximal_aerobic_power = maximal_aerobic_power, vo2max = vo2max,
    vlamax = vlamax, vlamax_range = vlamax_range, sc = sc, sa = sa,
    m_active = m_active, pcr_0 = pcr_0, initial_bla = initial_bla, ...)

  energies <- function(ser) {
    tt <- ser$time
    c(alactic = pracma::trapz(tt, ser$alactic),
      lactic = pracma::trapz(tt, ser$lactic),
      aerobic = pracma::trapz(tt, ser$aerobic))
  }
  make_rows <- function(ser, model) {
    e <- energies(ser)
    tibble::tibble(model = model, pathway = names(e),
                   energy_j_kg = unname(e), pct = 100 * unname(e) / sum(e))
  }

  out <- rbind(make_rows(fit$sprint_series, "sprint"),
               make_rows(fit$dynamic_series, "dynamic"))
  out$pathway <- factor(out$pathway, levels = c("alactic", "lactic", "aerobic"))
  out <- out[order(out$model, out$pathway), , drop = FALSE]
  attr(out, "vlamax") <- fit$vlamax
  attr(out, "rmse") <- fit$rmse
  tibble::as_tibble(out)
}

#' Plot the dynamic and sprint models against the observed demand
#'
#' Overlays the total metabolic power of both bioenergetic models on the observed
#' di Prampero power demand for one sprint, in the shared package style, so their
#' agreement and divergence over the effort is visible.
#'
#' @inheritParams compare_bioenergetic_models
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' plot_bioenergetic_comparison(gpexe, sprint_id = 1, vo2max = 78.75, vlamax = 0.5)
plot_bioenergetic_comparison <- function(motion_data, sprint_id = 1, sprints = NULL,
                                         maximal_aerobic_power = 27, vo2max = 78.75,
                                         vlamax = NULL, vlamax_range = c(0.1, 1.5),
                                         sc = 25, sa = 7, m_active = 0.25,
                                         pcr_0 = 23, initial_bla = 1.1, ...) {
  fit <- dynamic_vs_sprint_fit(
    motion_data, sprint_id = sprint_id, sprints = sprints,
    maximal_aerobic_power = maximal_aerobic_power, vo2max = vo2max,
    vlamax = vlamax, vlamax_range = vlamax_range, sc = sc, sa = sa,
    m_active = m_active, pcr_0 = pcr_0, initial_bla = initial_bla, ...)

  lev <- c("Observed metabolic power", "Sprint model (total)", "Dynamic model (total)")
  plot_data <- rbind(
    data.frame(time = fit$sprint_series$time, power = fit$sprint_series$measured,
               component = lev[1]),
    data.frame(time = fit$sprint_series$time, power = fit$sprint_series$total,
               component = lev[2]),
    data.frame(time = fit$dynamic_series$time, power = fit$dynamic_series$total,
               component = lev[3])
  )
  plot_data$component <- factor(plot_data$component, levels = lev)
  cols <- stats::setNames(c("grey60", "#0072B2", "#E69F00"), lev)

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = time, y = power, colour = component)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::labs(
      title = paste("Sprint", fit$sprint_id, "- dynamic vs sprint bioenergetic model"),
      subtitle = sprintf("Tuned VLamax = %.3f mmol/kg/s   RMSE = %.2f W/kg",
                         fit$vlamax, fit$rmse),
      x = "Time (s)", y = "Power (W/kg)", colour = NULL) +
    theme_runrgetics()
}
