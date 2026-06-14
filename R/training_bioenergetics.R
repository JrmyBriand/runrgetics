# Training bioenergetic analysis: apply the sprint bioenergetic model to detected
# sprints to split metabolic power into alactic / lactic / aerobic contributions.
# The model is only valid up to the "sprint finish" (the onset of the abrupt,
# end-of-effort deceleration), so each sprint is trimmed there before fitting; the
# gradual in-sprint (natural) deceleration is kept.

#' Trim a sprint's end-of-effort deceleration
#'
#' Finds the "sprint finish" - the onset of the abrupt, end-of-effort braking -
#' and drops everything after it. After the peak (smoothed) velocity, the finish
#' is the first sample whose (smoothed) acceleration falls below `-decel_threshold`.
#' The gradual, natural in-sprint deceleration (small negative acceleration) is
#' retained; only the sharp end braking is removed. If no abrupt deceleration is
#' found the sprint is returned unchanged.
#'
#' @param sprint_df A single sprint's data frame with `time` (s) and `velocity`
#'   (m/s) columns (an `acceleration` column is used if present).
#' @param decel_threshold Deceleration magnitude (m/s^2) marking the abrupt finish.
#'
#' @returns The input data frame truncated at the sprint finish.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' sprints <- detect_sprints(gpexe)
#' one <- gpexe[sprints$start_index[1]:sprints$end_index[1], ]
#' nrow(trim_sprint_deceleration(one))
trim_sprint_deceleration <- function(sprint_df, decel_threshold = 1.0) {
  if (!is.data.frame(sprint_df) || !all(c("time", "velocity") %in% names(sprint_df))) {
    stop("`sprint_df` must be a data frame with `time` and `velocity` columns.")
  }
  if (nrow(sprint_df) < 3L) return(sprint_df)
  dt <- stats::median(diff(sprint_df$time), na.rm = TRUE)
  k <- max(1L, round(1 / dt))
  v <- roll_smooth(sprint_df$velocity, k)
  a <- if ("acceleration" %in% names(sprint_df)) {
    roll_smooth(sprint_df$acceleration, k)
  } else {
    roll_smooth(central_diff(v, dt), k)
  }
  peak <- which.max(v)
  if (peak >= length(v)) return(sprint_df)
  after <- (peak + 1):length(v)
  brake <- after[which(a[after] < -decel_threshold)]
  if (length(brake) == 0) return(sprint_df)
  sprint_df[seq_len(brake[1]), , drop = FALSE]
}

#' Bioenergetic analysis of a single sprint
#'
#' Computes metabolic power over a sprint (if not already present), trims the
#' end-of-effort deceleration ([trim_sprint_deceleration()]), fits the sprint
#' bioenergetic model ([sprint_bioenergetic_model_fit()]) with the supplied
#' maximal aerobic power, and returns the alactic / lactic / aerobic peak powers,
#' energies and percentage contributions.
#'
#' @param sprint_df A single sprint's data frame with `time` (s) and `velocity`
#'   (m/s); a `metabolic_power` column is used if present, otherwise it is computed
#'   with [compute_metabolic_running_power()].
#' @param maximal_aerobic_power Maximal aerobic power, MAP (W/kg). Use 27 for the
#'   `sprint_mix` / `ten_200_sprints` sessions.
#' @param trim If `TRUE` (default), trim the end-of-effort deceleration before fitting.
#' @param decel_threshold Deceleration threshold (m/s^2) for the trim.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param mu,sigma,k1,k2 Sprint bioenergetic model shape parameters
#'   (see [sprint_bioenergetic_model()]).
#'
#' @returns A one-row [tibble][tibble::tibble] with `duration`, `maximal_alactic_power`,
#'   `maximal_lactic_power`, peak and mean total power, the alactic / lactic / aerobic
#'   energies (J/kg) and their percentage contributions, or a row of `NA`s if the
#'   model fit fails.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' sprints <- detect_sprints(gpexe)
#' one <- gpexe[sprints$start_index[1]:sprints$end_index[1], ]
#' analyze_sprint_bioenergetics(one, maximal_aerobic_power = 27)
analyze_sprint_bioenergetics <- function(sprint_df,
                                         maximal_aerobic_power = 27,
                                         trim = TRUE,
                                         decel_threshold = 1.0,
                                         cost_running_flat = 3.6,
                                         slope_equation = "extended",
                                         mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35) {
  if (!is.data.frame(sprint_df) || !all(c("time", "velocity") %in% names(sprint_df))) {
    stop("`sprint_df` must be a data frame with `time` and `velocity` columns.")
  }
  if (trim) sprint_df <- trim_sprint_deceleration(sprint_df, decel_threshold)

  # metabolic power (reuse the engine if not already computed)
  if (!"metabolic_power" %in% names(sprint_df)) {
    sprint_df <- compute_metabolic_running_power(
      sprint_df[, c("time", "velocity")],
      cost_running_flat = cost_running_flat, slope_equation = slope_equation)
  }

  md <- tibble::tibble(time = sprint_df$time - min(sprint_df$time),
                       power = sprint_df$metabolic_power)
  duration <- max(md$time)

  na_row <- tibble::tibble(
    duration = duration, maximal_alactic_power = NA_real_, maximal_lactic_power = NA_real_,
    peak_power = NA_real_, mean_power = NA_real_,
    energy_alactic = NA_real_, energy_lactic = NA_real_, energy_aerobic = NA_real_,
    energy_total = NA_real_, pct_alactic = NA_real_, pct_lactic = NA_real_,
    pct_aerobic = NA_real_
  )

  fit <- tryCatch(
    sprint_bioenergetic_model_fit(md, mu = mu, sigma = sigma, k1 = k1, k2 = k2,
                                  maximal_aerobic_power = maximal_aerobic_power),
    error = function(e) NULL)
  if (is.null(fit)) return(na_row)

  cf <- stats::coef(fit)
  max_al <- unname(cf[["maximal_alactic_power"]])
  max_la <- unname(cf[["maximal_lactic_power"]])
  tt <- md$time
  model_at <- function(out) {
    sprint_bioenergetic_model(tt, max_al, max_la, mu = mu, sigma = sigma,
                              k1 = k1, k2 = k2,
                              maximal_aerobic_power = maximal_aerobic_power, output = out)
  }
  p_al <- model_at("alactic power")
  p_la <- model_at("lactic power")
  p_aer <- model_at("aerobic power")
  p_tot <- p_al + p_la + p_aer

  e_al <- pracma::trapz(tt, p_al)
  e_la <- pracma::trapz(tt, p_la)
  e_aer <- pracma::trapz(tt, p_aer)
  e_tot <- e_al + e_la + e_aer

  tibble::tibble(
    duration = duration,
    maximal_alactic_power = max_al,
    maximal_lactic_power = max_la,
    peak_power = max(p_tot, na.rm = TRUE),
    mean_power = mean(p_tot, na.rm = TRUE),
    energy_alactic = e_al,
    energy_lactic = e_la,
    energy_aerobic = e_aer,
    energy_total = e_tot,
    pct_alactic = 100 * e_al / e_tot,
    pct_lactic = 100 * e_la / e_tot,
    pct_aerobic = 100 * e_aer / e_tot
  )
}

#' Bioenergetic analysis of a sprint-training workout
#'
#' Detects the sprints in a workout (or uses supplied ones), runs the bioenergetic
#' decomposition on each ([analyze_sprint_bioenergetics()]) with the given maximal
#' aerobic power, and returns the per-sprint results plus a workout-level summary.
#'
#' @inheritParams analyze_sprint_bioenergetics
#' @param motion_data A workout data frame with `time` (s) and `velocity` (m/s).
#' @param sprints Optional sprint table from [detect_sprints()]; if `NULL` (default),
#'   sprints are detected with [detect_sprints()].
#' @param ... Passed to [detect_sprints()] when `sprints` is `NULL`.
#'
#' @returns A list with `per_sprint` (a [tibble][tibble::tibble] of
#'   [analyze_sprint_bioenergetics()] rows, one per sprint, with a leading
#'   `sprint_id`) and `summary` (a one-row tibble of workout means: mean energies
#'   and mean percentage contributions across sprints).
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' res <- analyze_training_bioenergetics(gpexe, maximal_aerobic_power = 27)
#' res$summary
analyze_training_bioenergetics <- function(motion_data,
                                           sprints = NULL,
                                           maximal_aerobic_power = 27,
                                           trim = TRUE,
                                           decel_threshold = 1.0,
                                           cost_running_flat = 3.6,
                                           slope_equation = "extended",
                                           mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35,
                                           ...) {
  if (is.null(sprints)) sprints <- detect_sprints(motion_data, ...)
  if (nrow(sprints) == 0) stop("No sprints detected; adjust detection settings.")
  series <- workout_sprint_series(motion_data, sprints,
                                  cost_running_flat = cost_running_flat,
                                  slope_equation = slope_equation)

  parts <- lapply(split(series, series$sprint_id), function(s) {
    res <- analyze_sprint_bioenergetics(
      s, maximal_aerobic_power = maximal_aerobic_power, trim = trim,
      decel_threshold = decel_threshold, cost_running_flat = cost_running_flat,
      slope_equation = slope_equation, mu = mu, sigma = sigma, k1 = k1, k2 = k2)
    cbind(data.frame(sprint_id = s$sprint_id[1]), as.data.frame(res))
  })
  per_sprint <- tibble::as_tibble(do.call(rbind, parts))
  per_sprint <- per_sprint[order(per_sprint$sprint_id), , drop = FALSE]

  summary <- tibble::tibble(
    n_sprints = nrow(per_sprint),
    maximal_aerobic_power = maximal_aerobic_power,
    mean_energy_total = mean(per_sprint$energy_total, na.rm = TRUE),
    mean_pct_alactic = mean(per_sprint$pct_alactic, na.rm = TRUE),
    mean_pct_lactic = mean(per_sprint$pct_lactic, na.rm = TRUE),
    mean_pct_aerobic = mean(per_sprint$pct_aerobic, na.rm = TRUE)
  )

  list(per_sprint = per_sprint, summary = summary)
}
