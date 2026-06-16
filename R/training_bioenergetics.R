# Training bioenergetic analysis: apply the sprint bioenergetic model to detected
# sprints to split metabolic power into alactic / lactic / aerobic contributions.
# The model is only valid up to the "sprint finish" (the onset of the abrupt,
# end-of-effort deceleration), so each sprint is trimmed there before fitting; the
# gradual in-sprint (natural) deceleration is kept. The pre-launch lead-in is also
# trimmed so the model's t = 0 sits at the start of the acceleration burst (which
# keeps the alactic peak aligned with the real launch).

#' @importFrom utils globalVariables
utils::globalVariables(c("power", "component"))

#' Trim a sprint's pre-launch lead-in
#'
#' Drops the low-speed lead-in before the acceleration burst so that the sprint
#' starts at the launch. The launch is the first sample (up to the peak velocity)
#' whose smoothed acceleration exceeds `launch_accel`; one sample before it is kept
#' so the very start of the burst is included.
#'
#' @param sprint_df A single sprint's data frame with `time` (s) and `velocity`
#'   (m/s) columns (an `acceleration` column is used if present).
#' @param launch_accel Acceleration (m/s^2) marking the launch onset.
#'
#' @returns The input data frame truncated at its start to begin at the launch.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' sprints <- detect_sprints(gpexe)
#' one <- gpexe[sprints$start_index[1]:sprints$end_index[1], ]
#' nrow(trim_sprint_launch(one))
trim_sprint_launch <- function(sprint_df, launch_accel = 0.5) {
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
  if (peak <= 1) return(sprint_df)
  rise <- which(a[seq_len(peak)] > launch_accel)
  if (length(rise) == 0) return(sprint_df)
  onset <- max(1L, rise[1] - 1L)
  sprint_df[onset:nrow(sprint_df), , drop = FALSE]
}

#' Modeled bioenergetic series for one sprint
#'
#' Trims, computes metabolic power, fits the model and returns the measured and
#' modeled (total / alactic / lactic / aerobic) power time series. The fitted peak
#' alactic and lactic powers are attached as attributes. Returns model columns of
#' `NA` if the fit fails. Internal helper shared by the analysis and plot.
#' @noRd
sprint_bioenergetic_series <- function(sprint_df, maximal_aerobic_power = 27, trim = TRUE,
                                       decel_threshold = 1.0, launch_accel = 0.5,
                                       cost_running_flat = 3.6, slope_equation = "extended",
                                       mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35,
                                       fit_mu = TRUE, fit_sigma = TRUE) {
  if (trim) {
    sprint_df <- trim_sprint_launch(sprint_df, launch_accel)
    sprint_df <- trim_sprint_deceleration(sprint_df, decel_threshold)
  }
  # Metabolic power for the bioenergetic model: use the SPRINT cost of running
  # (cost_running_sprint clamps negative acceleration to 0, so the natural
  # in-sprint slowdown does not drop the power), consistent with the validated
  # sprint pipeline. Acceleration is smoothed (~1 s) to limit derivative noise.
  dt <- stats::median(diff(sprint_df$time), na.rm = TRUE)
  k <- max(1L, round(1 / dt))
  acc <- if ("acceleration" %in% names(sprint_df)) {
    sprint_df$acceleration
  } else {
    central_diff(sprint_df$velocity, dt)
  }
  acc <- roll_smooth(acc, k)
  mp <- cost_running_sprint(acc, sprint_df$velocity,
                            cost_running_flat = cost_running_flat,
                            slope_equation = slope_equation) * sprint_df$velocity
  md <- tibble::tibble(time = sprint_df$time - min(sprint_df$time), power = mp)
  fit <- tryCatch(
    sprint_bioenergetic_model_fit(md, mu = mu, sigma = sigma, k1 = k1, k2 = k2,
                                  maximal_aerobic_power = maximal_aerobic_power,
                                  fit_mu = fit_mu, fit_sigma = fit_sigma),
    error = function(e) NULL)

  out <- tibble::tibble(time = md$time, measured = md$power,
                        alactic = NA_real_, lactic = NA_real_,
                        aerobic = NA_real_, total = NA_real_)
  max_al <- NA_real_; max_la <- NA_real_; mu_used <- NA_real_; sigma_used <- NA_real_
  if (!is.null(fit)) {
    cf <- stats::coef(fit)
    max_al <- unname(cf[["maximal_alactic_power"]])
    max_la <- unname(cf[["maximal_lactic_power"]])
    mu_used <- if (fit_mu) unname(cf[["mu"]]) else mu
    sigma_used <- if (fit_sigma) unname(cf[["sigma"]]) else sigma
    model_at <- function(o) {
      sprint_bioenergetic_model(md$time, max_al, max_la, mu = mu_used, sigma = sigma_used,
                                k1 = k1, k2 = k2,
                                maximal_aerobic_power = maximal_aerobic_power, output = o)
    }
    out$alactic <- model_at("alactic power")
    out$lactic <- model_at("lactic power")
    out$aerobic <- model_at("aerobic power")
    out$total <- out$alactic + out$lactic + out$aerobic
  }
  attr(out, "maximal_alactic_power") <- max_al
  attr(out, "maximal_lactic_power") <- max_la
  attr(out, "mu") <- mu_used
  attr(out, "sigma") <- sigma_used
  out
}

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
#' Trims the pre-launch lead-in ([trim_sprint_launch()]) and the end-of-effort
#' deceleration ([trim_sprint_deceleration()]), computes the sprint metabolic power
#' (sprint cost of running, [cost_running_sprint()], on smoothed acceleration), fits
#' the sprint bioenergetic model ([sprint_bioenergetic_model_fit()]) with the
#' supplied maximal aerobic power, and returns the alactic / lactic / aerobic peak
#' powers, energies and percentage contributions.
#'
#' @param sprint_df A single sprint's data frame with `time` (s) and `velocity`
#'   (m/s); an `acceleration` column is used if present, otherwise acceleration is
#'   derived from velocity. Metabolic power is computed with [cost_running_sprint()].
#' @param maximal_aerobic_power Maximal aerobic power, MAP (W/kg). Use 27 for the
#'   `sprint_mix` / `ten_200_sprints` sessions.
#' @param trim If `TRUE` (default), trim the pre-launch lead-in and the
#'   end-of-effort deceleration before fitting.
#' @param decel_threshold Deceleration threshold (m/s^2) for the end-of-effort trim.
#' @param launch_accel Acceleration threshold (m/s^2) for the launch trim.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param mu,sigma,k1,k2 Sprint bioenergetic model shape parameters
#'   (see [sprint_bioenergetic_model()]). `mu` is the starting value when `fit_mu`
#'   is `TRUE`, otherwise the fixed value.
#' @param fit_mu If `TRUE` (default), also estimate the alactic peak location `mu`
#'   so it adapts to the observed metabolic-power peak (recommended for gpexe /
#'   training sprints, whose power peaks later than in maximal sprints); if `FALSE`,
#'   `mu` is held at the published fixed value.
#' @param fit_sigma If `TRUE` (default), also estimate the alactic width `sigma`
#'   (letting the bump narrow / decay faster); fitting `sigma` together with `mu`
#'   keeps the alactic separated from the lactic term and the contributions
#'   physiological. If `FALSE`, `sigma` is held fixed.
#'
#' @returns A one-row [tibble][tibble::tibble] with `duration`, `maximal_alactic_power`,
#'   `maximal_lactic_power`, `mu` and `sigma` (the alactic peak location and width
#'   used), peak and mean total power, the alactic / lactic / aerobic energies
#'   (J/kg) and their percentage contributions, or a row of `NA`s if the fit fails.
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
                                         launch_accel = 0.5,
                                         cost_running_flat = 3.6,
                                         slope_equation = "extended",
                                         mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35,
                                         fit_mu = TRUE, fit_sigma = TRUE) {
  if (!is.data.frame(sprint_df) || !all(c("time", "velocity") %in% names(sprint_df))) {
    stop("`sprint_df` must be a data frame with `time` and `velocity` columns.")
  }
  ser <- sprint_bioenergetic_series(
    sprint_df, maximal_aerobic_power = maximal_aerobic_power, trim = trim,
    decel_threshold = decel_threshold, launch_accel = launch_accel,
    cost_running_flat = cost_running_flat, slope_equation = slope_equation,
    mu = mu, sigma = sigma, k1 = k1, k2 = k2, fit_mu = fit_mu, fit_sigma = fit_sigma)
  duration <- max(ser$time)

  if (all(is.na(ser$total))) {
    return(tibble::tibble(
      duration = duration, maximal_alactic_power = NA_real_, maximal_lactic_power = NA_real_,
      mu = NA_real_, sigma = NA_real_, peak_power = NA_real_, mean_power = NA_real_,
      energy_alactic = NA_real_, energy_lactic = NA_real_, energy_aerobic = NA_real_,
      energy_total = NA_real_, pct_alactic = NA_real_, pct_lactic = NA_real_,
      pct_aerobic = NA_real_))
  }

  tt <- ser$time
  e_al <- pracma::trapz(tt, ser$alactic)
  e_la <- pracma::trapz(tt, ser$lactic)
  e_aer <- pracma::trapz(tt, ser$aerobic)
  e_tot <- e_al + e_la + e_aer

  tibble::tibble(
    duration = duration,
    maximal_alactic_power = attr(ser, "maximal_alactic_power"),
    maximal_lactic_power = attr(ser, "maximal_lactic_power"),
    mu = attr(ser, "mu"),
    sigma = attr(ser, "sigma"),
    peak_power = max(ser$total, na.rm = TRUE),
    mean_power = mean(ser$total, na.rm = TRUE),
    energy_alactic = e_al,
    energy_lactic = e_la,
    energy_aerobic = e_aer,
    energy_total = e_tot,
    pct_alactic = 100 * e_al / e_tot,
    pct_lactic = 100 * e_la / e_tot,
    pct_aerobic = 100 * e_aer / e_tot
  )
}

#' Plot a sprint's bioenergetic decomposition
#'
#' Decomposes one sprint of a workout into its alactic, lactic and aerobic
#' metabolic-power contributions and plots them together with the observed and
#' modeled total metabolic power, in the shared package style. The sprint is
#' extracted over its full effort and launch-aligned, so the alactic peak matches
#' the real acceleration burst.
#'
#' @param motion_data A workout data frame with `time` (s) and `velocity` (m/s).
#' @param sprint_id Which detected sprint to plot (its `sprint_id`).
#' @param sprints Optional sprint table from [detect_sprints()]; if `NULL` (default),
#'   sprints are detected with [detect_sprints()].
#' @inheritParams analyze_sprint_bioenergetics
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' plot_sprint_bioenergetics(gpexe, sprint_id = 1, maximal_aerobic_power = 27)
plot_sprint_bioenergetics <- function(motion_data, sprint_id = 1, sprints = NULL,
                                      maximal_aerobic_power = 27, trim = TRUE,
                                      decel_threshold = 1.0, launch_accel = 0.5,
                                      cost_running_flat = 3.6, slope_equation = "extended",
                                      mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35,
                                      fit_mu = TRUE, fit_sigma = TRUE) {
  if (is.null(sprints)) sprints <- detect_sprints(motion_data)
  series <- workout_sprint_series(motion_data, sprints,
                                  cost_running_flat = cost_running_flat,
                                  slope_equation = slope_equation)
  s <- series[series$sprint_id == sprint_id, , drop = FALSE]
  if (nrow(s) == 0) stop("`sprint_id` not found in the detected sprints.")

  ser <- sprint_bioenergetic_series(
    s, maximal_aerobic_power = maximal_aerobic_power, trim = trim,
    decel_threshold = decel_threshold, launch_accel = launch_accel,
    cost_running_flat = cost_running_flat, slope_equation = slope_equation,
    mu = mu, sigma = sigma, k1 = k1, k2 = k2, fit_mu = fit_mu, fit_sigma = fit_sigma)
  if (all(is.na(ser$total))) stop("The bioenergetic model fit failed for this sprint.")

  lev <- c("Observed metabolic power", "Modeled metabolic power",
           "Alactic power", "Lactic power", "Aerobic power")
  plot_data <- rbind(
    data.frame(time = ser$time, power = ser$measured, component = lev[1]),
    data.frame(time = ser$time, power = ser$total,    component = lev[2]),
    data.frame(time = ser$time, power = ser$alactic,  component = lev[3]),
    data.frame(time = ser$time, power = ser$lactic,   component = lev[4]),
    data.frame(time = ser$time, power = ser$aerobic,  component = lev[5])
  )
  plot_data$component <- factor(plot_data$component, levels = lev)
  cols <- stats::setNames(
    c("grey60", "#0072B2", "#D55E00", "#009E73", "#E69F00"), lev)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = power, colour = component)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::labs(title = paste("Sprint", sprint_id, "bioenergetic decomposition"),
                  x = "Time (s)", y = "Power (W/kg)", colour = NULL) +
    theme_runrgetics()
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
                                           launch_accel = 0.5,
                                           cost_running_flat = 3.6,
                                           slope_equation = "extended",
                                           mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35,
                                           fit_mu = TRUE, fit_sigma = TRUE,
                                           ...) {
  if (is.null(sprints)) sprints <- detect_sprints(motion_data, ...)
  if (nrow(sprints) == 0) stop("No sprints detected; adjust detection settings.")
  series <- workout_sprint_series(motion_data, sprints,
                                  cost_running_flat = cost_running_flat,
                                  slope_equation = slope_equation)

  parts <- lapply(split(series, series$sprint_id), function(s) {
    res <- analyze_sprint_bioenergetics(
      s, maximal_aerobic_power = maximal_aerobic_power, trim = trim,
      decel_threshold = decel_threshold, launch_accel = launch_accel,
      cost_running_flat = cost_running_flat,
      slope_equation = slope_equation, mu = mu, sigma = sigma, k1 = k1, k2 = k2,
      fit_mu = fit_mu, fit_sigma = fit_sigma)
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
