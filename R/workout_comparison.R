# Multi-sprint workout comparison (run on gpexe data): compare speed, distance,
# external and metabolic power across the detected efforts of a workout.

#' @importFrom utils globalVariables
utils::globalVariables(c("axis_value", "signal_value", "sprint_label"))

#' Per-sprint time series within a workout
#'
#' For each detected sprint, returns its velocity / power series re-zeroed to the
#' sprint start (time from 0, cumulative distance from 0).
#' @noRd
workout_sprint_series <- function(motion_data, sprints,
                                  cost_running_flat = 3.6, slope_equation = "extended",
                                  full_effort = TRUE) {
  validate_motion_data(motion_data)
  motion_data <- motion_data[order(motion_data$time), , drop = FALSE]
  t_all <- motion_data$time
  v_all <- motion_data$velocity
  dt <- stats::median(diff(t_all), na.rm = TRUE)
  a_all <- central_diff(v_all, dt)
  # smoothed speed for robustly walking each effort out to its bounding minima
  vs <- roll_smooth(v_all, max(1L, round(1 / dt)))
  n <- length(vs)

  series <- lapply(seq_len(nrow(sprints)), function(i) {
    s0 <- sprints$start_index[i]
    e0 <- sprints$end_index[i]
    if (full_effort) {
      while (s0 > 1 && vs[s0 - 1] < vs[s0]) s0 <- s0 - 1          # back down the launch ramp
      while (e0 < n && vs[e0 + 1] < vs[e0]) e0 <- e0 + 1          # forward down the deceleration
    }
    idx <- s0:e0
    tt <- t_all[idx] - t_all[idx[1]]
    vv <- v_all[idx]
    aa <- a_all[idx]
    tibble::tibble(
      sprint_id       = sprints$sprint_id[i],
      sprint_label    = paste("Sprint", sprints$sprint_id[i]),
      time            = tt,
      distance        = c(0, cumsum(diff(tt) * (utils::head(vv, -1) + utils::tail(vv, -1)) / 2)),
      velocity        = vv,
      acceleration    = aa,
      external_power  = external_power(aa, vv),
      metabolic_power = metabolic_power_vec(aa, vv, cost_running_flat, slope_equation)
    )
  })
  out <- dplyr::bind_rows(series)
  out$sprint_label <- factor(out$sprint_label,
                             levels = paste("Sprint", sort(unique(sprints$sprint_id))))
  out
}

#' Compare sprints within a workout
#'
#' Detects sprints (or uses supplied ones) in a workout and summarises and compares
#' them: per-sprint peak/mean speed, distance, duration and peak/mean external and
#' metabolic power. Intended to be run on gpexe data.
#'
#' @param motion_data A workout data frame with `time` (s) and `velocity` (m/s).
#' @param sprints Optional sprint table from [detect_sprints()]; if `NULL` (default),
#'   sprints are detected with [detect_sprints()].
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m) for metabolic power.
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param ... Passed to [detect_sprints()] when `sprints` is `NULL`.
#'
#' @returns A [tibble][tibble::tibble] with one row per sprint: `sprint_id`,
#'   `start_time`, `duration`, `distance`, `peak_speed`, `mean_speed`,
#'   `peak_external_power`, `mean_external_power`, `peak_metabolic_power`,
#'   `mean_metabolic_power`.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' compare_workout_sprints(gpexe)
compare_workout_sprints <- function(motion_data, sprints = NULL,
                                    cost_running_flat = 3.6, slope_equation = "extended",
                                    ...) {
  if (is.null(sprints)) sprints <- detect_sprints(motion_data, ...)
  if (nrow(sprints) == 0) stop("No sprints detected; adjust detection settings.")
  series <- workout_sprint_series(motion_data, sprints,
                                  cost_running_flat = cost_running_flat,
                                  slope_equation = slope_equation)

  parts <- lapply(split(series, series$sprint_id), function(s) {
    tibble::tibble(
      sprint_id            = s$sprint_id[1],
      start_time           = sprints$start_time[match(s$sprint_id[1], sprints$sprint_id)],
      duration             = max(s$time),
      distance             = max(s$distance),
      peak_speed           = max(s$velocity, na.rm = TRUE),
      mean_speed           = mean(s$velocity, na.rm = TRUE),
      peak_external_power  = max(s$external_power, na.rm = TRUE),
      mean_external_power  = mean(s$external_power, na.rm = TRUE),
      peak_metabolic_power = max(s$metabolic_power, na.rm = TRUE),
      mean_metabolic_power = mean(s$metabolic_power, na.rm = TRUE)
    )
  })
  out <- dplyr::bind_rows(parts)
  out[order(out$sprint_id), , drop = FALSE]
}

#' Plot a multi-sprint workout comparison
#'
#' Overlays each detected sprint's signal (speed, external or metabolic power)
#' against distance or time from the sprint start, one coloured line per sprint,
#' in the shared package style. The default (speed vs distance) reproduces the
#' classic sprint-comparison figure.
#'
#' @inheritParams compare_workout_sprints
#' @param signal Signal to plot: `"speed"` (default), `"external_power"` or
#'   `"metabolic_power"`.
#' @param x Horizontal axis: `"distance"` (m, default) or `"time"` (s), measured
#'   from each sprint's start.
#' @param sprint_ids Optional vector of `sprint_id`s to display; if `NULL` (default),
#'   all detected sprints are shown.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' plot_workout_sprints(gpexe, sprint_ids = c(1, 5, 9))
plot_workout_sprints <- function(motion_data, sprints = NULL,
                                 signal = c("speed", "external_power", "metabolic_power"),
                                 x = c("distance", "time"), sprint_ids = NULL,
                                 cost_running_flat = 3.6, slope_equation = "extended",
                                 ...) {
  signal <- match.arg(signal)
  x <- match.arg(x)
  if (is.null(sprints)) sprints <- detect_sprints(motion_data, ...)
  if (nrow(sprints) == 0) stop("No sprints detected; adjust detection settings.")
  if (!is.null(sprint_ids)) {
    sprints <- sprints[sprints$sprint_id %in% sprint_ids, , drop = FALSE]
    if (nrow(sprints) == 0) stop("None of `sprint_ids` match the detected sprints.")
  }
  series <- workout_sprint_series(motion_data, sprints,
                                  cost_running_flat = cost_running_flat,
                                  slope_equation = slope_equation)

  ycol <- c(speed = "velocity", external_power = "external_power",
            metabolic_power = "metabolic_power")[[signal]]
  ylab <- c(speed = "Speed (m/s)", external_power = "External power (W/kg)",
            metabolic_power = "Metabolic power (W/kg)")[[signal]]
  xlab <- c(distance = "Distance (m)", time = "Time (s)")[[x]]

  plot_data <- tibble::tibble(
    axis_value   = series[[x]],
    signal_value = series[[ycol]],
    sprint_label = series$sprint_label
  )

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = axis_value, y = signal_value, colour = sprint_label)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_colour_manual(values = runrgetics_pal(nlevels(plot_data$sprint_label))) +
    ggplot2::labs(title = "Workout sprint comparison",
                  x = xlab, y = ylab, colour = NULL) +
    theme_runrgetics()
}
