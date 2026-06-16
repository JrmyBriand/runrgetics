# Automatic detection of sprints / high-intensity efforts in a continuous workout.

#' @importFrom utils globalVariables
utils::globalVariables(c("sprint_id"))

#' Default detection threshold per metric
#' @noRd
default_threshold <- function(metric) {
  switch(metric,
    speed = 5,          # m/s (~18 km/h)
    power = 20,         # W/kg
    acceleration = 1.5, # m/s^2
    stop("Unknown metric.")
  )
}

#' Extract the detection metric series from a motion data frame
#' @noRd
detection_metric <- function(motion_data, metric, dt) {
  if (metric == "speed") {
    return(motion_data$velocity)
  }
  if (metric == "acceleration") {
    if ("acceleration" %in% names(motion_data)) return(motion_data$acceleration)
    return(central_diff(motion_data$velocity, dt))
  }
  if (metric == "power") {
    for (col in c("metabolic_power", "power", "power_w_kg")) {
      if (col %in% names(motion_data)) return(motion_data[[col]])
    }
    stop("metric = \"power\" requires a `metabolic_power`, `power` or `power_w_kg` column.")
  }
  stop("`metric` must be one of \"speed\", \"power\", \"acceleration\".")
}

#' Contiguous runs where a logical vector is TRUE
#' @noRd
runs_above_threshold <- function(above) {
  r <- rle(above)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  keep <- which(r$values)
  if (length(keep) == 0) return(data.frame(start = integer(0), end = integer(0)))
  data.frame(start = starts[keep], end = ends[keep])
}

#' Detect sprints / high-intensity efforts in a workout
#'
#' Automatically segments a continuous workout into individual sprint efforts:
#' contiguous spans where the chosen metric (speed, power or acceleration) exceeds
#' a threshold, with brief dips merged and short blips discarded. Per-effort
#' summary metrics are returned.
#'
#' @param motion_data A data frame with a numeric `time` (s) column and a
#'   `velocity` (m/s) column (plus `metabolic_power`/`power` for `metric = "power"`).
#' @param metric Detection metric: `"speed"` (default), `"power"` or `"acceleration"`.
#' @param threshold Detection threshold in the metric's units. If `NULL` (default),
#'   uses 5 m/s (speed), 20 W/kg (power) or 1.5 m/s^2 (acceleration).
#' @param min_duration Minimum effort duration to keep (s).
#' @param min_recovery Efforts separated by less than this (s) are merged.
#' @param smooth If `TRUE`, smooth the metric (~1 s rolling mean) before thresholding.
#'
#' @returns A [tibble][tibble::tibble] with one row per detected effort: `sprint_id`,
#'   `start_time`, `end_time`, `start_index`, `end_index`, `duration` (s),
#'   `peak_speed` (m/s), `mean_speed` (m/s), `distance` (m) and
#'   `peak_acceleration` (m/s^2).
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' detect_sprints(gpexe)
detect_sprints <- function(motion_data,
                           metric = c("speed", "power", "acceleration"),
                           threshold = NULL,
                           min_duration = 2,
                           min_recovery = 2,
                           smooth = TRUE) {
  metric <- match.arg(metric)
  if (!is.data.frame(motion_data)) stop("`motion_data` must be a data frame.")
  if (!all(c("time", "velocity") %in% names(motion_data))) {
    stop("`motion_data` must contain `time` (s) and `velocity` (m/s) columns.")
  }
  if (nrow(motion_data) < 3L) stop("`motion_data` must contain at least 3 rows.")
  if (is.null(threshold)) threshold <- default_threshold(metric)
  if (!is.numeric(threshold) || threshold <= 0) stop("`threshold` must be a positive number.")

  motion_data <- motion_data[order(motion_data$time), , drop = FALSE]
  t <- motion_data$time
  v <- motion_data$velocity
  dt <- stats::median(diff(t), na.rm = TRUE)
  fs <- 1 / dt
  acc <- central_diff(v, dt)

  x <- detection_metric(motion_data, metric, dt)
  if (smooth) x <- roll_smooth(x, max(1L, round(fs)))

  runs <- runs_above_threshold(!is.na(x) & x > threshold)
  if (nrow(runs) == 0) {
    return(tibble::tibble(
      sprint_id = integer(0), start_time = numeric(0), end_time = numeric(0),
      start_index = integer(0), end_index = integer(0), duration = numeric(0),
      peak_speed = numeric(0), mean_speed = numeric(0), distance = numeric(0),
      peak_acceleration = numeric(0)
    ))
  }

  # merge efforts separated by less than min_recovery
  merged <- runs[1, ]
  if (nrow(runs) > 1) {
    for (i in 2:nrow(runs)) {
      gap <- t[runs$start[i]] - t[merged$end[nrow(merged)]]
      if (gap < min_recovery) {
        merged$end[nrow(merged)] <- runs$end[i]
      } else {
        merged <- rbind(merged, runs[i, ])
      }
    }
  }

  # per-effort metrics, keeping only efforts of sufficient duration
  efforts <- lapply(seq_len(nrow(merged)), function(i) {
    s <- merged$start[i]; e <- merged$end[i]
    idx <- s:e
    duration <- t[e] - t[s]
    if (duration < min_duration) return(NULL)
    vv <- v[idx]; tt <- t[idx]
    distance <- sum(diff(tt) * (utils::head(vv, -1) + utils::tail(vv, -1)) / 2)
    tibble::tibble(
      start_time = t[s], end_time = t[e],
      start_index = s, end_index = e,
      duration = duration,
      peak_speed = max(vv, na.rm = TRUE),
      mean_speed = mean(vv, na.rm = TRUE),
      distance = distance,
      peak_acceleration = max(acc[idx], na.rm = TRUE)
    )
  })
  efforts <- dplyr::bind_rows(efforts)
  if (nrow(efforts) == 0) return(efforts)
  efforts$sprint_id <- seq_len(nrow(efforts))
  dplyr::relocate(efforts, sprint_id)
}
