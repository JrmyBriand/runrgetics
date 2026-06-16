# Shared internal helpers for motion / energetics computations, used across the
# sprint-training analysis functions (watch filtering, device comparison, sprint
# detection, workout comparison, bioenergetics and energy). Not exported.

#' Validate a motion data frame (must have numeric `time` and `velocity`)
#' @noRd
validate_motion_data <- function(motion_data, arg = "motion_data") {
  if (!is.data.frame(motion_data)) {
    stop("`", arg, "` must be a data frame.", call. = FALSE)
  }
  if (!all(c("time", "velocity") %in% names(motion_data))) {
    stop("`", arg, "` must contain `time` (s) and `velocity` (m/s) columns.", call. = FALSE)
  }
  if (!is.numeric(motion_data$time) || !is.numeric(motion_data$velocity)) {
    stop("`time` and `velocity` must be numeric.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate that a value is a single positive number
#' @noRd
check_positive <- function(value, arg) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
    stop("`", arg, "` must be a single positive number.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Great-circle distance in metres between WGS84 points
#' @param lat1,lon1,lat2,lon2 numeric vectors of coordinates (degrees)
#' @returns numeric vector of distances (m)
#' @noRd
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

#' Root-mean-square error over jointly finite values
#' @noRd
rmse <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  sqrt(mean((a[ok] - b[ok])^2))
}

#' Central-difference derivative on a uniform grid
#' @param x numeric vector; dt numeric time step (s)
#' @returns numeric vector of the same length (m/s per s for a velocity input)
#' @noRd
central_diff <- function(x, dt) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  a <- numeric(n)
  a[1] <- (x[2] - x[1]) / dt
  a[n] <- (x[n] - x[n - 1]) / dt
  if (n > 2) a[2:(n - 1)] <- (x[3:n] - x[1:(n - 2)]) / (2 * dt)
  a
}

#' Cumulative trapezoidal integral on a uniform grid
#' @noRd
cumdist <- function(x, dt) {
  n <- length(x)
  if (n < 2) return(rep(0, n))
  c(0, cumsum((x[-1] + x[-n]) / 2 * dt))
}

#' Resample an irregular series onto a uniform grid by linear interpolation
#' @noRd
resample_uniform <- function(time, x, target_hz) {
  grid <- seq(min(time), max(time), by = 1 / target_hz)
  list(time = grid, x = stats::approx(time, x, xout = grid, rule = 2)$y)
}

#' Centered rolling-mean smoother (edges extended)
#' @noRd
roll_smooth <- function(x, k) {
  if (k %% 2 == 0) k <- k + 1L
  if (k <= 1) return(x)
  as.numeric(zoo::na.fill(zoo::rollmean(x, k = k, fill = NA, align = "center"), "extend"))
}

#' Element-wise metabolic power from acceleration and velocity
#'
#' `cost_running()` branches on the sign of acceleration, so it is applied
#' element-wise; `metabolic power = cost of running x velocity`.
#' @noRd
metabolic_power_vec <- function(acceleration, velocity, cost_running_flat, slope_equation) {
  cr <- vapply(seq_along(velocity),
               function(i) cost_running(acceleration[i], velocity[i],
                                        cost_running_flat, slope_equation),
               numeric(1))
  cr * velocity
}

#' Matched motion / power channels on a common grid
#'
#' Interpolates a (time, velocity) signal onto `grid` and derives acceleration
#' (central difference), external power ([external_power()]) and metabolic power
#' ([metabolic_power_vec()]). Shared by the device / sprint-power comparisons.
#'
#' @param time,velocity numeric vectors for the input signal.
#' @param grid numeric vector of grid times (uniform, step `dt`).
#' @param dt grid time step (s).
#' @param cost_running_flat,slope_equation passed to [cost_running()].
#' @returns a list with `velocity`, `acceleration`, `external_power`, `metabolic_power`.
#' @noRd
motion_channels <- function(time, velocity, grid, dt, cost_running_flat, slope_equation) {
  v <- stats::approx(time, velocity, xout = grid, rule = 2)$y
  a <- central_diff(v, dt)
  list(velocity = v, acceleration = a,
       external_power = external_power(a, v),
       metabolic_power = metabolic_power_vec(a, v, cost_running_flat, slope_equation))
}
