# Filtering of Polar/Stryd watch motion data to emulate the gpexe-filtered signal.
#
# The gpexe device samples fast (~25 Hz) and applies onboard filtering, so its speed
# is treated as the reference. Watch GPS data (~1 Hz) is noisier. The functions here
# low-pass the watch signal and let its parameters be tuned against a time-aligned
# gpexe reference (see the paired datasets `sprint_mix_paired` / `ten_200_sprints_paired`).
#
# speed_source = "reported" (the watch's own speed column) is the default. Tuning on the
# paired data showed that "gps" (speed derived from raw GPS position) reproduces the
# gpexe speed/acceleration shape slightly better but inflates total distance (~+4%) via
# GPS path jitter. Future development: derive speed from *filtered* position to keep the
# GPS-shape fidelity while reducing that distance bias.

# ---- internal helpers --------------------------------------------------------
# Shared numeric helpers (haversine_m, rmse, central_diff, cumdist,
# resample_uniform) now live in R/utils-motion.R.

#' Apply a low-pass filter to a uniformly-sampled signal
#'
#' @param x numeric vector (uniform sampling)
#' @param method one of "butterworth", "savitzky_golay", "moving_average"
#' @param fs sampling frequency (Hz)
#' @param cutoff Butterworth cut-off frequency (Hz)
#' @param window window length (samples) for Savitzky-Golay / moving average
#' @param order filter order (Butterworth) or polynomial order (Savitzky-Golay)
#' @returns numeric vector, same length as `x`
#' @noRd
lowpass_filter <- function(x, method = c("butterworth", "savitzky_golay", "moving_average"),
                           fs, cutoff = 0.3, window = 5, order = 2) {
  method <- match.arg(method)
  if (anyNA(x)) x <- zoo::na.approx(x, na.rm = FALSE, rule = 2)

  if (method == "butterworth") {
    w <- cutoff / (fs / 2)
    if (w <= 0 || w >= 1) {
      stop("`cutoff` must be in (0, fs/2); got cutoff = ", cutoff, " Hz with fs = ", fs, " Hz.")
    }
    bf <- signal::butter(n = order, W = w, type = "low")
    return(as.numeric(signal::filtfilt(bf, x)))
  }

  if (window %% 2 == 0) window <- window + 1L
  if (method == "savitzky_golay") {
    if (window <= order) stop("Savitzky-Golay `window` must exceed `order`.")
    return(as.numeric(signal::sgolayfilt(x, p = order, n = window)))
  }
  # moving_average (centered, edges extended)
  ma <- zoo::rollmean(x, k = window, fill = NA, align = "center")
  as.numeric(zoo::na.fill(ma, "extend"))
}

#' Validate the inputs shared by the watch-filter functions
#' @noRd
validate_motion_df <- function(data, need_speed, need_position) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!"time" %in% names(data)) stop("`data` must contain a numeric `time` column (s).")
  if (!is.numeric(data$time)) stop("`time` must be numeric (seconds).")
  if (nrow(data) < 5L) stop("`data` must contain at least 5 rows to filter.")
  if (is.unsorted(data$time, na.rm = TRUE)) stop("`time` must be sorted in increasing order.")
  if (need_speed && !"velocity" %in% names(data)) {
    stop("`data` must contain a numeric `velocity` column (m/s) for speed_source = \"reported\".")
  }
  if (need_position && !all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must contain `latitude` and `longitude` columns for speed_source = \"gps\".")
  }
  invisible(TRUE)
}

# ---- exported functions ------------------------------------------------------

#' Speed derived from GPS position
#'
#' Computes instantaneous speed from successive GPS positions using the
#' great-circle (haversine) distance divided by the time step. This raw,
#' position-derived speed is noisy and is meant to be passed through
#' [filter_watch_motion()].
#'
#' @param time numeric vector of times (s), strictly increasing
#' @param latitude numeric vector of latitudes (decimal degrees)
#' @param longitude numeric vector of longitudes (decimal degrees)
#'
#' @returns a numeric vector of speeds (m/s); the first element is `NA`
#' @export
#'
#' @examples
#' w <- subset(sprint_mix_paired, source == "polar_stryd")
#' v <- gps_speed(w$time, w$latitude, w$longitude)
#' head(v)
gps_speed <- function(time, latitude, longitude) {
  n <- length(time)
  if (length(latitude) != n || length(longitude) != n) {
    stop("`time`, `latitude` and `longitude` must have the same length.")
  }
  if (n < 2L) return(rep(NA_real_, n))
  d <- haversine_m(latitude[-n], longitude[-n], latitude[-1], longitude[-1])
  c(NA_real_, d / diff(time))
}

#' Filter watch motion data to emulate the gpexe-filtered signal
#'
#' Low-pass filters noisy Polar/Stryd watch data so that the resulting speed,
#' acceleration and distance approximate the gpexe-filtered reference. The signal
#' is resampled onto a uniform grid (`target_hz`), filtered with a zero-phase filter
#' (so no time lag is introduced), and acceleration / cumulative distance are derived
#' from the filtered speed. If `latitude` / `longitude` are present they are smoothed
#' with the same filter. Default parameters were tuned against the paired datasets
#' (see [tune_watch_filter()]).
#'
#' @param data a data frame of watch data with a numeric `time` column (s) and either
#'   a `velocity` column (m/s, for `speed_source = "reported"`) or `latitude` /
#'   `longitude` columns (for `speed_source = "gps"`).
#' @param method low-pass filter family: `"butterworth"` (default, zero-phase),
#'   `"savitzky_golay"` or `"moving_average"`.
#' @param cutoff Butterworth cut-off frequency (Hz); used when `method = "butterworth"`.
#'   The default (0.175 Hz) was tuned against the paired datasets (see [tune_watch_filter()]).
#' @param order filter order (Butterworth) or polynomial order (Savitzky-Golay).
#' @param window window length in samples (Savitzky-Golay / moving average).
#' @param target_hz uniform resampling rate (Hz) used before filtering.
#' @param speed_source `"reported"` to filter the watch's speed column, or `"gps"`
#'   to filter speed derived from GPS position via [gps_speed()].
#'
#' @returns a [tibble][tibble::tibble] on the uniform grid with columns `time`,
#'   `velocity` (m/s), `acceleration` (m/s^2), `distance` (m) and, when position is
#'   supplied, smoothed `latitude` / `longitude`.
#' @export
#'
#' @examples
#' w <- subset(sprint_mix_paired, source == "polar_stryd")
#' filtered <- filter_watch_motion(w)
#' head(filtered)
filter_watch_motion <- function(data,
                                method = c("butterworth", "savitzky_golay", "moving_average"),
                                cutoff = 0.175,
                                order = 2,
                                window = 11,
                                target_hz = 5,
                                speed_source = c("reported", "gps")) {
  method <- match.arg(method)
  speed_source <- match.arg(speed_source)
  validate_motion_df(data, need_speed = speed_source == "reported",
                     need_position = speed_source == "gps")
  if (!is.numeric(target_hz) || target_hz <= 0) stop("`target_hz` must be a positive number.")

  v_raw <- if (speed_source == "reported") {
    data$velocity
  } else {
    gps_speed(data$time, data$latitude, data$longitude)
  }

  rs <- resample_uniform(data$time, v_raw, target_hz)
  v_filt <- lowpass_filter(rs$x, method = method, fs = target_hz,
                           cutoff = cutoff, window = window, order = order)
  v_filt <- pmax(v_filt, 0)               # speed cannot be negative
  dt <- 1 / target_hz

  out <- tibble::tibble(
    time = rs$time,
    velocity = v_filt,
    acceleration = central_diff(v_filt, dt),
    distance = cumdist(v_filt, dt)
  )

  if (all(c("latitude", "longitude") %in% names(data))) {
    lat <- resample_uniform(data$time, data$latitude, target_hz)$x
    lon <- resample_uniform(data$time, data$longitude, target_hz)$x
    out$latitude  <- lowpass_filter(lat, method = method, fs = target_hz,
                                    cutoff = cutoff, window = window, order = order)
    out$longitude <- lowpass_filter(lon, method = method, fs = target_hz,
                                    cutoff = cutoff, window = window, order = order)
  }
  out
}

#' Agreement between a watch signal and the gpexe reference
#'
#' Resamples a watch speed series and a reference (gpexe) speed series onto a common
#' uniform grid and reports agreement metrics on speed, acceleration (the derivative)
#' and cumulative distance. Use it to quantify the effect of filtering (before vs after).
#'
#' @param watch_time,watch_velocity numeric vectors for the watch signal (s, m/s).
#' @param ref_time,ref_velocity numeric vectors for the reference signal (s, m/s).
#' @param target_hz common grid rate (Hz) used for the comparison.
#'
#' @returns a one-row [tibble][tibble::tibble] with `speed_rmse`, `speed_bias`,
#'   `accel_rmse` (all on the common grid), `jitter_watch` / `jitter_ref`
#'   (sd of successive speed differences) and `distance_pct` (watch distance
#'   relative to the reference, in %).
#' @export
#'
#' @examples
#' w <- subset(sprint_mix_paired, source == "polar_stryd")
#' g <- subset(sprint_mix_paired, source == "gpexe")
#' f <- filter_watch_motion(w)
#' motion_agreement(f$time, f$velocity, g$time, g$velocity)
motion_agreement <- function(watch_time, watch_velocity, ref_time, ref_velocity,
                             target_hz = 5) {
  grid <- seq(max(min(watch_time), min(ref_time)),
              min(max(watch_time), max(ref_time)), by = 1 / target_hz)
  wv <- stats::approx(watch_time, watch_velocity, xout = grid, rule = 2)$y
  rv <- stats::approx(ref_time, ref_velocity, xout = grid, rule = 2)$y
  dt <- 1 / target_hz
  tibble::tibble(
    speed_rmse   = rmse(wv, rv),
    speed_bias   = mean(wv - rv, na.rm = TRUE),
    accel_rmse   = rmse(central_diff(wv, dt), central_diff(rv, dt)),
    jitter_watch = stats::sd(diff(wv), na.rm = TRUE),
    jitter_ref   = stats::sd(diff(rv), na.rm = TRUE),
    distance_pct = 100 * (sum(wv, na.rm = TRUE) / sum(rv, na.rm = TRUE) - 1)
  )
}

#' Tune the watch filter against the gpexe reference
#'
#' Sweeps filter families and parameters, applies each to the watch data with
#' [filter_watch_motion()], and scores the result against the time-aligned gpexe
#' reference with [motion_agreement()]. Returns the full scored grid plus the best
#' configuration (lowest combined speed + acceleration RMSE), and the unfiltered
#' baselines for comparison. Used to derive the package defaults and to re-tune on
#' new paired sessions.
#'
#' @param paired_data a paired-device data frame with a `source` column (e.g.
#'   [sprint_mix_paired]), containing both the reference and watch rows.
#' @param methods filter families to evaluate.
#' @param speed_sources watch speed inputs to evaluate (`"reported"`, `"gps"`).
#' @param cutoffs Butterworth cut-off frequencies to sweep (Hz).
#' @param windows window lengths to sweep (Savitzky-Golay / moving average).
#' @param orders filter / polynomial orders to sweep.
#' @param target_hz comparison grid rate (Hz).
#' @param reference,device values of the `source` column identifying the reference
#'   (gpexe) and the watch rows.
#'
#' @returns a list with `results` (a [tibble][tibble::tibble] of every configuration
#'   scored), `baseline` (unfiltered watch metrics per speed source) and `best`
#'   (the single best-scoring row).
#' @export
#'
#' @examples
#' \donttest{
#' tuned <- tune_watch_filter(sprint_mix_paired, methods = "butterworth",
#'                            speed_sources = "reported", cutoffs = c(0.2, 0.3))
#' tuned$best
#' }
tune_watch_filter <- function(paired_data,
                              methods = c("butterworth", "savitzky_golay", "moving_average"),
                              speed_sources = c("reported", "gps"),
                              cutoffs = seq(0.10, 0.45, by = 0.05),
                              windows = c(5, 7, 9, 11, 15),
                              orders = 2:3,
                              target_hz = 5,
                              reference = "gpexe",
                              device = "polar_stryd") {
  if (!is.data.frame(paired_data) || !"source" %in% names(paired_data)) {
    stop("`paired_data` must be a data frame with a `source` column.")
  }
  methods <- match.arg(methods, several.ok = TRUE)
  speed_sources <- match.arg(speed_sources, several.ok = TRUE)

  g <- paired_data[paired_data$source == reference, , drop = FALSE]
  w <- paired_data[paired_data$source == device, , drop = FALSE]
  g <- g[order(g$time), , drop = FALSE]
  w <- w[order(w$time), , drop = FALSE]
  if (nrow(g) < 5L || nrow(w) < 5L) {
    stop("Both `reference` and `device` rows must be present in `paired_data`.")
  }

  # unfiltered baselines (before)
  baseline <- list()
  for (src in speed_sources) {
    v0 <- if (src == "reported") w$velocity else gps_speed(w$time, w$latitude, w$longitude)
    baseline[[src]] <- cbind(
      data.frame(speed_source = src, stringsAsFactors = FALSE),
      as.data.frame(motion_agreement(w$time, v0, g$time, g$velocity, target_hz))
    )
  }
  baseline <- tibble::as_tibble(do.call(rbind, baseline))

  # build the configuration grid per method
  configs <- list()
  for (src in speed_sources) for (m in methods) {
    if (m == "butterworth") {
      for (ct in cutoffs) for (o in orders) {
        configs[[length(configs) + 1]] <- list(method = m, speed_source = src,
                                               cutoff = ct, window = NA_real_, order = o)
      }
    } else if (m == "savitzky_golay") {
      for (wi in windows) for (o in orders) {
        if (wi > o) configs[[length(configs) + 1]] <- list(method = m, speed_source = src,
                                                          cutoff = NA_real_, window = wi, order = o)
      }
    } else {
      for (wi in windows) {
        configs[[length(configs) + 1]] <- list(method = m, speed_source = src,
                                               cutoff = NA_real_, window = wi, order = NA_real_)
      }
    }
  }

  score_one <- function(cfg) {
    ff <- filter_watch_motion(
      w, method = cfg$method,
      cutoff = if (is.na(cfg$cutoff)) 0.3 else cfg$cutoff,
      order  = if (is.na(cfg$order)) 2 else cfg$order,
      window = if (is.na(cfg$window)) 11 else cfg$window,
      target_hz = target_hz, speed_source = cfg$speed_source
    )
    m <- motion_agreement(ff$time, ff$velocity, g$time, g$velocity, target_hz)
    cbind(data.frame(method = cfg$method, speed_source = cfg$speed_source,
                     cutoff = cfg$cutoff, window = cfg$window, order = cfg$order,
                     stringsAsFactors = FALSE),
          as.data.frame(m))
  }

  results <- tibble::as_tibble(do.call(rbind, lapply(configs, score_one)))
  results$combined_rmse <- results$speed_rmse + results$accel_rmse
  results <- results[order(results$combined_rmse), , drop = FALSE]

  list(results = results, baseline = baseline, best = results[1, , drop = FALSE])
}
