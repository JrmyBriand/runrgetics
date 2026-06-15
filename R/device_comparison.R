# Device comparison (watch vs gpexe): filter the watch with the tuned Task 2
# filter, derive matched motion/power channels for both devices on a common grid,
# and quantify their agreement.

#' @importFrom utils globalVariables
utils::globalVariables(c("value", "panel", "source"))

#' Per-channel agreement between two aligned signals
#'
#' Both signals are assumed already sampled on the same grid.
#' @noRd
channel_agreement <- function(channel, watch, ref) {
  ok <- is.finite(watch) & is.finite(ref)
  w <- watch[ok]; r <- ref[ok]
  tibble::tibble(
    channel    = channel,
    rmse       = sqrt(mean((w - r)^2)),
    bias       = mean(w - r),
    cor        = if (stats::sd(w) > 0 && stats::sd(r) > 0) stats::cor(w, r) else NA_real_,
    watch_mean = mean(w),
    ref_mean   = mean(r),
    ratio      = sum(abs(w)) / sum(abs(r)),
    n          = length(w)
  )
}

#' Build matched device channels on a common grid
#'
#' Returns the reference (gpexe) and filtered-watch motion/power channels
#' (velocity, acceleration, distance, external & metabolic power) interpolated
#' onto one shared uniform time grid.
#' @noRd
device_comparison_series <- function(paired_data,
                                     speed_source = "reported",
                                     filter_method = "butterworth",
                                     cutoff = 0.175,
                                     target_hz = 5,
                                     cost_running_flat = 3.6,
                                     slope_equation = "extended",
                                     reference = "gpexe",
                                     device = "polar_stryd") {
  if (!is.data.frame(paired_data) || !"source" %in% names(paired_data)) {
    stop("`paired_data` must be a data frame with a `source` column.")
  }
  g <- paired_data[paired_data$source == reference, , drop = FALSE]
  w <- paired_data[paired_data$source == device, , drop = FALSE]
  g <- g[order(g$time), , drop = FALSE]
  if (nrow(g) < 5L || nrow(w) < 5L) {
    stop("Both `reference` and `device` rows must be present in `paired_data`.")
  }

  # filtered watch (Task 2 filter)
  wf <- filter_watch_motion(w, method = filter_method, cutoff = cutoff,
                            target_hz = target_hz, speed_source = speed_source)

  # common grid over the overlap, at target_hz
  dt <- 1 / target_hz
  grid <- seq(max(min(g$time), min(wf$time)),
              min(max(g$time), max(wf$time)), by = dt)

  channels <- function(time, velocity) {
    v <- stats::approx(time, velocity, xout = grid, rule = 2)$y
    a <- central_diff(v, dt)
    # cost_running() is scalar (it branches on acceleration sign), so apply it
    # element-wise; external_power() is already vectorised.
    cr <- vapply(seq_along(v),
                 function(i) cost_running(a[i], v[i], cost_running_flat, slope_equation),
                 numeric(1))
    tibble::tibble(
      time            = grid,
      velocity        = v,
      acceleration    = a,
      distance        = cumdist(v, dt),
      external_power  = external_power(a, v),
      metabolic_power = cr * v
    )
  }

  list(
    reference = channels(g$time, g$velocity),
    device    = channels(wf$time, wf$velocity)
  )
}

#' Compare watch and gpexe motion and power signals
#'
#' Filters the Polar/Stryd watch signal with the tuned watch filter
#' ([filter_watch_motion()]), derives matched motion and power channels for both
#' the watch and the gpexe reference on a common grid, and reports their agreement
#' (speed, acceleration, distance, external power, metabolic power). gpexe is the
#' reference; positive `bias`/`ratio > 1` means the watch reads higher.
#'
#' @param paired_data A paired-device data frame with a `source` column (e.g.
#'   [sprint_mix_paired]) containing both the reference and watch rows.
#' @param speed_source Watch speed input passed to [filter_watch_motion()].
#' @param filter_method Watch filter family passed to [filter_watch_motion()].
#' @param cutoff Watch filter cut-off (Hz).
#' @param target_hz Common comparison grid rate (Hz).
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m) for metabolic power.
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param reference,device Values of the `source` column identifying the reference
#'   (gpexe) and watch rows.
#'
#' @returns A [tibble][tibble::tibble] with one row per channel (`speed`,
#'   `acceleration`, `distance`, `external_power`, `metabolic_power`) and columns
#'   `rmse`, `bias`, `cor`, `watch_mean`, `ref_mean`, `ratio` and `n`.
#' @export
#'
#' @examples
#' compare_devices(sprint_mix_paired)
compare_devices <- function(paired_data,
                            speed_source = "reported",
                            filter_method = "butterworth",
                            cutoff = 0.175,
                            target_hz = 5,
                            cost_running_flat = 3.6,
                            slope_equation = "extended",
                            reference = "gpexe",
                            device = "polar_stryd") {
  s <- device_comparison_series(paired_data, speed_source = speed_source,
                                filter_method = filter_method, cutoff = cutoff,
                                target_hz = target_hz, cost_running_flat = cost_running_flat,
                                slope_equation = slope_equation,
                                reference = reference, device = device)
  channels <- c(speed = "velocity", acceleration = "acceleration",
                distance = "distance", external_power = "external_power",
                metabolic_power = "metabolic_power")
  out <- Map(function(label, col) {
    channel_agreement(label, s$device[[col]], s$reference[[col]])
  }, names(channels), channels)
  dplyr::bind_rows(out)
}

#' Plot a watch-vs-gpexe signal comparison
#'
#' Overlays one signal (speed, acceleration, external or metabolic power) for the
#' filtered watch and the gpexe reference over time, in the shared package style.
#'
#' @inheritParams compare_devices
#' @param signal Which signal to plot: `"speed"`, `"acceleration"`,
#'   `"external_power"` or `"metabolic_power"`.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' plot_device_comparison(sprint_mix_paired, signal = "speed")
plot_device_comparison <- function(paired_data,
                                   signal = c("speed", "acceleration",
                                              "external_power", "metabolic_power"),
                                   speed_source = "reported",
                                   filter_method = "butterworth",
                                   cutoff = 0.175,
                                   target_hz = 5,
                                   cost_running_flat = 3.6,
                                   slope_equation = "extended",
                                   reference = "gpexe",
                                   device = "polar_stryd") {
  signal <- match.arg(signal)
  col <- c(speed = "velocity", acceleration = "acceleration",
           external_power = "external_power", metabolic_power = "metabolic_power")[[signal]]
  ylab <- c(speed = "Speed (m/s)", acceleration = "Acceleration (m/s^2)",
            external_power = "External power (W/kg)",
            metabolic_power = "Metabolic power (W/kg)")[[signal]]

  s <- device_comparison_series(paired_data, speed_source = speed_source,
                                filter_method = filter_method, cutoff = cutoff,
                                target_hz = target_hz, cost_running_flat = cost_running_flat,
                                slope_equation = slope_equation,
                                reference = reference, device = device)
  plot_data <- dplyr::bind_rows(
    tibble::tibble(time = s$reference$time, value = s$reference[[col]], device = reference),
    tibble::tibble(time = s$device$time,    value = s$device[[col]],    device = "watch (filtered)")
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = value, colour = device)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      runrgetics_pal(2), c(reference, "watch (filtered)"))) +
    ggplot2::labs(title = paste0("Device comparison: ", signal),
                  x = "Time (s)", y = ylab, colour = "Device") +
    theme_runrgetics()
}

#' Element-wise metabolic power from acceleration and velocity
#' @noRd
metabolic_power_vec <- function(acceleration, velocity, cost_running_flat, slope_equation) {
  cr <- vapply(seq_along(velocity),
               function(i) cost_running(acceleration[i], velocity[i],
                                        cost_running_flat, slope_equation),
               numeric(1))
  cr * velocity
}

#' Compare watch speed/power derivations for one sprint
#'
#' For a single detected sprint, derives the watch motion from two sources -
#' the watch's reported speed and speed derived from raw GPS position - each
#' filtered with [filter_watch_motion()], and assembles a tidy comparison of four
#' channels: speed, acceleration, metabolic power (`cost_running()`) and external
#' power (`external_power()`). The external-power panel also includes the Stryd
#' unit's measured power (`power_w`) expressed per kilogram using `body_mass`.
#'
#' @param paired_data A paired-device data frame with a `source` column (e.g.
#'   [ten_200_sprints_paired]); the device rows must carry a `power_w` (Stryd, W)
#'   column.
#' @param sprint_id Which detected sprint to compare (its `sprint_id`).
#' @param sprints Optional sprint table from [detect_sprints()] (detected on the
#'   reference); if `NULL` (default), sprints are detected.
#' @param body_mass Athlete body mass (kg) used to convert the Stryd power to W/kg.
#' @param filter_method,cutoff,target_hz Passed to [filter_watch_motion()].
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param reference,device Values of the `source` column identifying the reference
#'   (gpexe) and watch rows.
#' @param margin Seconds of data to include on each side of the detected sprint.
#'
#' @returns A [tibble][tibble::tibble] with columns `time` (s, from the sprint
#'   start), `panel`, `source` (`"watch"`, `"gps"`, `"stryd"`) and `value`.
#' @export
#'
#' @examples
#' compare_sprint_power_sources(ten_200_sprints_paired, sprint_id = 1, body_mass = 67)
compare_sprint_power_sources <- function(paired_data, sprint_id = 1, sprints = NULL,
                                         body_mass = 67,
                                         filter_method = "butterworth", cutoff = 0.175,
                                         target_hz = 5, cost_running_flat = 3.6,
                                         slope_equation = "extended",
                                         reference = "gpexe", device = "polar_stryd",
                                         margin = 3) {
  if (!is.data.frame(paired_data) || !"source" %in% names(paired_data)) {
    stop("`paired_data` must be a data frame with a `source` column.")
  }
  if (!is.numeric(body_mass) || body_mass <= 0) stop("`body_mass` must be a positive number.")
  g <- paired_data[paired_data$source == reference, , drop = FALSE]
  w <- paired_data[paired_data$source == device, , drop = FALSE]
  g <- g[order(g$time), , drop = FALSE]
  w <- w[order(w$time), , drop = FALSE]
  if (!"power_w" %in% names(w)) {
    stop("The device rows must contain a `power_w` (Stryd) column for this comparison.")
  }
  if (is.null(sprints)) sprints <- detect_sprints(g)
  sp <- sprints[sprints$sprint_id == sprint_id, , drop = FALSE]
  if (nrow(sp) == 0) stop("`sprint_id` not found in the detected sprints.")

  win <- w[w$time >= sp$start_time - margin & w$time <= sp$end_time + margin, , drop = FALSE]
  if (nrow(win) < 5L) stop("Too few watch samples in the selected sprint window.")

  wf_rep <- filter_watch_motion(win, method = filter_method, cutoff = cutoff,
                                target_hz = target_hz, speed_source = "reported")
  wf_gps <- filter_watch_motion(win, method = filter_method, cutoff = cutoff,
                                target_hz = target_hz, speed_source = "gps")

  met_rep <- metabolic_power_vec(wf_rep$acceleration, wf_rep$velocity, cost_running_flat, slope_equation)
  met_gps <- metabolic_power_vec(wf_gps$acceleration, wf_gps$velocity, cost_running_flat, slope_equation)
  ext_rep <- external_power(wf_rep$acceleration, wf_rep$velocity)
  ext_gps <- external_power(wf_gps$acceleration, wf_gps$velocity)
  stryd <- stats::approx(win$time, win$power_w, xout = wf_rep$time, rule = 2)$y / body_mass

  t0 <- min(wf_rep$time)
  mk <- function(panel, source, time, value) {
    tibble::tibble(time = time - t0, panel = panel, source = source, value = value)
  }
  out <- dplyr::bind_rows(
    mk("Speed (m/s)", "watch", wf_rep$time, wf_rep$velocity),
    mk("Speed (m/s)", "gps", wf_gps$time, wf_gps$velocity),
    mk("Acceleration (m/s^2)", "watch", wf_rep$time, wf_rep$acceleration),
    mk("Acceleration (m/s^2)", "gps", wf_gps$time, wf_gps$acceleration),
    mk("Metabolic power (W/kg)", "watch", wf_rep$time, met_rep),
    mk("Metabolic power (W/kg)", "gps", wf_gps$time, met_gps),
    mk("External power (W/kg)", "watch", wf_rep$time, ext_rep),
    mk("External power (W/kg)", "gps", wf_gps$time, ext_gps),
    mk("External power (W/kg)", "stryd", wf_rep$time, stryd)
  )
  out$panel <- factor(out$panel, levels = c("Speed (m/s)", "Acceleration (m/s^2)",
                                            "Metabolic power (W/kg)", "External power (W/kg)"))
  out$source <- factor(out$source, levels = c("watch", "gps", "stryd"))
  out
}

#' Plot the watch speed/power source comparison for one sprint
#'
#' Four-panel comparison for a single sprint of the watch's reported-speed and
#' GPS-derived derivations across speed, acceleration, metabolic power and external
#' power, with the Stryd-measured external power overlaid on the external-power panel.
#'
#' @inheritParams compare_sprint_power_sources
#' @param ... Additional arguments passed to [compare_sprint_power_sources()].
#'
#' @returns A ggplot object (2x2 facets).
#' @export
#'
#' @examples
#' plot_sprint_power_sources(ten_200_sprints_paired, sprint_id = 1, body_mass = 67)
plot_sprint_power_sources <- function(paired_data, sprint_id = 1, sprints = NULL,
                                      body_mass = 67, ...) {
  d <- compare_sprint_power_sources(paired_data, sprint_id = sprint_id,
                                    sprints = sprints, body_mass = body_mass, ...)
  ggplot2::ggplot(d, ggplot2::aes(x = time, y = value, colour = source)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 2) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      runrgetics_pal(3), c("watch", "gps", "stryd"))) +
    ggplot2::labs(title = paste("Sprint", sprint_id, "- speed and power sources"),
                  x = "Time (s)", y = NULL, colour = NULL) +
    theme_runrgetics()
}
