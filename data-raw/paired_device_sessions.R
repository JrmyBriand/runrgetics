## code to prepare `sprint_mix_paired` and `ten_200_sprints_paired`
#
# Paired, simultaneously-recorded track sessions:
#   gpexe          (semicolon CSV, ~25 Hz, gpexe's onboard filter -> reference signal)
#   Polar / Stryd  (.fit, ~1 Hz, read via the package's get_fit_data_frame())
#
# Per session this script:
#   1. reads both modalities,
#   2. trims the first and last 5 min of each recording,
#   3. auto-detects the start-line standstill by combining three signals --
#        (a) sustained speed ~ 0, (b) GPS clustered at one point, and
#        (c) the UTC wall-clock to cross-reference the two devices --
#      then sets a common t = 0 at the moment motion resumes and aligns position,
#   4. merges the two modalities into one tidy long frame with a `source` column,
#      keeping each device's native sampling rate (gpexe downsampled for the lazy
#      dataset only; full-resolution raw is shipped gzipped under inst/extdata).
#
# Run modes:
#   Rscript data-raw/paired_device_sessions.R
#       -> full build: writes inst/extdata/*.csv.gz + *.fit and data/*.rda
#   RUNRGETICS_DETECT_ONLY=1 Rscript data-raw/paired_device_sessions.R
#       -> diagnostics only: prints detection summary, writes overlay PNGs to
#          data-raw/diagnostics/, saves nothing.

suppressMessages({
  library(dplyr)
  library(ggplot2)
})
# Reuse the package's existing FIT reader without modifying it.
source("R/compute_running_power.R")

detect_only <- identical(Sys.getenv("RUNRGETICS_DETECT_ONLY"), "1")

# ---- configuration -----------------------------------------------------------

TZ_LOCAL       <- "America/Montreal"  # gpexe 'date time' is local wall-clock (EDT)
TRIM_S         <- 300                 # 5 min head/tail trim
SPEED_THRESH   <- 0.4                 # m/s; standstill speed gate
MIN_STILL_S    <- 3                   # s; minimum sustained standstill duration
GPS_RADIUS_M   <- 6                   # m; max GPS spread to count as "parked"
MATCH_TOL_S    <- 30                  # s; max wall-clock gap between matched parks
GPS_MATCH_M    <- 25                  # m; max centroid distance for the same start line
SPRINT_THRESH  <- 6                   # m/s; velocity defining a sprint (t=0 anchor)
GPEXE_TARGET_HZ <- 5                  # gpexe downsample target for the lazy dataset

EXTDATA <- "inst/extdata"
DIAG    <- "data-raw/diagnostics"
dir.create(DIAG, showWarnings = FALSE, recursive = TRUE)

sessions <- list(
  list(name = "sprint_mix",
       gpexe = "data-raw/sprint_mix_gpexe.csv",
       fit   = "data-raw/sprint_mix_polar_stryd.fit",
       dataset = "sprint_mix_paired"),
  list(name = "ten_200_sprints",
       gpexe = "data-raw/ten_200_sprints_gpexe.csv",
       fit   = "data-raw/ten_200_sprints_polar_stryd.fit",
       dataset = "ten_200_sprints_paired")
)

# ---- helpers -----------------------------------------------------------------

# Great-circle distance (m) between two WGS84 points (vectorised).
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

read_gpexe <- function(path) {
  raw <- utils::read.csv(path, sep = ";", dec = ".",
                         check.names = FALSE, stringsAsFactors = FALSE)
  tibble::tibble(
    source       = "gpexe",
    datetime_utc = as.POSIXct(raw[["date time"]], tz = TZ_LOCAL,
                              format = "%Y-%m-%d %H:%M:%OS"),
    elapsed_s    = raw[["time (s)"]],
    velocity     = raw[["speed (km/h)"]] / 3.6,             # km/h -> m/s
    latitude     = raw[["latitude"]],
    longitude    = raw[["longitude"]],
    power_w_kg              = raw[["power (W/kg)"]],
    external_power_positive = raw[["external_power_positive (W/kg)"]],
    raw_speed    = raw[["raw_speed (km/h)"]] / 3.6,         # km/h -> m/s
    acc          = raw[["acc (m/s2)"]]
  )
}

read_watch <- function(path) {
  fit <- get_fit_data_frame(path)   # FITfileR speed is m/s; positions in degrees
  tibble::tibble(
    source       = "polar_stryd",
    datetime_utc = as.POSIXct(fit$timestamp, tz = "UTC"),
    velocity     = fit$speed,
    latitude     = fit$position_lat,
    longitude    = fit$position_long,
    power_w      = fit$power,        # Stryd: ABSOLUTE watts (not W/kg)
    heart_rate   = fit$heart_rate,
    cadence      = fit$cadence,
    altitude     = fit$altitude
  ) |>
    dplyr::arrange(datetime_utc) |>
    dplyr::mutate(elapsed_s = as.numeric(datetime_utc) -
                    as.numeric(min(datetime_utc)))
}

trim_recording <- function(df, trim = TRIM_S) {
  dplyr::filter(df,
                elapsed_s >= min(elapsed_s) + trim,
                elapsed_s <= max(elapsed_s) - trim)
}

# Centered rolling mean, ~1 s window from the sampling rate.
smooth_speed <- function(v, fs) {
  k <- max(1, round(fs))
  if (k %% 2 == 0) k <- k + 1
  if (k == 1) return(v)
  as.numeric(zoo::rollmean(v, k = k, fill = NA, align = "center"))
}

# All qualifying standstills (speed gate + GPS-cluster gate), one row each.
detect_standstills <- function(df) {
  fs <- 1 / stats::median(diff(df$elapsed_s), na.rm = TRUE)
  v  <- smooth_speed(df$velocity, fs)
  below <- !is.na(v) & v < SPEED_THRESH

  r      <- rle(below)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  cand   <- which(r$values)

  out <- lapply(cand, function(i) {
    idx <- starts[i]:ends[i]
    clat <- stats::median(df$latitude[idx],  na.rm = TRUE)
    clon <- stats::median(df$longitude[idx], na.rm = TRUE)
    resume <- min(ends[i] + 1, nrow(df))
    tibble::tibble(
      start_s      = df$elapsed_s[starts[i]],
      end_s        = df$elapsed_s[ends[i]],
      dur_s        = df$elapsed_s[ends[i]] - df$elapsed_s[starts[i]],
      gps_spread_m = max(haversine_m(df$latitude[idx], df$longitude[idx],
                                     clat, clon), na.rm = TRUE),
      centroid_lat = clat,
      centroid_lon = clon,
      start_utc    = as.numeric(df$datetime_utc[starts[i]]),
      end_utc      = as.numeric(df$datetime_utc[ends[i]]),
      resume_utc   = as.numeric(df$datetime_utc[resume])
    )
  })
  res <- dplyr::bind_rows(out)
  if (nrow(res) == 0) return(res)
  dplyr::filter(res, dur_s >= MIN_STILL_S, gps_spread_m <= GPS_RADIUS_M)
}

# Match the gpexe/watch standstill pair that agrees on BOTH wall-clock (overlapping
# UTC intervals, within tolerance) AND GPS location (same physical start line, modulo
# device GPS bias). Of the mutually-agreed parks, take the one immediately preceding
# the first sprint -- i.e. the start-line standstill before the athlete launches.
pick_standstill_pair <- function(g_cand, w_cand, first_sprint_s) {
  if (nrow(g_cand) == 0 || nrow(w_cand) == 0) {
    stop("No qualifying standstill found on one of the devices; relax thresholds.")
  }
  grid <- expand.grid(gi = seq_len(nrow(g_cand)), wi = seq_len(nrow(w_cand)))
  # wall-clock gap between intervals (0 if they overlap)
  grid$wall_gap <- pmax(0,
    pmax(g_cand$start_utc[grid$gi], w_cand$start_utc[grid$wi]) -
    pmin(g_cand$end_utc[grid$gi],   w_cand$end_utc[grid$wi]))
  grid$gps_dist <- haversine_m(
    g_cand$centroid_lat[grid$gi], g_cand$centroid_lon[grid$gi],
    w_cand$centroid_lat[grid$wi], w_cand$centroid_lon[grid$wi])

  ok <- grid[grid$wall_gap <= MATCH_TOL_S & grid$gps_dist <= GPS_MATCH_M, , drop = FALSE]
  if (nrow(ok) == 0) {
    stop("No gpexe/watch standstill pair agrees on both wall-clock and GPS; ",
         "inspect candidates / relax MATCH_TOL_S or GPS_MATCH_M.")
  }
  # the agreed park whose gpexe interval ends latest but before the first sprint
  before <- ok[g_cand$end_s[ok$gi] <= first_sprint_s, , drop = FALSE]
  best <- if (nrow(before) > 0) {
    before[which.max(g_cand$end_s[before$gi]), ]
  } else {
    ok[order(g_cand$start_utc[ok$gi]), , drop = FALSE][1, ]  # fallback: earliest agreed
  }
  list(g = g_cand[best$gi, ], w = w_cand[best$wi, ],
       wallclock_gap_s = best$wall_gap, gps_match_m = best$gps_dist)
}

# Device clock offset (s) to ADD to watch UTC so it aligns to the gpexe clock,
# from velocity-signal cross-correlation over the overlapping span. Robust because
# it locks onto the sharp sprint accelerations, not the fuzzy standstill edges.
estimate_clock_offset <- function(g, w, max_lag = 30, grid_hz = 5) {
  tg <- as.numeric(g$datetime_utc)
  tw <- as.numeric(w$datetime_utc)
  t_lo <- max(min(tg), min(tw))
  t_hi <- min(max(tg), max(tw))
  grid <- seq(t_lo, t_hi, by = 1 / grid_hz)
  gv <- stats::approx(tg, g$velocity, grid, rule = 2)$y
  lags <- seq(-max_lag, max_lag, by = 1 / grid_hz)
  cors <- vapply(lags, function(d) {
    wv <- stats::approx(tw + d, w$velocity, grid, rule = 2)$y
    stats::cor(gv, wv)
  }, numeric(1))
  list(offset_s = lags[which.max(cors)], max_cor = max(cors))
}

# ---- per-session processing --------------------------------------------------

process_session <- function(cfg) {
  message(sprintf("\n==== %s ====", cfg$name))

  g <- trim_recording(read_gpexe(cfg$gpexe))
  w <- trim_recording(read_watch(cfg$fit))

  g_cand <- detect_standstills(g)
  w_cand <- detect_standstills(w)

  # first sprint = first gpexe sample whose smoothed speed exceeds SPRINT_THRESH
  fs_g <- 1 / stats::median(diff(g$elapsed_s), na.rm = TRUE)
  first_sprint_s <- g$elapsed_s[which(smooth_speed(g$velocity, fs_g) > SPRINT_THRESH)[1]]

  pair <- pick_standstill_pair(g_cand, w_cand, first_sprint_s)
  g0 <- pair$g; w0 <- pair$w

  # Device clock offset from velocity cross-correlation (robust across the session).
  cc <- estimate_clock_offset(g, w)
  clock_offset_s <- cc$offset_s

  # Common t = 0 at the gpexe start-line resume (sprint launch); shift the watch onto
  # the gpexe clock by the estimated offset.
  t0 <- g0$resume_utc
  g$time <- as.numeric(g$datetime_utc) - t0
  w$time <- (as.numeric(w$datetime_utc) + clock_offset_s) - t0

  # Position alignment: rigid-shift the watch onto the gpexe start-line origin.
  shift_lat <- g0$centroid_lat - w0$centroid_lat
  shift_lon <- g0$centroid_lon - w0$centroid_lon
  start_offset_m <- haversine_m(w0$centroid_lat, w0$centroid_lon,
                                g0$centroid_lat, g0$centroid_lon)
  w$latitude  <- w$latitude  + shift_lat
  w$longitude <- w$longitude + shift_lon

  # ---- diagnostics printout ----
  cat(sprintf(
    paste0("  gpexe standstills found: %d | watch standstills found: %d\n",
           "  chosen gpexe park : %.1f-%.1f s (dur %.1f s, GPS spread %.1f m)\n",
           "  chosen watch park : %.1f-%.1f s (dur %.1f s, GPS spread %.1f m)\n",
           "  matched-park wall-clock gap : %.1f s | GPS distance : %.1f m\n",
           "  clock offset (cross-corr, add to watch) : %.2f s (max r = %.3f)\n",
           "  start-line GPS offset (watch->gpexe) : %.1f m\n",
           "  gpexe peak velocity: %.2f m/s | watch peak velocity: %.2f m/s\n"),
    nrow(g_cand), nrow(w_cand),
    g0$start_s, g0$end_s, g0$dur_s, g0$gps_spread_m,
    w0$start_s, w0$end_s, w0$dur_s, w0$gps_spread_m,
    pair$wallclock_gap_s, pair$gps_match_m, clock_offset_s, cc$max_cor, start_offset_m,
    max(g$velocity, na.rm = TRUE), max(w$velocity, na.rm = TRUE)
  ))

  # ---- overlay diagnostic plot ----
  both <- dplyr::bind_rows(
    dplyr::select(g, source, time, velocity, latitude, longitude),
    dplyr::select(w, source, time, velocity, latitude, longitude)
  )
  cols <- c(gpexe = "#1b9e77", polar_stryd = "#d95f02")

  p_zoom <- ggplot(dplyr::filter(both, time >= -20, time <= 40),
                   aes(time, velocity, color = source)) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = cols) +
    labs(title = paste0(cfg$name, " — velocity around synced t=0 (first launch)"),
         x = "time (s, 0 = motion resume)", y = "velocity (m/s)") +
    theme_minimal()

  p_full <- ggplot(both, aes(time, velocity, color = source)) +
    geom_line(linewidth = 0.3, alpha = 0.8) +
    scale_color_manual(values = cols) +
    labs(title = "full-session velocity (synced)",
         x = "time (s)", y = "velocity (m/s)") +
    theme_minimal()

  p_gps <- ggplot(both, aes(longitude, latitude, color = source)) +
    geom_path(linewidth = 0.3, alpha = 0.7) +
    annotate("point", x = g0$centroid_lon, y = g0$centroid_lat,
             color = "black", size = 2) +
    scale_color_manual(values = cols) +
    coord_quickmap() +
    labs(title = "GPS tracks (watch aligned to gpexe start line ●)",
         x = "longitude", y = "latitude") +
    theme_minimal()

  plt <- patchwork::wrap_plots(p_zoom, p_full, p_gps, ncol = 1) +
    patchwork::plot_layout(guides = "collect")
  png_path <- file.path(DIAG, paste0(cfg$name, "_sync_overlay.png"))
  ggsave(png_path, plt, width = 9, height = 11, dpi = 110)
  message("  overlay plot -> ", png_path)

  list(gpexe = g, watch = w, dataset = cfg$dataset)
}

# ---- merge + save (full build only) -----------------------------------------

downsample_gpexe <- function(g, target_hz) {
  fs <- 1 / stats::median(diff(g$time), na.rm = TRUE)
  step <- max(1L, as.integer(round(fs / target_hz)))
  g[seq(1L, nrow(g), by = step), , drop = FALSE]
}

build_merged <- function(res) {
  g <- downsample_gpexe(res$gpexe, GPEXE_TARGET_HZ)
  merged <- dplyr::bind_rows(g, res$watch) |>
    dplyr::arrange(time, source) |>
    dplyr::relocate(source, time, datetime_utc, velocity, latitude, longitude)
  merged
}

gzip_copy <- function(src, dest) {
  ic <- file(src, "rb"); on.exit(close(ic), add = TRUE)
  oc <- gzfile(dest, "wb", compression = 9); on.exit(close(oc), add = TRUE)
  writeBin(readBin(ic, "raw", n = file.size(src)), oc)
}

# ---- run ---------------------------------------------------------------------

# RUNRGETICS_FUNCS_ONLY=1 sources the helpers without executing the pipeline
# (used by the candidate-inspection diagnostic during development).
if (!identical(Sys.getenv("RUNRGETICS_FUNCS_ONLY"), "1")) {

results <- lapply(sessions, process_session)

if (!detect_only) {
  dir.create(EXTDATA, showWarnings = FALSE, recursive = TRUE)
  for (i in seq_along(sessions)) {
    cfg <- sessions[[i]]
    gzip_copy(cfg$gpexe, file.path(EXTDATA, paste0(basename(cfg$gpexe), ".gz")))
    file.copy(cfg$fit, file.path(EXTDATA, basename(cfg$fit)), overwrite = TRUE)
  }

  sprint_mix_paired      <- build_merged(results[[1]])
  ten_200_sprints_paired <- build_merged(results[[2]])

  usethis::use_data(sprint_mix_paired, overwrite = TRUE)
  usethis::use_data(ten_200_sprints_paired, overwrite = TRUE)

  cat("\n---- saved dataset summary ----\n")
  for (nm in c("sprint_mix_paired", "ten_200_sprints_paired")) {
    d <- get(nm)
    cat(sprintf("  %-24s %6d rows x %2d cols | data/%s.rda = %s\n",
                nm, nrow(d), ncol(d), nm,
                format(structure(file.size(file.path("data", paste0(nm, ".rda"))),
                                 class = "object_size"), units = "auto")))
  }
}

}  # end RUNRGETICS_FUNCS_ONLY guard
