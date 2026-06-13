## Tune and report the watch motion filter against the gpexe reference (Task 2)
#
# Reports, per paired session:
#   - unfiltered watch baseline vs gpexe (before),
#   - best configuration per filter family / speed source,
#   - the overall best, and a before/after overlay plot.
#
# Diagnostics only (writes PNGs to data-raw/diagnostics/, gitignored); saves no data.
#   Rscript data-raw/tune_watch_filter.R

suppressMessages({
  library(ggplot2)
})
source("R/watch_filter.R")  # functions under development (no other pkg deps needed)

DIAG <- "data-raw/diagnostics"
dir.create(DIAG, showWarnings = FALSE, recursive = TRUE)

# round numeric columns for compact console display
round_df <- function(df, digits = 3) {
  df <- as.data.frame(df)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  df
}

sessions <- c("sprint_mix_paired", "ten_200_sprints_paired")

best_per_session <- list()

for (nm in sessions) {
  load(file.path("data", paste0(nm, ".rda")))
  d <- get(nm)

  tuned <- tune_watch_filter(d)

  cat(sprintf("\n================ %s ================\n", nm))
  cat("-- unfiltered baseline (before) --\n")
  print(as.data.frame(round_df(tuned$baseline)))

  cat("\n-- best configuration per family / speed source --\n")
  by_grp <- split(tuned$results, list(tuned$results$method, tuned$results$speed_source))
  top <- do.call(rbind, lapply(by_grp, function(x) x[which.min(x$combined_rmse), ]))
  top <- top[order(top$combined_rmse), ]
  print(as.data.frame(round_df(top)))

  cat("\n-- overall best --\n")
  print(as.data.frame(round_df(tuned$best)))
  best_per_session[[nm]] <- tuned$best

  ## ---- before/after overlay plot ----
  b <- tuned$best
  g <- d[d$source == "gpexe", ]
  w <- d[d$source == "polar_stryd", ]
  g <- g[order(g$time), ]; w <- w[order(w$time), ]
  filt <- filter_watch_motion(
    w, method = b$method,
    cutoff = if (is.na(b$cutoff)) 0.3 else b$cutoff,
    order  = if (is.na(b$order)) 2 else b$order,
    window = if (is.na(b$window)) 11 else b$window,
    speed_source = b$speed_source
  )
  raw_v <- if (b$speed_source == "reported") w$velocity else gps_speed(w$time, w$latitude, w$longitude)

  spd <- rbind(
    data.frame(time = g$time,    velocity = g$velocity, signal = "gpexe (reference)"),
    data.frame(time = w$time,    velocity = raw_v,      signal = "watch raw"),
    data.frame(time = filt$time, velocity = filt$velocity, signal = "watch filtered")
  )
  cols <- c("gpexe (reference)" = "#1b9e77", "watch raw" = "#bdbdbd", "watch filtered" = "#d95f02")

  p_zoom <- ggplot(subset(spd, time >= -5 & time <= 45),
                   aes(time, velocity, color = signal)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = cols) +
    labs(title = paste0(nm, " — speed, first sprint"),
         x = "time (s)", y = "velocity (m/s)") + theme_minimal()
  p_full <- ggplot(spd, aes(time, velocity, color = signal)) +
    geom_line(linewidth = 0.3, alpha = 0.85) +
    scale_color_manual(values = cols) +
    labs(title = "speed, full session", x = "time (s)", y = "velocity (m/s)") +
    theme_minimal()

  # acceleration (the derivative — where filtering matters most)
  dt_g <- 1 / 5
  ga <- central_diff(stats::approx(g$time, g$velocity, xout = filt$time, rule = 2)$y, dt_g)
  raw_a <- central_diff(stats::approx(w$time, raw_v, xout = filt$time, rule = 2)$y, dt_g)
  acc <- rbind(
    data.frame(time = filt$time, acceleration = ga,            signal = "gpexe (reference)"),
    data.frame(time = filt$time, acceleration = raw_a,         signal = "watch raw"),
    data.frame(time = filt$time, acceleration = filt$acceleration, signal = "watch filtered")
  )
  p_acc <- ggplot(subset(acc, time >= -5 & time <= 45),
                  aes(time, acceleration, color = signal)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = cols) +
    labs(title = "acceleration, first sprint", x = "time (s)", y = "acceleration (m/s^2)") +
    theme_minimal()

  plt <- patchwork::wrap_plots(p_zoom, p_acc, p_full, ncol = 1) +
    patchwork::plot_layout(guides = "collect")
  png_path <- file.path(DIAG, paste0(nm, "_filter_overlay.png"))
  ggsave(png_path, plt, width = 9, height = 11, dpi = 110)
  cat("  overlay plot ->", png_path, "\n")
}

cat("\n================ recommended default ================\n")
bp <- do.call(rbind, best_per_session)
print(as.data.frame(round_df(cbind(session = names(best_per_session), bp))))
