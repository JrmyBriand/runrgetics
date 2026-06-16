# Cumulative metabolic energy by pathway: integrate each pathway's power over the
# sprint to show how energy (J/kg) accumulates as the sprint progresses.

#' @importFrom utils globalVariables
utils::globalVariables(c("x_value", "cumulative_energy", "pathway"))

#' Cumulative energy by pathway for one sprint
#'
#' Decomposes a sprint's metabolic power (alactic / lactic / aerobic) and returns
#' the cumulative energy (running integral of power, J/kg) of each pathway, indexed
#' by both time and distance from the sprint start. The cumulative `total` is the
#' sum of the three pathways.
#'
#' @inheritParams plot_sprint_bioenergetics
#' @param ... Additional arguments forwarded to the bioenergetic fit (e.g.
#'   `fit_mu`, `fit_sigma`, `fit_k2`, `trim`, `decel_threshold`).
#'
#' @returns A [tibble][tibble::tibble] (long) with `sprint_id`, `time` (s),
#'   `distance` (m), `pathway` (`total`/`alactic`/`lactic`/`aerobic`) and
#'   `cumulative_energy` (J/kg).
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' head(sprint_energy_data(gpexe, sprint_id = 1, maximal_aerobic_power = 27))
sprint_energy_data <- function(motion_data, sprint_id = 1, sprints = NULL,
                               maximal_aerobic_power = 27, ...) {
  if (is.null(sprints)) sprints <- detect_sprints(motion_data)
  series <- workout_sprint_series(motion_data, sprints)
  s <- series[series$sprint_id == sprint_id, , drop = FALSE]
  if (nrow(s) == 0) stop("`sprint_id` not found in the detected sprints.")
  ser <- sprint_bioenergetic_series(s, maximal_aerobic_power = maximal_aerobic_power, ...)
  if (all(is.na(ser$total))) stop("The bioenergetic model fit failed for this sprint.")

  dt <- stats::median(diff(ser$time), na.rm = TRUE)
  e_al <- cumdist(ser$alactic, dt)   # cumulative trapezoidal integral (uniform dt)
  e_la <- cumdist(ser$lactic, dt)
  e_aer <- cumdist(ser$aerobic, dt)

  mk <- function(pathway, cumulative_energy) {
    tibble::tibble(sprint_id = sprint_id, time = ser$time, distance = ser$distance,
                   pathway = pathway, cumulative_energy = cumulative_energy)
  }
  out <- dplyr::bind_rows(
    mk("total", e_al + e_la + e_aer),
    mk("alactic", e_al), mk("lactic", e_la), mk("aerobic", e_aer)
  )
  out$pathway <- factor(out$pathway, levels = c("total", "alactic", "lactic", "aerobic"))
  out
}

#' Plot cumulative energy by pathway for one sprint
#'
#' Shows how metabolic energy accumulates as the sprint progresses: the alactic,
#' lactic and aerobic cumulative energies are drawn as stacked filled areas, so the
#' top of the stack at any point is the cumulative total energy (J/kg). For example,
#' if 350 J/kg are expended over the sprint and half by 50 m, the stack reaches 175
#' at 50 m.
#'
#' @inheritParams plot_sprint_bioenergetics
#' @param x Horizontal axis: `"distance"` (m, default) or `"time"` (s).
#' @param ... Additional arguments passed to [sprint_energy_data()].
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' plot_sprint_energy(gpexe, sprint_id = 1, maximal_aerobic_power = 27)
plot_sprint_energy <- function(motion_data, sprint_id = 1, sprints = NULL,
                               maximal_aerobic_power = 27, x = c("distance", "time"), ...) {
  x <- match.arg(x)
  d <- sprint_energy_data(motion_data, sprint_id = sprint_id, sprints = sprints,
                          maximal_aerobic_power = maximal_aerobic_power, ...)
  d <- d[d$pathway != "total", , drop = FALSE]
  d$pathway <- droplevels(d$pathway)
  plot_data <- tibble::tibble(x_value = d[[x]], cumulative_energy = d$cumulative_energy,
                              pathway = d$pathway)
  xlab <- c(distance = "Distance (m)", time = "Time (s)")[[x]]

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = x_value, y = cumulative_energy, fill = pathway)) +
    ggplot2::geom_area(position = "stack", alpha = 0.85) +
    ggplot2::scale_fill_manual(values = runrgetics_pal(3)) +
    ggplot2::labs(title = paste("Sprint", sprint_id, "- cumulative energy by pathway"),
                  x = xlab, y = "Cumulative energy (J/kg)", fill = NULL) +
    theme_runrgetics()
}

#' Plot cumulative energy by pathway across a workout's sprints
#'
#' Superposes the workout's sprints to compare how cumulative energy builds with
#' distance (or time). Faceted by pathway (`total`, `alactic`, `lactic`, `aerobic`),
#' one coloured line per sprint. The aerobic curves typically superimpose closely
#' (similar duration and maximal aerobic power), while the anaerobic pathways spread
#' the inter-sprint differences.
#'
#' @inheritParams plot_sprint_energy
#' @param sprints Optional sprint table from [detect_sprints()]; if `NULL` (default),
#'   sprints are detected.
#' @param ... Additional arguments passed to [sprint_energy_data()].
#'
#' @returns A ggplot object (faceted by pathway).
#' @export
#'
#' @examples
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' plot_workout_energy(gpexe, maximal_aerobic_power = 27)
plot_workout_energy <- function(motion_data, sprints = NULL, maximal_aerobic_power = 27,
                                x = c("distance", "time"), ...) {
  x <- match.arg(x)
  if (is.null(sprints)) sprints <- detect_sprints(motion_data)
  parts <- lapply(sprints$sprint_id, function(id) {
    tryCatch(
      sprint_energy_data(motion_data, sprint_id = id, sprints = sprints,
                         maximal_aerobic_power = maximal_aerobic_power, ...),
      error = function(e) NULL)
  })
  d <- dplyr::bind_rows(parts)
  if (nrow(d) == 0) stop("No sprints could be analysed.")
  d$sprint_label <- factor(paste("Sprint", d$sprint_id),
                           levels = paste("Sprint", sort(unique(d$sprint_id))))
  plot_data <- tibble::tibble(x_value = d[[x]], cumulative_energy = d$cumulative_energy,
                              pathway = d$pathway, sprint_label = d$sprint_label)
  xlab <- c(distance = "Distance (m)", time = "Time (s)")[[x]]

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = x_value, y = cumulative_energy, colour = sprint_label)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::facet_wrap(~pathway, scales = "free_y") +
    ggplot2::scale_colour_manual(values = runrgetics_pal(nlevels(plot_data$sprint_label))) +
    ggplot2::labs(title = "Cumulative energy by pathway across sprints",
                  x = xlab, y = "Cumulative energy (J/kg)", colour = NULL) +
    theme_runrgetics()
}
