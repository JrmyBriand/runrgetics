# High-level helpers to run the sprint-training analysis on a gpexe session file or
# a folder of sessions, and to assemble the per-session results.

#' Read a gpexe CSV export into a motion data frame
#'
#' Reads a gpexe semicolon-separated CSV (decimal point) and returns a tidy motion
#' data frame with `time` (s) and `velocity` (m/s, from the filtered `speed (km/h)`
#' column). Available extra channels (raw speed, acceleration, power, position) are
#' carried through. Column order is irrelevant (columns are matched by name).
#' Gzipped files (`.csv.gz`) are read transparently.
#'
#' @param file Path to a gpexe `.csv` (or `.csv.gz`) export.
#'
#' @returns A [tibble][tibble::tibble] with `time` (s) and `velocity` (m/s), plus any
#'   of `raw_speed`, `acc`, `power_w_kg`, `external_power_positive`, `gpexe_distance`,
#'   `latitude`, `longitude` that were present.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata", "sprint_mix_gpexe.csv.gz", package = "runrgetics")
#' if (nzchar(f)) head(read_gpexe_csv(f))
#' }
read_gpexe_csv <- function(file) {
  if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
  raw <- utils::read.csv(file, sep = ";", dec = ".", check.names = FALSE,
                         stringsAsFactors = FALSE)
  required <- c("time (s)", "speed (km/h)")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("`", file, "` does not look like a gpexe export (missing column(s): ",
         paste(missing, collapse = ", "), ").", call. = FALSE)
  }
  out <- tibble::tibble(time = raw[["time (s)"]],
                        velocity = raw[["speed (km/h)"]] / 3.6)
  extras <- list(
    raw_speed                = c(col = "raw_speed (km/h)", div = 3.6),
    acc                      = c(col = "acc (m/s2)", div = 1),
    power_w_kg               = c(col = "power (W/kg)", div = 1),
    external_power_positive  = c(col = "external_power_positive (W/kg)", div = 1),
    gpexe_distance           = c(col = "dist (m)", div = 1),
    latitude                 = c(col = "latitude", div = 1),
    longitude                = c(col = "longitude", div = 1)
  )
  for (nm in names(extras)) {
    col <- extras[[nm]][["col"]]
    if (col %in% names(raw)) out[[nm]] <- raw[[col]] / as.numeric(extras[[nm]][["div"]])
  }
  out
}

#' Analyse a sprint-training session
#'
#' Runs the full sprint-training pipeline on one session's motion data: detects the
#' sprints, compares them across the workout, and decomposes their metabolic power
#' into alactic / lactic / aerobic contributions.
#'
#' @param motion_data A data frame with `time` (s) and `velocity` (m/s), e.g. from
#'   [read_gpexe_csv()].
#' @param maximal_aerobic_power Maximal aerobic power, MAP (W/kg). Default 20.
#' @param ... Passed to [detect_sprints()].
#'
#' @returns A list with `sprints` ([detect_sprints()] table), `workout`
#'   ([compare_workout_sprints()] table) and `bioenergetics`
#'   ([analyze_training_bioenergetics()] list), plus `maximal_aerobic_power`. The
#'   workout/bioenergetics entries are `NULL` if no sprint is detected.
#' @export
#'
#' @examples
#' \donttest{
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' res <- analyze_sprint_session(gpexe, maximal_aerobic_power = 20)
#' res$bioenergetics$summary
#' }
analyze_sprint_session <- function(motion_data, maximal_aerobic_power = 20, ...) {
  validate_motion_data(motion_data)
  check_positive(maximal_aerobic_power, "maximal_aerobic_power")
  sprints <- detect_sprints(motion_data, ...)
  if (nrow(sprints) == 0) {
    return(list(sprints = sprints, workout = NULL, bioenergetics = NULL,
                maximal_aerobic_power = maximal_aerobic_power))
  }
  workout <- compare_workout_sprints(motion_data, sprints = sprints)
  bioenergetics <- analyze_training_bioenergetics(
    motion_data, sprints = sprints, maximal_aerobic_power = maximal_aerobic_power)
  list(sprints = sprints, workout = workout, bioenergetics = bioenergetics,
       maximal_aerobic_power = maximal_aerobic_power)
}

#' Batch-analyse a set of gpexe session files
#'
#' Reads each gpexe CSV, runs [analyze_sprint_session()], and writes per-session
#' tables (CSV) and figures (PNG) under `output_dir/<label>/`, plus a `run_log.csv`
#' summarising the batch. Each file is processed independently; a file that errors
#' (or has no detectable sprint) is recorded in the log and does not stop the batch.
#'
#' @param files Character vector of gpexe `.csv`/`.csv.gz` paths.
#' @param output_dir Directory to write results to (created if needed).
#' @param maximal_aerobic_power MAP (W/kg); a scalar applied to all files, or a
#'   vector recycled to `length(files)` for per-session values.
#' @param labels Output sub-folder name per file (default: file name without extension).
#' @param width,height,dpi Figure size / resolution passed to [ggplot2::ggsave()].
#'
#' @returns (invisibly) a data frame run log with one row per file (`file`, `label`,
#'   `n_sprints`, `map`, `status`, `message`).
#' @export
#'
#' @examples
#' \donttest{
#' gpexe <- subset(ten_200_sprints_paired, source == "gpexe")
#' f <- tempfile(fileext = ".csv")
#' utils::write.table(data.frame("time (s)" = gpexe$time,
#'                               "speed (km/h)" = gpexe$velocity * 3.6,
#'                               check.names = FALSE),
#'                    f, sep = ";", dec = ".", row.names = FALSE)
#' batch_sprint_analysis(f, output_dir = tempfile("sls"), maximal_aerobic_power = 20)
#' }
batch_sprint_analysis <- function(files, output_dir, maximal_aerobic_power = 20,
                                  labels = NULL, width = 9, height = 5, dpi = 110) {
  if (length(files) == 0) stop("`files` is empty.", call. = FALSE)
  if (is.null(labels)) labels <- tools::file_path_sans_ext(basename(files))
  map <- rep_len(maximal_aerobic_power, length(files))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- lapply(seq_along(files), function(i) {
    tryCatch({
      md <- read_gpexe_csv(files[i])
      res <- analyze_sprint_session(md, maximal_aerobic_power = map[i])
      odir <- file.path(output_dir, labels[i])
      dir.create(odir, showWarnings = FALSE, recursive = TRUE)
      utils::write.csv(res$sprints, file.path(odir, "sprints.csv"), row.names = FALSE)
      if (!is.null(res$bioenergetics)) {
        utils::write.csv(res$bioenergetics$per_sprint,
                         file.path(odir, "bioenergetics.csv"), row.names = FALSE)
        utils::write.csv(res$bioenergetics$summary,
                         file.path(odir, "summary.csv"), row.names = FALSE)
        ggplot2::ggsave(file.path(odir, "speed_distance.png"),
                        plot_workout_sprints(md, sprints = res$sprints),
                        width = width, height = height, dpi = dpi)
        ggplot2::ggsave(file.path(odir, "energy.png"),
                        plot_workout_energy(md, sprints = res$sprints,
                                            maximal_aerobic_power = map[i]),
                        width = width, height = height + 1, dpi = dpi)
        ggplot2::ggsave(file.path(odir, "bioenergetics_sprint1.png"),
                        plot_sprint_bioenergetics(md, sprint_id = res$sprints$sprint_id[1],
                                                  sprints = res$sprints,
                                                  maximal_aerobic_power = map[i]),
                        width = width, height = height, dpi = dpi)
      }
      data.frame(file = files[i], label = labels[i], n_sprints = nrow(res$sprints),
                 map = map[i], status = "ok", message = "", stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(file = files[i], label = labels[i], n_sprints = NA_integer_,
                 map = map[i], status = "error", message = conditionMessage(e),
                 stringsAsFactors = FALSE)
    })
  })
  run_log <- do.call(rbind, rows)
  utils::write.csv(run_log, file.path(output_dir, "run_log.csv"), row.names = FALSE)
  invisible(run_log)
}
