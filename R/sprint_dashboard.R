#' Launch the sprint-training analysis dashboard
#'
#' Starts an interactive Shiny dashboard for coaches to explore the sprint
#' kinematics and bioenergetic decomposition of gpexe sessions stored in a folder.
#' Pick a session file and a maximal aerobic power (MAP) and the dashboard runs the
#' [analyze_sprint_session()] pipeline, showing the detected sprints, the
#' speed-vs-distance comparison, and the per-sprint alactic / lactic / aerobic
#' partition and cumulative energy.
#'
#' Requires the suggested packages \pkg{shiny}, \pkg{shinydashboard} and \pkg{DT}.
#'
#' @param data_dir Directory holding the gpexe `.csv`/`.csv.gz` session files to
#'   browse (searched recursively). Defaults to the working directory.
#' @param ... Passed to [shiny::runApp()] (e.g. `launch.browser`, `port`).
#'
#' @returns Invisibly `NULL`; called for the side effect of running the app.
#' @export
#'
#' @examples
#' \dontrun{
#' launch_sprint_dashboard("data-raw/SLS")
#' }
launch_sprint_dashboard <- function(data_dir = ".", ...) {
  data_dir <- normalizePath(data_dir, mustWork = TRUE)
  for (pkg in c("shiny", "shinydashboard", "DT")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for the dashboard. Install it with ",
           "install.packages('", pkg, "').", call. = FALSE)
    }
  }
  app_dir <- system.file("dashboard", package = "runrgetics")
  if (!nzchar(app_dir)) stop("Dashboard app not found in the installed package.", call. = FALSE)

  old <- Sys.getenv("RUNRGETICS_DASHBOARD_DATA", unset = NA)
  Sys.setenv(RUNRGETICS_DASHBOARD_DATA = data_dir)
  on.exit(if (is.na(old)) Sys.unsetenv("RUNRGETICS_DASHBOARD_DATA")
          else Sys.setenv(RUNRGETICS_DASHBOARD_DATA = old), add = TRUE)

  shiny::runApp(app_dir, ...)
  invisible(NULL)
}
