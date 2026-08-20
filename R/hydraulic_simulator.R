#' Launch the hydraulic simulator
#'
#' Starts an interactive Shiny app implementing di Prampero's five-reservoir
#' hydraulic model of muscular energetics. Size each reservoir on the figure,
#' drive it with an exercise, and read how much energy each pathway supplied.
#'
#' The model is the one in Chapter I-6, Fig. I-6.1 of di Prampero, P. E. (2015),
#' \emph{La locomozione umana su terra, in acqua, in aria. Fatti e teorie}, 2nd
#' ed., Milano: Edi\if{latex}{\out{$\cdot$}}\if{html}{\out{&middot;}}Ermes,
#' ISBN 978-88-7051-476-6. P is the alactic store (ATP + PCr), Ox the aerobic
#' source, OS the O2 stores, Gl the glycolytic source and eLa early lactate;
#' the tap S sets the demand and the tubes Z, Z1 and A carry the supplies.
#'
#' Two configurations are supplied: the chapter's own reference athlete, and a
#' set adjusted to follow the case figures more closely. A tutorial tab walks
#' through the model and the app for readers meeting either for the first time.
#'
#' Note that di Prampero describes the model as a qualitative account of
#' exercise energetics that cannot be pushed to rigorous quantitative analysis,
#' because the kinetics of the individual mechanisms come out distorted. Read
#' the outputs accordingly.
#'
#' Requires the suggested packages \pkg{shiny}, \pkg{bslib} and \pkg{bsicons}.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `launch.browser`, `port`).
#'
#' @returns Invisibly `NULL`; called for the side effect of running the app.
#' @export
#'
#' @examples
#' \dontrun{
#' launch_hydraulic_simulator()
#' }
launch_hydraulic_simulator <- function(...) {
  for (pkg in c("shiny", "bslib", "bsicons")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for the hydraulic simulator. ",
           "Install it with install.packages('", pkg, "').", call. = FALSE)
    }
  }
  app_dir <- system.file("hydraulic_app2", package = "runrgetics")
  if (!nzchar(app_dir))
    stop("Hydraulic simulator app not found in the installed package.",
         call. = FALSE)

  shiny::runApp(app_dir, ...)
  invisible(NULL)
}
