#' Graubner and Nixdorf sprint data
#'
#' Time splits over distances recorded at the 2009 World athletics Championships in Berlin.
#'
#' @format ## `graubner_nixdorf_sprints`
#' A tibble with 45 rows and 6 columns:
#' \describe{
#'   \item{distance}{Distance (in m) at which time splits were measured for each event}
#'   \item{splits}{Time splits (in s)}
#'   \item{velocity}{Average velocity (m/s) over each distance interval}
#'   \item{reaction_time}{Reaction time (s) measured on the starting blocks at the beginning of the race}
#'   \item{maximal_velocity}{Maximal velocity (m/s) measured during the race. If not available, NA}
#'   \item{athlete}{Name of the athlete}
#'   \item{event}{Race event at the World athletics championships}
#' }
#' @source Graubner, R., & Nixdorf, E. (2009). Biomechanical analysis of the sprint and hurdles events at the 2009 IAAF World Championships in Athletics. Positions, 1(10).
"graubner_nixdorf_sprints"


#' Kindermann lactate data
#'
#' Provides lactate and accumulated lactate values over running durations
#'
#' @format ## `kindermann_lactate`
#' A tibble with 10 rows and 3 columns:
#' \describe{
#'   \item{duration}{Duration (in s) of the running event over which lactate was measured}
#'   \item{lactate}{Blood lactate concentration (in mmol/L)}
#'   \item{accumulated_lactate}{Accumulated lactate above blood lactate resting levels, which are assumed to be 1 mmol/L}
#' }
#' @source Kindermann, W. (1977). Lactate acidosis with different forms of sports activities. Can. J. Appl. Sports Sci. https://cir.nii.ac.jp/crid/1574231873976904704

"kindermann_lactate"
