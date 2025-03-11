#' Middle Point
#'
#' Calculates the midpoint time within a given time interval.
#'
#' @param t1 a double representing the time of the first boundary of the interval (in s)
#' @param t2 a double representing the time of the second boundary of the interval (in s)
#'
#' @returns a double representing the midpoint time (in s)
#' @export
#'
#' @examples
#' middle_point(2, 4)
#'
middle_point <- function(t1, t2) {
  return((t1 + t2) / 2)
}
