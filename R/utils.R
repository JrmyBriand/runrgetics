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



#' Bi-exponential Function Normalisation Constant
#'
#' Calculates a normalization constant using the time constants of the rising and decaying exponential terms,
#' ensuring that the amplitude term preceding the exponential functions matches the maximum value achieved by the bi-exponential function.
#'
#' @param t1 a double representing the time constant of the rising exponential term (in s)
#' @param t2 a double representing the time constant of the decaying exponential term (in s)
#'
#' @returns a double representing the normalization constant
#' @export
#'
#' @examples
#'
#' bi_exponential_knorm(2, 40)
#'
bi_exponential_knorm <- function(t1, t2){

 return((t1+t2)/(t2*(t1/(t1+t2))^(t1/t2)) )
}
