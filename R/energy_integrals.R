


#' Total Energy Expenditure
#'
#' Computes the integral of the total metabolic power over time to obtain total energy expenditure.
#'
#' @param data A tibble with a time series of power values and associated times. The tibble must minimally have the following columns: time (s), power (W/kg)
#'
#' @returns A double with the total energy expended (J/kg)
#'
#' @importFrom pracma trapz
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'  time = c(0, 1, 2, 3, 4),
#'  power = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_total(data)
#'
#'
#'
energy_total <- function(data) {
  # Ensure the data are sorted by time
  data <- data %>% arrange(time)

  # Use the trapezoidal rule to calculate the integral of power over time
  integral <- pracma::trapz(data$time, data$power)

  return(integral)
}

#' Aerobic Energy Expenditure
#'
#' Computes the integral of the aerobic power over time to obtain aerobic energy expenditure.
#'
#' @param data A tibble with a time series of aerobic power values and associated times. The tibble must minimally have the following columns: time (s), power_aerobic (W/kg)
#'
#' @returns A double corresponding to aerobic energy expended (J/kg)
#'
#' @importFrom pracma trapz
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'  time = c(0, 1, 2, 3, 4),
#'  power_aerobic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_aerobic(data)
#'
energy_aerobic <- function(data) {
  # Ensure the data are sorted by time
  data <- data %>% arrange(time)

  # Use the trapezoidal rule to calculate the integral of power over time
  integral <- pracma::trapz(data$time, data$power_aerobic)

  return(integral)
}

#' Anaerobic Energy Expenditure
#'
#' Computes the integral of the anaerobic power over time to obtain anaerobic energy expenditure.
#'
#' @param data A tibble with a time series of anaerobic power values and associated times. The tibble must minimally have the following columns: time (s), power_anaerobic (W/kg)
#'
#' @returns A double corresponding to anaerobic energy expended (J/kg)
#'
#' @importFrom pracma trapz
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'  time = c(0, 1, 2, 3, 4),
#'  power_anaerobic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_anaerobic(data)
#'
energy_anaerobic <- function(data) {
  # Ensure the data are sorted by time
  data <- data %>% arrange(time)

  # Use the trapezoidal rule to calculate the integral of power over time
  integral <- pracma::trapz(data$time, data$power_anaerobic)

  return(integral)
}


#' Anaerobic Lactic Energy Expenditure
#'
#' Computes the integral of the anaerobic lactic power over time to obtain anaerobic lactic energy expenditure.
#'
#' @param data A tibble with a time series of lactic power values and associated times. The tibble must minimally have the following columns: time (s), power_lactic (W/kg)
#'
#' @returns A double corresponding to lactic energy expended (J/kg)
#'
#' @importFrom pracma trapz
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'  time = c(0, 1, 2, 3, 4),
#'  power_lactic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_lactic(data)
#'
energy_lactic <- function(data) {
  # Ensure the data are sorted by time
  data <- data %>% arrange(time)

  # Use the trapezoidal rule to calculate the integral of power over time
  integral <- pracma::trapz(data$time, data$power_lactic)

  return(integral)
}


#' Anaerobic alactic Energy Expenditure
#'
#' Computes the integral of the anaerobic alactic power over time to obtain anaerobic alactic energy expenditure.
#'
#' @param data A tibble with a time series of alactic power values and associated times. The tibble must minimally have the following columns: time (s), power_alactic (W/kg)
#'
#' @returns A double corresponding to alactic energy expended (J/kg)
#'
#' @importFrom pracma trapz
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'  time = c(0, 1, 2, 3, 4),
#'  power_alactic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_alactic(data)
#'
energy_alactic <- function(data) {
  # Ensure the data are sorted by time
  data <- data %>% arrange(time)

  # Use the trapezoidal rule to calculate the integral of power over time
  integral <- pracma::trapz(data$time, data$power_alactic)

  return(integral)
}





