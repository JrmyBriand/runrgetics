#' Total Energy Expenditure
#'
#' Computes the integral of the total metabolic power over time to obtain total energy expenditure. The total energy expenditure estimated the sprint bioenergetic model
#' can also be computed by entering an extra column (power_mod) corresponding to the power derived from the sprint bioenergetic model.The `type` parameter has to be adjusted accordingly.
#'
#' @param data A tibble with a time series of power values and associated times. The tibble must minimally have the following columns: time (s), power (W/kg)
#' @param type A string indicating the type of power to integrate. Default is "power" and refers to estimated metabolic power derived from di Prampero et al. equivalent slope approach.
#' "power bioenergetic model" refers to the power estimated using the sprint bioenergetic model.
#'
#' @returns A double with the total energy expended (J/kg)
#'
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   time = c(0, 1, 2, 3, 4),
#'   power = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_total(data)
#'
energy_total <- function(data, type = "power") {
  if (type == "power") {
    # Ensure the data are sorted by time
    data <- data |> arrange(time)

    # Use the trapezoidal rule to calculate the integral of power over time
    integral <- pracma::trapz(data$time, data$power)
  }
  if (type == "power bioenergetic model") {
    # Ensure the data are sorted by time
    data <- data |> arrange(time)

    # Use the trapezoidal rule to calculate the integral of power over time
    integral <- pracma::trapz(data$time, data$power_mod)
  }

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
#'   time = c(0, 1, 2, 3, 4),
#'   power_aerobic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_aerobic(data)
#'
energy_aerobic <- function(data) {
  # Ensure the data are sorted by time
  data <- data |> arrange(time)

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
#'   time = c(0, 1, 2, 3, 4),
#'   power_anaerobic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_anaerobic(data)
#'
energy_anaerobic <- function(data) {
  # Ensure the data are sorted by time
  data <- data |> arrange(time)

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
#'   time = c(0, 1, 2, 3, 4),
#'   power_lactic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_lactic(data)
#'
energy_lactic <- function(data) {
  # Ensure the data are sorted by time
  data <- data |> arrange(time)

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
#'   time = c(0, 1, 2, 3, 4),
#'   power_alactic = c(0, 1, 2, 1, 0)
#' )
#'
#' energy_alactic(data)
#'
energy_alactic <- function(data) {
  # Ensure the data are sorted by time
  data <- data |> arrange(time)

  # Use the trapezoidal rule to calculate the integral of power over time
  integral <- pracma::trapz(data$time, data$power_alactic)

  return(integral)
}


#' Alactic Energy Percentage
#'
#' Computes the percentage of the total energy expenditure that is attributed to anaerobic alactic energy pathway. The alactic energy expenditure percentage with respect to total energy expenditure estimated the sprint bioenergetic model
#' can also be computed by entering an extra column (power_mod) corresponding to the power derived from the sprint bioenergetic model.The `type` parameter has to be adjusted accordingly.
#'
#'
#' @param data A tibble with a time series of alactic power values and associated times. The tibble must minimally have the following columns: time (s), power (W/kg), power_alactic (W/kg)
#' @param type A string indicating the type of power to integrate. Default is "power" and refers to estimated metabolic power derived from di Prampero et al. equivalent slope approach. "power model" refers to the power estimated using the sprint bioenergetic model.
#'
#' @returns A double representing the percentage (%) of anaerobic alactic energy expenditure relative to total energy expenditure.
#' @export
#'
#'
#' @examples
#'
#' data <- tibble::tibble(
#'   time = c(0, 1, 2, 3, 4),
#'   power = c(0, 1, 2, 1, 0),
#'   power_alactic = c(0, 0.5, 1, 0.5, 0)
#' )
#'
#' alactic_energy_percentage(data)
#'
alactic_energy_percentage <- function(data, type = "power") {
  # Calculate the total energy expenditure
  total_energy <- energy_total(data, type = type)

  # Calculate the anaerobic alactic energy expenditure
  alactic_energy <- energy_alactic(data)

  # Calculate the percentage of anaerobic alactic energy
  percentage <- (alactic_energy / total_energy) * 100

  return(percentage)
}

#' Lactic Energy Percentage
#'
#' Computes the percentage of the total energy expenditure that is attributed to anaerobic lactic energy pathway. The lactic energy expenditure percentage with respect to total energy expenditure estimated the sprint bioenergetic model
#' can also be computed by entering an extra column (power_mod) corresponding to the power derived from the sprint bioenergetic model.The `type` parameter has to be adjusted accordingly.
#'
#' @param data A tibble with a time series of alactic power values and associated times. The tibble must minimally have the following columns: time (s), power (W/kg), power_lactic (W/kg)
#' @param type A string indicating the type of power to integrate. Default is "power" and refers to estimated metabolic power derived from di Prampero et al. equivalent slope approach. "power model" refers to the power estimated using the sprint bioenergetic model.
#'
#' @returns A double representing the percentage (%) of anaerobic lactic energy expenditure relative to total energy expenditure.
#' @export
#'
#'
#' @examples
#'
#' data <- tibble::tibble(
#'   time = c(0, 1, 2, 3, 4),
#'   power = c(0, 1, 2, 1, 0),
#'   power_lactic = c(0, 0.5, 1, 0.5, 0)
#' )
#'
#' lactic_energy_percentage(data)
#'
lactic_energy_percentage <- function(data, type = "power") {
  # Calculate the total energy expenditure
  total_energy <- energy_total(data, type = type)

  # Calculate the anaerobic lactic energy expenditure
  lactic_energy <- energy_lactic(data)

  # Calculate the percentage of anaerobic lactic energy
  percentage <- (lactic_energy / total_energy) * 100

  return(percentage)
}

#' Anaerobic Energy Percentage
#'
#' Computes the percentage of the total energy expenditure that is attributed to anaerobic energy pathways. The anaerobic energy expenditure percentage with respect to total energy expenditure estimated the sprint bioenergetic model
#' can also be computed by entering an extra column (power_mod) corresponding to the power derived from the sprint bioenergetic model.The `type` parameter has to be adjusted accordingly.
#'
#' @param data A tibble with a time series of anaerobic power values and associated times. The tibble must minimally have the following columns: time (s), power (W/kg), power_anaerobic (W/kg)
#' @param type A string indicating the type of power to integrate. Default is "power" and refers to estimated metabolic power derived from di Prampero et al. equivalent slope approach. "power model" refers to the power estimated using the sprint bioenergetic model.
#'
#' @returns A double representing the percentage (%) of anaerobic energy expenditure relative to total energy expenditure.
#' @export
#'
#'
#' @examples
#'
#' data <- tibble::tibble(
#'   time = c(0, 1, 2, 3, 4),
#'   power = c(0, 1, 2, 1, 0),
#'   power_anaerobic = c(0, 0.5, 1, 0.5, 0)
#' )
#'
#' anaerobic_energy_percentage(data)
#'
anaerobic_energy_percentage <- function(data, type = "power") {
  # Calculate the total energy expenditure
  total_energy <- energy_total(data, type = type)

  # Calculate the anaerobic energy expenditure
  anaerobic_energy <- energy_anaerobic(data)

  # Calculate the percentage of anaerobic energy
  percentage <- (anaerobic_energy / total_energy) * 100

  return(percentage)
}

#' Aerobic Energy Percentage
#'
#' Computes the percentage of the total energy expenditure that is attributed to aerobic energy pathway. The aerobic energy expenditure percentage with respect to total energy expenditure estimated the sprint bioenergetic model
#' can also be computed by entering an extra column (power_mod) corresponding to the power derived from the sprint bioenergetic model.The `type` parameter has to be adjusted accordingly.
#'
#' @param data A tibble with a time series of aerobic power values and associated times. The tibble must minimally have the following columns: time (s), power (W/kg), power_aerobic (W/kg)
#' @param type A string indicating the type of power to integrate. Default is "power" and refers to estimated metabolic power derived from di Prampero et al. equivalent slope approach. "power model" refers to the power estimated using the sprint bioenergetic model.
#'
#' @returns A double representing the percentage (%) of anaerobic aerobic energy expenditure relative to total energy expenditure.
#' @export
#'
#'
#' @examples
#'
#' data <- tibble::tibble(
#'   time = c(0, 1, 2, 3, 4),
#'   power = c(0, 1, 2, 1, 0),
#'   power_aerobic = c(0, 0.5, 1, 0.5, 0)
#' )
#'
#' aerobic_energy_percentage(data)
#'
aerobic_energy_percentage <- function(data, type = "power") {
  # Calculate the total energy expenditure
  total_energy <- energy_total(data, type = type)

  # Calculate the aerobic energy expenditure
  aerobic_energy <- energy_aerobic(data)

  # Calculate the percentage of aerobic energy
  percentage <- (aerobic_energy / total_energy) * 100

  return(percentage)
}
