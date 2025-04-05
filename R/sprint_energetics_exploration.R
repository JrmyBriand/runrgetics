#' @importFrom utils globalVariables
utils::globalVariables(c("power_aerobic", "power_anaerobic", "power_lactic", "power_alactic"))

#' Sprint Motion Acceleration Data
#'
#' Extracts data from a data frame composed of sprint motion data corresponding
#' to the sprint acceleration phase. The sprint motion data can be computed with the function
#' \code{\link{sprint_motion_model_data}}
#'
#' @param sprint_motion_data A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power
#'
#' @returns A tibble, corresponding to the sprint motion acceleration data, with the following columns: time, velocity, acceleration, distance, cost of running and power
#' @export
#'
#' @examples
#'
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints |>
#'   dplyr::filter(event == "Men's 100 m")
#'
#'
#' # Get the sprint motion data for both men and women
#'
#' sprint_data <- sprint_motion_model_data(
#'   mean_velocity_splits = men_100$velocity,
#'   time_splits = men_100$splits,
#'   distance = men_100$distance,
#'   reaction_time = men_100$reaction_time[1],
#'   maximal_velocity = men_100$maximal_velocity[1]
#' )
#'
#' sprint_motion_acceleration_data(sprint_data)
#'
sprint_motion_acceleration_data <- function(sprint_motion_data) {
  acceleration_limit <- which(sprint_motion_data$velocity == max(sprint_motion_data$velocity))

  acceleration_data <- sprint_motion_data[1:acceleration_limit, ]

  return(acceleration_data)
}

#' Sprint Motion Acceleration Data
#'
#' Extracts data from a data frame composed of sprint motion data corresponding
#' to the sprint acceleration phase. The sprint motion data can be computed with the function
#' \code{\link{sprint_motion_model_data}}.
#'
#' @param sprint_motion_data A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power
#'
#' @returns A tibble, corresponding to the sprint motion deceleration data, with the following columns: time, velocity, acceleration, distance, cost of running and power
#' @export
#'
#' @examples
#'
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints |>
#'   dplyr::filter(event == "Men's 100 m")
#'
#'
#' # Get the sprint motion data for both men and women
#'
#' sprint_data <- sprint_motion_model_data(
#'   mean_velocity_splits = men_100$velocity,
#'   time_splits = men_100$splits,
#'   distance = men_100$distance,
#'   reaction_time = men_100$reaction_time[1],
#'   maximal_velocity = men_100$maximal_velocity[1]
#' )
#'
#' sprint_motion_deceleration_data(sprint_data)
#'
sprint_motion_deceleration_data <- function(sprint_motion_data) {
  # compute aerobic and anaerobic power

  sprint_motion_data_mod <- sprint_motion_data |>
    mutate(
      power_aerobic = sprint_approx_aerobic_power(time = time),
      power_anaerobic = power - power_aerobic
    )

  acceleration_limit <- which(sprint_motion_data$velocity == max(sprint_motion_data$velocity))

  deceleration_data <- sprint_motion_data[(acceleration_limit + 1):nrow(sprint_motion_data), ]

  return(deceleration_data)
}




#' Sprint Approximate Aerobic Power
#'
#' @param time A vector of time points (s)
#' @param k_aer A double. Time constant of the exponential rise to the maximal aerobic power (s). Default is 23 s.
#' @param maximal_aerobic_power A double. Maximal aerobic power or MAP (W/kg). Default is 24.5 W/kg corresponding to World class male sprinter observed MAP. The equivalent for female is 21 W/kg
#' @param basal_metabolic_rate A double. Basal metabolic rate (W/kg). Default is 1.2 W/kg
#'
#' @returns A vector of the approximate aerobic power (W/kg) at each time point
#' @export
#'
#' @examples
#'
#' sprint_approx_aerobic_power(time = seq(0, 10, by = 0.1))
#'
sprint_approx_aerobic_power <- function(time, k_aer = 23, maximal_aerobic_power = 24.5, basal_metabolic_rate = 1.2) {
  return((maximal_aerobic_power - basal_metabolic_rate) * (1 - exp(-time / k_aer)))
}


#' Sprint Approximate Anaerobic Lactic Power (Acceleration)
#'
#' @param time A vector of time points (s)
#' @param maximal_lactic_power A double corresponding to the maximal anaerobic lactic power (W/kg).
#' @param k_an A double corresponding to th time constant (s) of the exponential rise to the approximate maximal lactic power. Default is 2 s.
#'
#' @returns A vector of the approximate anaerobic lactic power (W/kg) at each time point
#' @export
#'
#' @examples
#'
#' sprint_acceleration_approx_lactic_power(time = seq(0, 10, by = 0.1), maximal_lactic_power = 10)
#'
sprint_acceleration_approx_lactic_power <- function(time, maximal_lactic_power, k_an = 2) {
  return(maximal_lactic_power * (1 - exp(-time / k_an)))
}



#' Sprint Approximate Power Distributions
#'
#' Computes approximate aerobic, lactic and alactic power distributions from sprint motion data.
#'
#' @param sprint_motion_data A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power
#' @param maximal_aerobic_power A double. Maximal aerobic power or MAP (W/kg). Default is 24.5 W/kg corresponding to World class male sprinter observed MAP. The equivalent for female is 21 W/kg
#' @param basal_metabolic_rate A double. Basal metabolic rate (W/kg). Default is 1.2 W/kg
#'
#' @returns A tibble with the following columns: time, velocity, acceleration, distance, cost of running, power, power_aerobic, power_anaerobic, power_lactic, power_alactic
#'
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom stats lm
#' @importFrom stats predict
#'
#'
#' @export
#'
#'
#' @examples
#' #'
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints |>
#'   dplyr::filter(event == "Men's 100 m")
#'
#'
#' # Get the sprint motion data for both men and women
#'
#' sprint_data <- sprint_motion_model_data(
#'   mean_velocity_splits = men_100$velocity,
#'   time_splits = men_100$splits,
#'   distance = men_100$distance,
#'   reaction_time = men_100$reaction_time[1],
#'   maximal_velocity = men_100$maximal_velocity[1]
#' )
#'
#' sprint_approx_power_distributions(sprint_data,
#'   maximal_aerobic_power = 24.5,
#'   basal_metabolic_rate = 1.2
#' )
#'
sprint_approx_power_distributions <- function(sprint_motion_data, maximal_aerobic_power = 24.5, basal_metabolic_rate = 1.2) {
  # calculate aerobic and anaerobic power

  sprint_motion_data_mod <- sprint_motion_data |>
    mutate(
      power_aerobic = sprint_approx_aerobic_power(time = time, maximal_aerobic_power = maximal_aerobic_power, basal_metabolic_rate = basal_metabolic_rate),
      power_anaerobic = power - power_aerobic
    )


  # extract acceleration data
  acceleration_data <- sprint_motion_acceleration_data(sprint_motion_data_mod)

  # extract deceleration data
  deceleration_data <- sprint_motion_deceleration_data(sprint_motion_data_mod)

  # estimate the maximal lactic power as the anaerobic power at the end of the acceleration

  maximal_lactic_power <- acceleration_data$power_anaerobic[nrow(acceleration_data)]

  # compute the aerobic power alactic and lactic contribution in the acceleration

  acceleration_data <- acceleration_data |>
    dplyr::mutate(
      power_lactic = sprint_acceleration_approx_lactic_power(time = time, maximal_lactic_power = maximal_lactic_power),
      power_alactic = power_anaerobic - power_lactic,
    )

  # Construct a linear model for the alactic power

  power_alactic_end_acceleration <- acceleration_data$power_alactic[nrow(acceleration_data)]
  power_alactic_end_sprint <- 0
  alactic_powers <- c(power_alactic_end_acceleration, power_alactic_end_sprint)
  times <- c(deceleration_data$time[1], deceleration_data$time[nrow(deceleration_data)])

  alactic_model <- stats::lm(alactic_powers ~ times)

  # Compute the alactic power at each time point

  deceleration_data <- deceleration_data |>
    dplyr::mutate(
      power_alactic = stats::predict(alactic_model, newdata = tibble::tibble(times = time)),
      power_lactic = power_anaerobic - power_alactic
    )

  # invert power alactic and power_lactic columns in deceleration data

  deceleration_data <- deceleration_data |>
    dplyr::relocate(power_lactic, .before = power_alactic)

  # Bind acceleration and deceleration data together

  sprint_energetics_data <- dplyr::bind_rows(acceleration_data, deceleration_data)

  return(sprint_energetics_data)
}


sprint_approx_lactic_power <- function() {

}

sprint_approx_alactic_power <- function() {

}
