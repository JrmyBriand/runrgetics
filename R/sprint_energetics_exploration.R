

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
#' men_100 <- graubner_nixdorf_sprints  |>
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
#'
sprint_motion_acceleration_data <- function(sprint_motion_data){

acceleration_limit <- which(sprint_motion_data$velocity == max(sprint_motion_data$velocity))

acceleration_data <- sprint_motion_data[1:acceleration_limit,]

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
#' men_100 <- graubner_nixdorf_sprints  |>
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
#'
sprint_motion_deceleration_data <- function(sprint_motion_data){

  acceleration_limit <- which(sprint_motion_data$velocity == max(sprint_motion_data$velocity))

  deceleration_data <- sprint_motion_data[(acceleration_limit + 1):nrow(sprint_motion_data),]

  return(deceleration_data)
}




#' Sprint Approximate Aerobic Power
#'
#' @param time A vector of time points (s)
#' @param k_aer A double. Time constant of the exponential rise to the maximal aerobic power (s). Default is 23 s.
#' @param maximal_aerobic_power A double. Maximal aerobic power (W/kg). Default is 24.5 W/kg corresponding to World class male sprinter observed MAP. The equivalent for female is 21 W/kg
#' @param basal_metabolic_rate A double. Basal metabolic rate (W/kg). Default is 1.2 W/kg
#'
#' @returns A vector of the approximate aerobic power (W/kg) at each time point
#' @export
#'
#' @examples
#'
#' sprint_approx_aerobic_power(time = seq(0, 10, by = 0.1))
#'
#'
sprint_approx_aerobic_power <- function(time, k_aer = 23, maximal_aerobic_power = 24.5, basel_metabolic_rate = 1.2){

  return((MAP-BMR) * (1 - exp(-time/k_aer)))

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
sprint_acceleration_approx_lactic_power <- function(time, maximal_lactic_power, k_an = 2){


  return(maximal_lactic_power * (1 - exp(-time/k_an)))

}



sprint_acceleration_approx_contributions <- function(){

}

sprint_deceleration_approx_contributions <- function(){

}

sprint_approx_contributions <- function(){

}

sprint_approx_metabolic_energies <- function(){

}

sprint_approx_lactic_power <- function(){

}

sprint_approx_alactic_power <- function(){

}
