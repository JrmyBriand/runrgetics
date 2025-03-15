

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


# simple approaximate aerobic model

sprint_approx_aerobic_power <- function(){


}

# simple approximate anaerobic lactic model

sprint_acceleration_approx_lactic_power <- function(){


}


# Computation of the approximate contributions

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
