


#' Sprint motion model data (motion metrics, cost of running and metbolic power)
#'
#' Compute the velocity, acceleration, distance, cost of running
#' and power for a sprint event. Based on available time splits and velocity data
#' provided in the graubner_nixdorf_sprints data set.
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#'
#' @returns A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power
#' @importFrom tibble tibble
#' @export
#'
#' @examples sprint_motion_model_data(mean_velocity_splits = c(0, 5.77, 9.99),
#'                             time_splits = c(0, 1.88, 2.88),
#'                             distance = c(0, 10, 20),
#'                             reaction_time = 0.146,
#'                             maximal_velocity = 12.34)
#'
#'
sprint_motion_model_data <- function(mean_velocity_splits, time_splits, distance, reaction_time , maximal_velocity = NA, dt = 0.01){

  # 1. Times associated with velocities

  times_velocity <- find_time_velocity(time_splits, reaction_time)

  # 2. Calculate terminal velocity

  v_terminal <- find_terminal_velocity(time_splits, mean_velocity_splits, distance, reaction_time)

  # 3. Calculate maximal velocity

  if(is.na(maximal_velocity)){
    v_max <- max(mean_velocity_splits)
  } else {
    v_max <- maximal_velocity
  }

  # 4. Calculate tau

  tau <- find_tau(times_velocity, mean_velocity_splits, reaction_time)

  # 5. Calculate the time at the end of the acceleration

  time <- times_velocity - reaction_time

  time_maximal_velocity <- time[which.max(mean_velocity_splits)]

  # 6. Calculate predicted maximal velocity

  predicted_maximal_velocity <- predicted_maximal_velocity(v_max, tau, time_maximal_velocity)

  # 7. Calculate deceleration rate

  race_time <- time_splits[length(time_splits)]

  decel_rate <- (predicted_maximal_velocity - v_terminal) / ((race_time - reaction_time) - time_maximal_velocity)

  # 8. define a time sequence

  time_seq <- seq(0, race_time - reaction_time, by = dt)

  # 9. calculate the velocity at each time point

  modeled_velocity <- sprint_velocity_model(time_seq,
                                            time_maximal_velocity = time_maximal_velocity,
                                            maximal_velocity = v_max,
                                            tau = tau,
                                            fitted_maximal_velocity = predicted_maximal_velocity,
                                            decel_rate = decel_rate)

  # 10. Calculate the acceleration at each time point

  modeled_acceleration <- sprint_acceleration_model(time_seq,
                                                    time_maximal_velocity = time_maximal_velocity,
                                                    tau = tau,
                                                   maximal_velocity = v_max,
                                                    decel_rate = decel_rate)

  # 10.5 Calculate distance at wichi maximal velocity is attained

  dist_max_velocity <- sprint_acc_distance_model(time_maximal_velocity, tau, v_max)


  # 11. Calculate the distance at each time point

  modeled_distance <- sprint_distance_model(time_seq,
                                            time_maximal_velocity = time_maximal_velocity,
                                            maximal_velocity = v_max,
                                            distance_maximal_velocity = dist_max_velocity,
                                            tau = tau,
                                            fitted_maximal_velocity = predicted_maximal_velocity,
                                            decel_rate = decel_rate)

  # 12. Calculate the cost of running at each time point

  sprint_data <- tibble::tibble(time = time_seq,
                                      distance = modeled_distance,
                                      velocity = modeled_velocity,
                                      acceleration = modeled_acceleration)

  sprint_data$cost_running <- cost_running_sprint(sprint_data$acceleration,
                                                        sprint_data$velocity)

  # 13. Calculate the power at each time point

  sprint_data$power <- sprint_data$cost_running *  sprint_data$velocity

  # 14. Return a data frame with the following columns: time, velocity, acceleration, distance, cost of running and power

  return(sprint_data)

}


#' Maximum Matebolic Power
#'
#' Computes the maximal metabolic power generated during a sprint event.
#'
#' @param sprint_motion_data A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power
#'
#' @returns A double with the maximal metabolic power (W/kg)
#' @export
#'
#' @examples
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
#' sprint_maximum_metabolic_power(sprint_data)
#'
sprint_maximum_metabolic_power <- function(sprint_motion_data){
  max(sprint_motion_data$power)
}

