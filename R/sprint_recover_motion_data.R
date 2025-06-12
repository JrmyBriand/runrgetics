#' Sprint Recover Motion Data
#'
#' From a power time series, the function invert di Prampero et al.'s (2005, 2018) equivalent slope approach to recover acceleration, velocity and distace.
#'
#' @param power_series a vector of power values (W/kg) corresponding to instantaneous power output during a sprint.
#' @param dt a numeric value representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#' @inheritParams cost_running
#'
#' @returns A tibble containing time, distance, velocity, acceleration, and power.
#' @export
#'
#' @examples
#'
#' # Extract Bolt's 100 m data from Graubner and Nixdorf data set.
#' bolt_100m <- graubner_nixdorf_sprints |>
#'   dplyr::filter(
#'     athlete == "Bolt",
#'     event == "Men's 100 m"
#'   )
#'
#' # Compute sprint motion data
#'
#' bolt_100m_motion_data <- sprint_motion_model_data(
#'   mean_velocity_splits = bolt_100m$velocity,
#'   time_splits = bolt_100m$splits,
#'   distance = bolt_100m$distance,
#'   reaction_time = bolt_100m$reaction_time[1],
#'   maximal_velocity = bolt_100m$maximal_velocity[1]
#' )
#'
#'
#' # Recover motion data from sprint power series
#'
#' recovered_motion_data <- sprint_recover_motion(bolt_100m_motion_data$power, dt = 0.01)
#' head(recovered_motion_data)
#'
sprint_recover_motion <- function(power_series, dt = 0.01, cost_running_flat = 3.6, slope_equation = "original") {
  n <- length(power_series)
  velocity <- numeric(n)
  acceleration <- numeric(n)
  distance <- numeric(n)

  # Initial conditions
  velocity[1] <- 0
  acceleration[1] <- 0
  distance[1] <- 0

  # Iterative solution
  for (i in 2:n) {
    # Function to minimize (difference between observed and calculated power)
    objective <- function(delta_v) {
      # Calculate acceleration from velocity change
      acc <- delta_v / dt
      # Calculate new velocity
      vel <- velocity[i - 1] + delta_v
      # Calculate power
      calc_power <- cost_running_sprint(acceleration = acc, velocity = vel, cost_running_flat = cost_running_flat, slope_equation = slope_equation) * vel
      return((calc_power - power_series[i])^2)
    }

    # Find delta_v that minimizes the difference
    result <- stats::optimize(objective,
      interval = c(-0.01, 0.08),
      tol = 1e-4
    )

    # Update velocity and acceleration
    velocity[i] <- velocity[i - 1] + result$minimum
    acceleration[i] <- result$minimum / dt

    # Update distance (trapezoidal integration)
    distance[i] <- distance[i - 1] + (velocity[i] + velocity[i - 1]) / 2 * dt
  }

  # compute time

  time <- seq(0, (n - 1) * dt, by = dt)

  # create a tibble with time, distance, velocity, acceleration, and power

  motion_data <- tibble::tibble(
    time = time,
    distance = distance,
    velocity = velocity,
    acceleration = acceleration,
    power = cost_running_sprint(acceleration, velocity, cost_running_flat = cost_running_flat, slope_equation = slope_equation) * velocity
  )

  # Return both the motion parameters and the final distance
  return(motion_data)
}


#' Recover Sprint Distance
#'
#' @param power_series a vector of power values (W/kg) corresponding to instantaneous power output during a sprint.
#' @param dt a numeric value representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#' @inheritParams cost_running
#'
#' @returns A numeric value representing the total distance covered during the sprint.
#' @export
#'
#' @examples
#' # Extract Bolt's 100 m data from Graubner and Nixdorf data set.
#' bolt_100m <- graubner_nixdorf_sprints |>
#'   dplyr::filter(
#'     athlete == "Bolt",
#'     event == "Men's 100 m"
#'   )
#'
#' # Compute sprint motion data
#'
#' bolt_100m_motion_data <- sprint_motion_model_data(
#'   mean_velocity_splits = bolt_100m$velocity,
#'   time_splits = bolt_100m$splits,
#'   distance = bolt_100m$distance,
#'   reaction_time = bolt_100m$reaction_time[1],
#'   maximal_velocity = bolt_100m$maximal_velocity[1]
#' )
#'
#'
#' # compute distance from sprint motion power series
#'
#' recovered_distance <- sprint_recover_distance(bolt_100m_motion_data$power, dt = 0.01)
#'
sprint_recover_distance <- function(power_series, dt = 0.01, cost_running_flat = 3.6, slope_equation = "original") {
  motion_data <- sprint_recover_motion(power_series, dt = dt, cost_running_flat = cost_running_flat, slope_equation = slope_equation)

  # integrate velocity to get distance

  distance <- pracma::trapz(motion_data$time, motion_data$velocity)

  return(distance)
}


#' Sprint Recovered Distance Percentage Error
#'
#' Compares the distance recovered from the sprint power data using di Prampero et al.'s (2005, 2018) equivalent slope approach and Briand et al.'s (2025) sprint bioenergetic model.
#'
#' @param sprint_power_data A tibble with at leats following columns: time (s), power (W/kg), corresponding to the power estimated by di Prampero et al.'s (2005, 2018) equivalent slope approach, power_mod (W/kg), corresponding to power estimated using Briand et al.'s (2025) sprint bioenergetic model.
#' @param dt a numeric value representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#' @inheritParams cost_running
#'
#' @returns A numeric value representing the percentage difference in distance recovered between the two methods.
#' @export
#'
#' @examples
#' # Extract Bolt's 100 m data from Graubner and Nixdorf data set.
#' bolt_100m <- graubner_nixdorf_sprints |>
#'   dplyr::filter(
#'     athlete == "Bolt",
#'     event == "Men's 100 m"
#'   )
#'
#' # Compute sprint motion data
#'
#' bolt_100m_motion_data <- sprint_motion_model_data(
#'   mean_velocity_splits = bolt_100m$velocity,
#'   time_splits = bolt_100m$splits,
#'   distance = bolt_100m$distance,
#'   reaction_time = bolt_100m$reaction_time[1],
#'   maximal_velocity = bolt_100m$maximal_velocity[1]
#' )
#'
#'
#' # compute the power estimated from the sprint bioenergetic model
#'
#' bolt_modeled_data <- sprint_bioenergetic_model_data(bolt_100m_motion_data)
#'
#' percent_error <- sprint_modeled_distance_percentage_error(bolt_modeled_data)
#'
#' round(percent_error, 2)
#'
sprint_modeled_distance_percentage_error <- function(sprint_power_data, dt = 0.01, cost_running_flat = 3.6, slope_equation = "original") {
  distance_recovered <- sprint_recover_distance(sprint_power_data$power, dt = dt, cost_running_flat = cost_running_flat, slope_equation = slope_equation)

  distance_recovered_bioenergetic_model <- sprint_recover_distance(sprint_power_data$power_mod, dt = dt, cost_running_flat = cost_running_flat, slope_equation = slope_equation)

  modeled_distance_percentage_diff <- (distance_recovered_bioenergetic_model - distance_recovered) / distance_recovered * 100

  return(modeled_distance_percentage_diff)
}
