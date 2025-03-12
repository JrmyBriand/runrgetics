#' Find Times Associated With Velocity Measurements
#'
#' In the cases where data are provided as time splits, with reaction time and average velocity
#' for each time intervals, it can be assumed that the instantaneous velocity measurement occurs at the
#' middle point of each interval. The function returns a vector of times where instantaenous velocities occur.
#'
#' @param splits A vector containing time splits (in s)
#' @param reaction_time A double corresponding to the reaction time (in s)
#'
#' @returns A vector of times (in s) where instantaneous velocity measurements occur
#' @export
#'
#' @examples
#' find_time_velocity(c(0, 1.88, 2.88, 3.78, 4.64, 5.47, 6.29, 7.10, 7.92, 8.74, 9.58), 0.173)
#'
find_time_velocity <- function(splits, reaction_time) {
  time_velocity <- c(reaction_time, sapply(
    1:(length(splits) - 1),
    function(i) middle_point(splits[i], splits[i + 1])
  ))


  return(time_velocity)
}


#' Find Terminal Velocity
#'
#' Estimates the velocity at the end of the race. Based on splits data measured at different intervals
#' and the average velocity over each interval.
#'
#' @param splits A vector containing time splits (in s)
#' @param velocity A vector containing average velocity over each interval (in m/s)
#' @param distance A vector containing intermediate distances at which splits are measured (in m)
#' @param reaction_time A double corresponding to the reaction time (in s)
#'
#' @returns A double representing the terminal velocity (in m/s)
#' @export
#'
#' @examples
#' find_terminal_velocity(c(8.74, 9.58), c(12.17, 11.96), c(90, 100), 0.146)
#'
find_terminal_velocity <- function(splits, velocity, distance, reaction_time) {
  time_velocity <- find_time_velocity(splits, reaction_time)

  # final race time
  t_final <- splits[length(splits)]

  # calculate distance intervals

  distance_intervals <- distance[length(distance)] - distance[length(distance) - 1]

  # calculate terminal velocity

  v_terminal <- distance_intervals / (t_final - time_velocity[length(time_velocity)]) - velocity[length(velocity)]

  return(v_terminal)
}


#' Instantaneous Velocity During Running Acceleration
#'
#' Estimates the instantaneous velocity during the acceleration phase of a sprint run. If the reaction time is known,
#' it can be included in the model. The model is based on an exponential rise in velocity.
#'
#' @param time A vector of time points (in s)
#' @param tau A double representing the time constant of the exponential rise in velocity (in s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param reaction_time A double representing the reaction time (in s) Default value is 0
#'
#' @returns A vector of instantaneous velocities (in m/s)
#' @export
#'
#' @examples
#' acc_velocity_model(1:10, 0.5, 10)
#' acc_velocity_model(1:10, 0.5, 10, reaction_time = 0.1)
acc_velocity_model <- function(time, tau, maximal_velocity, reaction_time = 0) {
  maximal_velocity * (1 - exp(-(time - reaction_time) / tau))
}



#' Instantaneous Distance During Running Acceleration
#'
#' Estimates the instantaneous distance during the acceleration phase of a sprint run. The model is based on an exponential rise in velocity.
#'
#'
#' @param time A vector of time points (in s)
#' @param tau A double representing the time constant of the exponential rise in velocity (in s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#'
#' @returns A vector of instantaneous distances (in m)
#' @export
#'
#' @examples
#' acc_distance_model(1:10, 0.5, 10)
acc_distance_model <- function(time, tau, maximal_velocity) {
  maximal_velocity * (time - tau * (1 - exp(-time / tau)))
}



#' Instantaneous Velocity During Sprint Running Deceleration Phase
#'
#' Estimates the instantaneous velocity during the deceleration phase of a sprint run. The model is based on a constant rate of deceleration.
#'
#' @param time A vector of time points (in s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param time_maximal_velocity A double representing the time at which maximal velocity is reached (in s)
#' @param decel_rate A double representing the rate of deceleration (in m/s^2)
#'
#' @returns A vector of instantaneous velocities (in m/s)
#' @export
#'
#' @examples
#' dec_velocity_model(6:10, 10, 6, 0.1)
dec_velocity_model <- function(time, maximal_velocity, time_maximal_velocity, decel_rate) {
  maximal_velocity - decel_rate * (time - time_maximal_velocity)
}



#' Instantaneous Distance During Sprint Running Decceleration Phase
#'
#' Estimates the instantaneous distance during the deceleration phase of a sprint run. The model is based on a constant rate of deceleration.
#'
#' @param time A vector of time points (in s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param time_maximal_velocity A double representing the time at which maximal velocity is reached (in s)
#' @param distance_maximal_velocity A double representing the distance at which maximal velocity is reached (in m)
#' @param decel_rate A double representing the rate of deceleration (in m/s^2)
#'
#' @returns A vector of instantaneous distances (in m)
#' @export
#'
#' @examples
#' dec_distance_model(6:10, 10, 6, 60, 0.1)
dec_distance_model <- function(time, maximal_velocity, time_maximal_velocity, distance_maximal_velocity, decel_rate) {
  v_decel <- dec_velocity_model(time, maximal_velocity, time_maximal_velocity, decel_rate)

  dist <- (maximal_velocity + v_decel) * (time - time_maximal_velocity) / 2 + distance_maximal_velocity

  return(dist)
}



#' Instantaneous Velocity During Sprint Running (Acceleration and Deceleration phases)
#'
#'
#' Estimates the instantaneous velocity during the acceleration and deceleration phases of a sprint run.
#' The calculation is based on an exponential rise in velocity during the acceleration phase and a constant rate of deceleration during the deceleration phase.
#'
#' @param time A vector of time points (in s)
#' @param time_maximal_velocity A double representing the time at which maximal velocity is reached (in s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param tau A double representing the time constant of the exponential rise in velocity (in s)
#' @param fitted_maximal_velocity A double representing the maximal velocity fitted using the model for instantaneous velocity in acceleration phase (in m/s)
#' @param decel_rate A double representing the rate of deceleration (in m/s^2)
#'
#' @returns A vector of instantaneous velocities (in m/s)
#' @export
#'
#' @examples
#' velocity_sprint_model(1:10, 5, 10, 1.5, 10, 0.1)
#'
velocity_sprint_model <- function(time, time_maximal_velocity, maximal_velocity, tau, fitted_maximal_velocity, decel_rate) {
  # Create a vector of the same length as time
  result <- numeric(length(time))

  # Apply acceleration model to times <= time_maximal_velocity
  acc_indices <- which(time <= time_maximal_velocity)
  if (length(acc_indices) > 0) {
    result[acc_indices] <- acc_velocity_model(time[acc_indices], tau, maximal_velocity, reaction_time = 0)
  }

  # Apply deceleration model to times > time_maximal_velocity
  decel_indices <- which(time > time_maximal_velocity)
  if (length(decel_indices) > 0) {
    result[decel_indices] <- dec_velocity_model(time[decel_indices], fitted_maximal_velocity, time_maximal_velocity, decel_rate)
  }

  return(result)
}


#' Instantaneous Acceleration During Sprint Running (Acceleration and Deceleration phases)
#'
#' Estimates the instantaneous acceleration during the acceleration and deceleration phases of a sprint run.
#' The calculation is based on an exponential rise in velocity during the acceleration phase and a constant rate of deceleration during the deceleration phase.
#'
#' @param time A vector of time points (in s)
#' @param time_maximal_velocity A double representing the time at which maximal velocity is reached (in s)
#' @param maximal_velocity A double representing the maximal velocity fitted using the model for instantaneous velocity in acceleration phase (in m/s)
#' @param tau A double representing the time constant of the exponential rise in velocity (in s)
#' @param decel_rate A double representing the rate of deceleration (in m/s^2)
#'
#' @returns A vector of instantaneous accelerations (in m/s^2)
#' @export
#'
#' @examples
#' acceleration_sprint_model(1:10, 5, 10, 1.5, 0.1)
#'
acceleration_sprint_model <- function(time, time_maximal_velocity, maximal_velocity, tau, decel_rate) {
  # Create a vector of the same length as time
  result <- numeric(length(time))

  # Calculate acceleration for times <= time_maximal_velocity
  acc_indices <- which(time <= time_maximal_velocity)
  if (length(acc_indices) > 0) {
    result[acc_indices] <- (maximal_velocity - maximal_velocity *
      (1 - exp(-(time[acc_indices]) / tau))) / tau
  }

  # Apply constant deceleration for times > time_maximal_velocity
  decel_indices <- which(time > time_maximal_velocity)
  if (length(decel_indices) > 0) {
    result[decel_indices] <- -decel_rate
  }

  return(result)
}


#' Instantaneous Distance During Sprint Running (Acceleration and Deceleration phases)
#'
#' @param time A vector of time points (in s)
#' @param time_maximal_velocity A double representing the time at which maximal velocity is reached (in s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param distance_maximal_velocity  A double representing the distance at which maximal velocity is reached (in m)
#' @param fitted_maximal_velocity A double representing the maximal velocity fitted using the model for instantaneous velocity in acceleration phase (in m/s)
#' @param tau tau A double representing the time constant of the exponential rise in velocity (in s)
#' @param decel_rate A double representing the rate of deceleration (in m/s^2)
#'
#' @returns A vector of instantaneous distances (in m)
#' @export
#'
#' @examples
#' distance_sprint_model(1:10, 5, 10, 50, 10, 1.5, 0.1)
#'
distance_sprint_model <- function(time, time_maximal_velocity, maximal_velocity, distance_maximal_velocity, fitted_maximal_velocity, tau, decel_rate) {
  # Create a vector of the same length as time
  result <- numeric(length(time))

  # Apply acceleration distance model to times <= time_maximal_velocity
  acc_indices <- which(time <= time_maximal_velocity)
  if (length(acc_indices) > 0) {
    result[acc_indices] <- acc_distance_model(time[acc_indices], tau, maximal_velocity)
  }

  # Apply deceleration distance model to times > time_maximal_velocity
  decel_indices <- which(time > time_maximal_velocity)
  if (length(decel_indices) > 0) {
    result[decel_indices] <- dec_distance_model(
      time[decel_indices], fitted_maximal_velocity,
      time_maximal_velocity, distance_maximal_velocity, decel_rate
    )
  }

  return(result)
}





#' Average Velocity From Splits
#'
#'
#' Computes average velocity over time intervals based on recorded time splits and their
#' associated distances.
#'
#' @param splits a vector of time splits (in s)
#' @param distances a vector of distances (in m)
#'
#' @returns a vector of average velocities (in m/s)
#' @export
#'
#' @examples average_velocity_from_splits(c(0, 1.8), c(0, 10))
average_velocity_from_splits <- function(splits, distances) {
  # Calculate time intervals
  time_intervals <- diff(splits)

  # Calculate distance intervals
  distance_intervals <- diff(distances)


  velocities <- c(0,distance_intervals / time_intervals)

  return(velocities)
}




#' Finding Acceleration Exponential Rise Time-Constant (tau)
#'
#' Computes the time constant of the exponential rise in velocity during the acceleration phase of a sprint run
#' using vectors of assumed instantaneous velocities and times throughout the sprint acceleration.
#'
#' @param time A vector of time points (in s)
#' @param velocity A vector of velocities (in s)
#' @param reaction_time A double representing the reaction time (in s)
#'
#' @returns A double representing the time constant of the exponential rise in velocity (in s)
#' @importFrom minpack.lm nlsLM
#' @importFrom stats coef
#' @export
#'
#' @examples find_tau(1:10, 1:10, 0.1)
find_tau <- function(time, velocity, reaction_time){

  mod_velocity <- minpack.lm::nlsLM(
    velocity ~ acc_velocity_model(time, tau, vf, reaction_time = reaction_time),
    start = list(tau = 0.5, vf = 12),
    data = data.frame(time = time, velocity = velocity)
  )

  # extract value of tau
  tau <- stats::coef(mod_velocity)[1]

  return(tau)

}


#' Predicted Sprint Maximal Velocity
#'
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param tau A double representing the time constant of the exponential rise in velocity (in s)
#' @param time_maximal_velocity A double representing the time at which the acceleration phase ends (in s)
#'
#' @returns A double representing the predicted maximal velocity (in m/s)
#' @export
#'
#' @examples predicted_maximal_velocity(10, 1.5, 5)
predicted_maximal_velocity <- function(maximal_velocity, tau, time_maximal_velocity){
  predicted_maximal_velocity <- acc_velocity_model(time_maximal_velocity, tau, maximal_velocity, reaction_time = 0)

  return(predicted_maximal_velocity)
}

