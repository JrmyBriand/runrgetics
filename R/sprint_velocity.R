

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
#' find_time_velocity( c(0, 1.88, 2.88, 3.78, 4.64, 5.47, 6.29, 7.10, 7.92, 8.74, 9.58), 0.173)
#'
find_time_velocity <- function(splits, reaction_time){

  time_velocity <- c(reaction_time,sapply(1:(length(splits)-1),
                            function(i) middle_point(splits[i], splits[i+1])))


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
#' find_terminal_velocity(c(8.74, 9.58), c( 12.17, 11.96), c(90, 100), 0.146)
#'
find_terminal_velocity <- function(splits, velocity, distance, reaction_time){

  time_velocity <- find_time_velocity(splits, reaction_time)

  #final race time
  t_final <- splits[length(splits)]

  # calculate distance intervals

  distance_intervals <- distance[length(distance)] - distance[length(distance)-1]

  # calculate terminal velocity

  v_terminal <- distance_intervals/(t_final -time_velocity[length(time_velocity)]) - velocity[length(velocity)]

  return(v_terminal)

}




