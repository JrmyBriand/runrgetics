utils::globalVariables(c("Optimal_Time", "Actual_Time"))

#' Sprint Simulation Race Time Error Function
#'
#' Error function that computes the distance traveled over a sprint of a given duration. A simulation is made
#' using the sprint bioenergetic models well as alactic and lactic capacities as well as maximal aerobic power.
#' The function can be used to find the running time corresponding to classical sprint running distances (e.g. 100 m, 200 m, etc.)
#'
#' @param time_performance A numeric value representing the running time performance of the sprint (in s). Typically, this performance running time corresponds to the difference between time performance and reaction time (in s).
#' @param alactic_capacity A numeric value representing the alactic capacity of the athlete (in J/kg).
#' @param lactic_capacity A numeric value representing the lactic capacity of the athlete (in J/kg).
#' @param maximal_aerobic_power A numeric value representing the maximal aerobic power of the athlete (in W/kg). Default is set at 24.5 W/kg.
#' @param dt A numeric value representing the time step for the simulation (in s). Default is set at 0.01 s.
#'
#' @returns A numeric value representing the distance traveled during the sprint (in m).
#' @export
#'
#' @examples
#'
#' # Take Bolt 100 m
#'
#' total_time <- 9.58
#' reaction_time <- 0.146
#'
#' performance_running_time <- total_time - reaction_time
#'
#' # use Briand et al. (2025) alactic and lactic capacities estimates for the simulation
#'
#' alactic_capacity <- 328
#' lactic_capacity <- 1460
#'
#' # apply the error function. We expect the output to be close to 100 m
#'
#' sprint_race_time_error_function(performance_running_time, alactic_capacity, lactic_capacity)
#'
#'
sprint_race_time_error_function <- function(time_performance, alactic_capacity, lactic_capacity, maximal_aerobic_power = 24.5, dt = 0.01) {

  # estimate max_al based on alactic capacity for all distances
  max_al <- find_max_al(time_performance, alactic_capacity)


  # estimate max_al based on alactic capacity for all distances
  max_la <- find_max_la(time_performance, lactic_capacity)

  time <- seq(dt, time_performance, by = dt)

  # Generate power series from the sprint model
  power_series <- sprint_bioenergetic_model(
    time = time,
    maximal_alactic_power = max_al,
    maximal_lactic_power = max_la,
    maximal_aerobic_power = maximal_aerobic_power
  )

  # Recover motion and calculate total distances
  distance<- sprint_recover_distance(power_series, dt)


  return(distance)
}





#' Sprint Time Performance Simulation
#'
#' Simulates the time performances on target distance given an athlete's estimated alactic and lactic capacities as well as maximal aerobic power.
#' The simulation is based on the sprint bioenergetic model. The returned results present the `optimal_time`, which corresponds to running time, as well as `actual_time`,
#' which corresponds to the running time plus reaction time. The reaction time can be provided as a function parameter. The simulation can be used to estimate the running time of a sprint on classical sprint distances (e.g. 100 m, 200 m, etc.).
#'
#' @param target_distances A numeric vector representing the target distances for the sprint simulation (in m).
#' @param alactic_capacity A numeric value representing the alactic capacity of the athlete (in J/kg).
#' @param lactic_capacity A numeric value representing the lactic capacity of the athlete (in J/kg).
#' @param maximal_aerobic_power A numeric value representing the maximal aerobic power of the athlete (in W/kg). Default is set at 24.5 W/kg.
#' @param dt A numeric value representing the time step for the simulation (in s). Default is set at 0.01 s.
#' @param reaction_time A numeric value representing the reaction time of the athlete (in s). Default is set at 0.15 s.
#'
#' @returns A tibble with the following columns:
#'  \itemize{
#'    \item Distance: The target distance for the sprint (in m).
#'    \item Optimal_Time: The running time for the sprint, without considering reaction time (in s).
#'    \item Actual_Time: The running time for the sprint, including reaction time (in s).
#'    }
#' @export
#'
#' @examples
#'
#' target_distances <- c(60, 100, 200, 300, 400)
#'  # Use Briand et al. (2025) alactic and lactic capacities estimates for the simulation
#'  alactic_capacity <- 328
#'  lactic_capacity <- 1460
#'
#' # apply the simulation
#'
#' sprint_time_perf_simulation(target_distances, alactic_capacity, lactic_capacity)
#'
sprint_time_perf_simulation <- function(target_distances, alactic_capacity, lactic_capacity, maximal_aerobic_power = 24.5, dt = 0.01, reaction_time = 0.15){

  # Helper function to find optimal time for a given distance
  find_running_time <- function(target_distance, alactic_capacity, lactic_capacity, maximal_aerobic_power, dt) {
    # Objective function: Minimize squared error between calculated and target distance
    objective_function <- function(time) {
      distance <- sprint_race_time_error_function(time, alactic_capacity, lactic_capacity, maximal_aerobic_power, dt)
      return((distance - target_distance)^2)
    }

    # Perform optimization
    result <- stats::optimize(
      f = objective_function,
      interval = c(1, 50), # Reasonable bounds for sprint times
      tol = 1e-4
    )

    return(result$minimum)
  }

  # Calculate optimal times for each target distance
  optimal_times <- sapply(target_distances, function(dist) {
    find_running_time(
      target_distance = dist,
      alactic_capacity = alactic_capacity,
      lactic_capacity = lactic_capacity,
      maximal_aerobic_power = maximal_aerobic_power,
      dt = dt
    )
  })

  # Add reaction time and create results dataframe
  actual_times <- optimal_times + reaction_time

  results <- tibble::tibble(
    Distance = target_distances,
    Optimal_Time = optimal_times,
    Actual_Time = actual_times
  )

  return(results)

}


#' Sprint Time Performance Simulation Formatted Table
#'
#' Provides a formatted table of simulated time performances on target distance given an athlete's estimated alactic and lactic capacities as well as maximal aerobic power.
#' The simulation is based on the sprint bioenergetic model. The returned results present the `Running Time`, which corresponds to running time in s, as well as `Performance Time`,
#' which corresponds to the running time plus reaction time (in s). The reaction time can be provided as a function parameter. The simulation can be used to estimate the running time of a sprint on classical sprint distances (e.g. 100 m, 200 m, etc.).
#'
#' @param target_distances A numeric vector representing the target distances for the sprint simulation (in m).
#' @param alactic_capacity A numeric value representing the alactic capacity of the athlete (in J/kg).
#' @param lactic_capacity A numeric value representing the lactic capacity of the athlete (in J/kg).
#' @param maximal_aerobic_power A numeric value representing the maximal aerobic power of the athlete (in W/kg). Default is set at 24.5 W/kg.
#' @param dt A numeric value representing the time step for the simulation (in s). Default is set at 0.01 s.
#' @param reaction_time A numeric value representing the reaction time of the athlete (in s). Default is set at 0.15 s.
#'
#' @returns A tinytable with the following columns:
#'  \itemize{
#'    \item Distance: The target distance for the sprint (in m).
#'    \item Running Time: The running time for the sprint, without considering reaction time (in s).
#'    \item Performance Time: The running time for the sprint, including reaction time (in s).
#'    }
#' @export
#'
#' @examples
#'
#' target_distances <- c(60, 100, 200, 300, 400)
#'  # Use Briand et al. (2025) alactic and lactic capacities estimates for the simulation
#'  alactic_capacity <- 328
#'  lactic_capacity <- 1460
#'
#' # apply the simulation
#'
#' sprint_time_perf_simulation_briand_table(target_distances, alactic_capacity, lactic_capacity)
sprint_time_perf_simulation_briand_table <- function(target_distances, alactic_capacity, lactic_capacity, maximal_aerobic_power = 24.5, dt = 0.01, reaction_time = 0.15){

  table <- sprint_time_perf_simulation(
    target_distances = target_distances,
    alactic_capacity = alactic_capacity,
    lactic_capacity = lactic_capacity,
    maximal_aerobic_power = maximal_aerobic_power,
    dt = dt,
    reaction_time = reaction_time
  )

  #round the time columns

  table <- table |>
    dplyr::mutate(
      Optimal_Time = round(Optimal_Time, 2),
      Actual_Time = round(Actual_Time, 2)
    )



  # rename columns

  # rename all the columns

  colnames(table) <- c(
    "Distance (m)",
    "Running time (s)",
    "Time Performance - including reaction time (s)"
  )

  return(tinytable::tt(table, notes = paste("Simulated sprint times for the hypothetical athlete. The simulation is
  based on an athlete with a maximal aerobic capacity of", round(maximal_aerobic_power, 2) , "W kg-1, a maximal alactic capacity of",  round(alactic_capacity, 2),"J kg-1 and lactic capacity of", round(lactic_capacity, 2), "J kg-1.", sep = " " ))
  )

}



