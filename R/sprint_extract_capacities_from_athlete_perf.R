#' Calculate Optimal Alactic and Lactic Capacities from Sprint Times
#'
#' Computes the optimal alactic and lactic capacities based on
#' sprint performances across different distances. It uses an optimization approach
#' to find the capacities that best explain the observed performances.
#'
#' @param performance_data A tibble containing at least two columns:
#'   \itemize{
#'     \item distance: The race distance in meters
#'     \item performance: The race time in seconds
#'   }
#' @param maximal_aerobic_power Numeric value for maximum aerobic power (default = 24.5)
#' @param dt Time step for numerical integration (default = 0.01)
#' @param init_alactic Initial guess for alactic capacity (default = 350)
#' @param init_lactic Initial guess for lactic capacity (default = 1000)
#' @param lower_bounds Numeric vector of length 2 specifying lower bounds for
#'   alactic and lactic capacities respectively (default = c(200, 1000))
#' @param upper_bounds Numeric vector of length 2 specifying upper bounds for
#'   alactic and lactic capacities respectively (default = c(400, 2000))
#'
#' @return A named list containing:
#'   \itemize{
#'     \item alactic_capacity: Optimal alactic capacity
#'     \item lactic_capacity: Optimal lactic capacity
#'     \item convergence: Optimization convergence status
#'     \item error: Final error value
#'   }
#'
#' @examples
#' performance_data <- tibble::tibble(
#'   distance = c(100, 200, 400),
#'   performance = c(9.43, 19.1, 43.9)
#' )
#' #sprint_get_capacities_from_perf(performance_data)
#'
#' @export
sprint_get_capacities_from_perf <- function(performance_data,
                                 maximal_aerobic_power = 24.5,
                                 dt = 0.01,
                                 init_alactic = 350,
                                 init_lactic = 1000,
                                 lower_bounds = c(200, 1000),
                                 upper_bounds = c(400, 2000)) {

  # Input validation
  if (!all(c("distance", "performance") %in% colnames(performance_data))) {
    stop("performance_data must contain 'distance' and 'performance' columns")
  }

  # Prepare optimized times (subtracting reaction time)
  times_opt <- performance_data$performance
  distances <- performance_data$distance

  # Define the error calculation function
  calculate_error <- function(alactic_capacity, lactic_capacity) {
    total_error <- 0

    for (i in seq_along(distances)) {
      # Calculate maximum alactic and lactic power for current distance
      max_al <- find_max_al(duration = times_opt[i], alactic_capacity =  alactic_capacity)
      max_la <- find_max_la(duration = times_opt[i], lactic_capacity = lactic_capacity)

      # Generate power series
      power_series <- sprint_bioenergetic_model(
        time =  seq(0, times_opt[i], by = dt),
        maximal_alactic_power = max_al,
        maximal_lactic_power = max_la,
        maximal_aerobic_power = maximal_aerobic_power
      )

      # Calculate distance
      distance_achieved <- sprint_recover_distance(power_series, dt = dt)

      # Add percentage error
      total_error <- total_error +
        abs(distance_achieved - distances[i]) / distances[i] * 100
    }

    return(total_error)
  }

  # Optimization function
  optimize_capacities <- function(params) {
    alactic_capacity <- params[1]
    lactic_capacity <- params[2]
    return(calculate_error(alactic_capacity, lactic_capacity))
  }

  # Perform optimization
  result <- stats::optim(
    par = c(init_alactic, init_lactic),
    fn = optimize_capacities,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds
  )

  # Return results
  return(list(
    alactic_capacity = result$par[1],
    lactic_capacity = result$par[2],
    convergence = result$convergence,
    error = result$value
  ))
}


#' Sprint Simulation Metrics
#'
#' Provides metrics synthesizing a sprint simulation of a given time performance for a set running distance.
#' The simulation is based on an athlete's alactic and lactic capacities, as well as maximal aerobic power.
#' The returned metrics include the estimated distance traveled, the error with respect to the actual distance, total energy expenditure,
#' as well as the energy expenditure from the three metabolic pathways.
#'
#' @param time_performance A numeric value representing the time performance of the sprint (in s).
#' @param distance A numeric value representing the distance of the sprint (in m).
#' @param alactic_capacity A numeric value representing the alactic capacity of the athlete (in J/kg). Default is set at 328 J/kg.
#' @param lactic_capacity A numeric value representing the lactic capacity of the athlete (in J/kg). Default is set at 1460 J/kg.
#' @param maximal_aerobic_power A numeric value representing the maximal aerobic power of the athlete (in W/kg). Default is set at 24.5 W/kg.
#' @param dt A numeric value representing the time step for the simulation (in s). Default is set at 0.01 s.
#'
#' @returns A tibble containing the following columns:
#'   \itemize{
#'   \item time_performance: The time performance of the sprint (in s).
#'   \item distance: The actual distance of the sprint (in m).
#'   \item distance_estimated: The estimated distance traveled during the sprint (in m).
#'   \item distance_error: The percentage error between the estimated and actual distance traveled during the sprint.
#'   \item total_energy: The total energy expenditure during the sprint (in J/kg).
#'   \item alactic_energy: The energy expenditure from the alactic pathway (in J/kg).
#'   \item lactic_energy: The energy expenditure from the lactic pathway (in J/kg).
#'   \item aerobic_energy: The energy expenditure from the aerobic pathway (in J/kg).
#'   }
#' @export
#'
#' @examples
#'
#' # Let's take Bolt 100 m performance
#'
#' time_bolt <- 9.58
#' reaction_time <- 0.146
#'
#' running_time_performance <- time_bolt - reaction_time
#'
#' distance <- 100
#'
#' # extract the metrics
#'
#' sprint_simulation_metrics(running_time_performance, distance)
#'
#'
sprint_simulation_metrics <- function(time_performance, distance, alactic_capacity = 328, lactic_capacity = 1460,
                                              maximal_aerobic_power = 24.5, dt = 0.01){

  # find max_al and max_la

  max_al <- find_max_al(duration = time_performance, alactic_capacity = alactic_capacity)

  max_la <- find_max_la(duration = time_performance, lactic_capacity = lactic_capacity)

  # generate power series

  time <- seq(dt, time_performance, by = dt)

  power_series <- tibble::tibble(
    time = time,
    power_mod = sprint_bioenergetic_model(
      time = time,
      maximal_alactic_power = max_al,
      maximal_lactic_power = max_la,
      maximal_aerobic_power = maximal_aerobic_power,
      output = "total power"
    ),
    power_alactic = sprint_bioenergetic_model(
      time = time,
      maximal_alactic_power = max_al,
      maximal_lactic_power = max_la,
      maximal_aerobic_power = maximal_aerobic_power,
      output = "alactic power"
    ),
    power_lactic = sprint_bioenergetic_model(
      time = time,
      maximal_alactic_power = max_al,
      maximal_lactic_power = max_la,
      maximal_aerobic_power = maximal_aerobic_power,
      output = "lactic power"
    ),
    power_aerobic = sprint_bioenergetic_model(
      time = time,
      maximal_alactic_power = max_al,
      maximal_lactic_power = max_la,
      maximal_aerobic_power = maximal_aerobic_power,
      output = "aerobic power"
    )

  )

  # compute distance

  distance_estimated <- sprint_recover_distance(power_series$power_mod, dt = dt)
  distance_error <- (distance_estimated - distance) / distance * 100

  # energy expenditure

  total_energy <- energy_total(power_series, type = "power bioenergetic model")

  alactic_energy <- energy_alactic(power_series)

  lactic_energy <- energy_lactic(power_series)

  aerobic_energy <- energy_aerobic(power_series)


  # create a tibble with results

  results <- tibble::tibble(
    time_performance = round(time_performance,2),
    distance = round(distance, 2),
    distance_estimated = round(distance_estimated, 2),
    distance_error = round(distance_error, 2),
    total_energy = round(total_energy),
    alactic_energy = round(alactic_energy),
    lactic_energy = round(lactic_energy),
    aerobic_energy = round(aerobic_energy)
  )

  return(results)

}


#' Sprint Simulation Metrics Table (as in Briand et al. (2025) article)
#'
#' Provides a table of metrics synthesizing a sprint simulation of fictitious male and female athletes able to
#' realise respectively the men's and women's 100, 200 and 400 performances from the `graubner_nixdorf_sprints` dataset.
#' The simulation is based on an athlete's alactic and lactic capacities, as well as maximal aerobic power.
#' The returned metrics include the estimated distance traveled, the error with respect to the actual distance, total energy expenditure,
#' as well as the energy expenditure from the three metabolic pathways for each sprint event. The table provide similar information as the one presented in
#' Briand et al. (2025) article presenting the sprint bioenergetic model.
#'
#' @param male_alactic_capacity A numeric value representing the male athlete's alactic capacity (in J/kg). Default is set at 328 J/kg.
#' @param male_lactic_capacity A numeric value representing the male athlete's lactic capacity (in J/kg). Default is set at 1460 J/kg.
#' @param female_alactic_capacity A numeric value representing the female athlete's lactic capacity (in J/kg). Default is set at 218 J/kg.
#' @param female_lactic_capacity A numeric value representing the female athlete's lactic capacity (in J/kg). Default is set at 1295 J/kg.
#' @param male_maximal_aerobic_power A numeric value representing the male athlete's maximal aerobic power (in W/kg). Default is set at 24.5 W/kg.
#' @param female_maximal_aerobic_power A numeric value representing the female athlete's maximal aerobic power (in W/kg). Default is set at 21 W/kg.
#' @param dt A numeric value representing the time step for the simulation (in s). Default is set at 0.01 s.
#'
#' @returns A tinytable object containing the following columns:
#' \itemize{
#'   \item Event: The name of the event.
#'   \item Time Performance: The time performance of the sprint (in s).
#'   \item Actual Distance: The actual distance of the sprint (in m).
#'   \item Estimated Distance: The estimated distance traveled during the sprint (in m).
#'   \item Distance Error: The percentage error between the estimated and actual distance traveled during the sprint.
#'   \item Total Energy: The total energy expenditure during the sprint (in J/kg).
#'   \item Alactic Energy: The energy expenditure from the alactic pathway (in J/kg).
#'   \item Lactic Energy: The energy expenditure from the lactic pathway (in J/kg).
#'   \item Aerobic Energy: The energy expenditure from the aerobic pathway (in J/kg).
#'   }
#'
#' @export
#'
#' @examples
#' sprint_simulation_metrics_briand_article()
sprint_simulation_metrics_briand_article <- function( male_alactic_capacity = 328, male_lactic_capacity = 1460,
                                                      female_alactic_capacity = 218, female_lactic_capacity = 1295,
                                                     male_maximal_aerobic_power = 24.5,
                                                     female_maximal_aerobic_power = 21,
                                                     dt = 0.01){

  #initalize performance table

  perf_table <- tibble::tibble(
    event = character(),
    time_performance = numeric(),
    distance = numeric(),
    distance_estimated = numeric(),
    distance_error = numeric(),
    total_energy = numeric(),
    alactic_energy = numeric(),
    lactic_energy = numeric(),
    aerobic_energy = numeric()
  )


  # get performance table

  perf_data <- graubner_nixdorf_perf_from_splits()

  for(i in perf_data$event){

    sub_data <- perf_data |>
      dplyr::filter(event == i)

    sex <- unique(sub_data$sex)

    if(sex == "male"){
      alactic_capacity <- male_alactic_capacity
      lactic_capacity <- male_lactic_capacity
      maximal_aerobic_power <- male_maximal_aerobic_power
    }
    if(sex == "female"){
      alactic_capacity <- female_alactic_capacity
      lactic_capacity <- female_lactic_capacity
      maximal_aerobic_power <- female_maximal_aerobic_power
    }

    time_performance <- sub_data$performance[1]
    distance <- sub_data$distance[1]

    # compute metrics
    metrics <- sprint_simulation_metrics(
      time_performance = time_performance,
      distance = distance,
      alactic_capacity = alactic_capacity,
      lactic_capacity = lactic_capacity,
      maximal_aerobic_power = maximal_aerobic_power,
      dt = dt
    )

    # add event name as first element
    metrics <- metrics |>
      dplyr::mutate(event = i) |>
      dplyr::select(event, everything())

    # append to performance table

    perf_table <- dplyr::bind_rows(perf_table, metrics)

  }

  # rename all the columns

  colnames(perf_table) <- c(
    "Event",
    "Time Performance (s)",
    "Actual Distance (m)",
    "Estimated Distance (m)",
    "Distance Error (%)",
    "Total Energy (J/kg)",
    "Alactic Energy (J/kg)",
    "Lactic Energy (J/kg)",
    "Aerobic Eneergy (J/kg)"
  )

  return(tinytable::tt(perf_table, notes = "Distance traveled and energy contribution of the 3 metabolic pathways over 100, 200, and 400 m for hypothetical male and female athletes"))

}

