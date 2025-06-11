#' Sprint Lactic Energy and Power Over Running Duration
#'
#' Computes the lactic energy and average power over the duration of a sprint. The function is based on a provided sprint power data frame
#' containing instantaneous power contribution of the alactic, lactic and aerobic energy pathways. Such data frame can be derived using Briand et al.'s (2025)
#' sprint bioenergetic model
#'
#' @param sprint_power_data A tibble with a time column (time, in s) and instantaneous lactic power (power_lactic, in W/kg) over the sprint, as computed using the sprint bioenergetic model proposed by Briand et al. (2025).
#'
#' @returns A tibble with the following columns: duration (s), lactic_energy (J/kg), lactic_power (W/kg).
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
#'
#' # compute lactic energy vs duration for Bolt's 100 m performance
#'
#'  sprint_lactic_energy_duration(bolt_modeled_data)
#'
sprint_lactic_energy_duration <- function(sprint_power_data) {
  # extract duration (last time value of the sprint_power_data time colmunn)

  duration <- max(sprint_power_data$time)

  # compute lactic energy

  lactic_energy <- energy_lactic(sprint_power_data)

  # Compute average lactic power

  average_lactic_power <- lactic_energy / duration

  # return table with proper data

  result <- tibble::tibble(
    duration = duration,
    lactic_energy = lactic_energy,
    lactic_power = average_lactic_power
  )

  return(result)
}

#' Sprint Lactic Energy and Power Over Running Duration (Graubner and Nixdorf dataset)
#'
#' Computes the lactic energy and average power over the duration of a sprint for the Graubner and Nixdorf dataset.
#' The default data is the `graubner_nixdorf_sprints` dataset, which contains sprint performances of elite athletes.
#' The `athlete_sex` parameter enables to compute lactic energy and power over running duration for either male or female 100, 200 and 400 m events.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#' @param athlete_sex A character corresponding the athlete sex for the analysis. Default is "male" and computes energy and power over the Graubner and Nixdorf's (2011) men's 100, 200 and 400 m performances. It also sets the maximal aerobic power to 24.5 W/kg. Similarly, setting athlete_sex to "female" performs the computation on women's performances and sets maximal aerobic power to 21 W/kg.
#' @inheritParams sprint_alactic_energy_duration_graubner_nixdorf
#'
#' @returns A tibble with duration (s), lactic_energy (J/kg), lactic_power (W/kg).
#' @export
#'
#' @examples
#'
#' sprint_lactic_energy_duration_graubner_nixdorf()
#'
sprint_lactic_energy_duration_graubner_nixdorf <- function(data = graubner_nixdorf_sprints,
                                                           athlete_sex = "male",
                                                           mu = -0.4,
                                                           sigma = 1,
                                                           k1 = 2.75,
                                                           k2 = 35,
                                                           dt = 0.01,
                                                           cost_running_flat = 3.8,
                                                           slope_equation = "original") {
  if (athlete_sex == "male") {
    dat <- data |>
      dplyr::filter(event == "Men's 100 m" |
        event == "Men's 200 m" |
        event == "Men's 400 m")

    map <- 24.5
  }

  if (athlete_sex == "female") {
    dat <- data |>
      dplyr::filter(event == "Women's 100 m" |
        event == "Women's 200 m" |
        event == "Women's 400 m")

    map <- 21
  }

  # events
  events <- unique(dat$event)


  # initialize table

  table <- tibble::tibble(
    duration = numeric(),
    lactic_energy = numeric(),
    lactic_power = numeric(),
    athlete_sex = character()
  )

  for (i in events) {
    event_data <- dat |>
      dplyr::filter(event == i)


    sprint_data <- sprint_motion_model_data(
      mean_velocity_splits = event_data$velocity,
      time_splits = event_data$splits,
      distance = event_data$distance,
      reaction_time = event_data$reaction_time[1],
      maximal_velocity = event_data$maximal_velocity[1],
      dt = dt,
      cost_running_flat = cost_running_flat,
      slope_equation = slope_equation
    )


    sprint_power_data <- sprint_bioenergetic_model_data(sprint_data,
      mu = mu,
      sigma = sigma,
      k1 = k1,
      k2 = k2,
      maximal_aerobic_power = map
    )

    # compute lactic energy vs duration for each event

    sprint_lactic_energy_duration_data <- sprint_lactic_energy_duration(sprint_power_data) |>
      dplyr::mutate(athlete_sex = athlete_sex)

    # add to table

    table <- dplyr::bind_rows(table, sprint_lactic_energy_duration_data)
  }

  return(table)
}



#' Sprint Lactic Energy Duration Model
#'
#' Computes the lactic energy as a function of duration using a bi-exponential model.
#' Other than duration, the lactic capacity is the only parameter of the model. The lactic capacity corresponds to the maximal energy available through the lactic energy pathway.
#'
#' @param duration A numeric vector representing the duration of the sprint (in seconds).
#' @param lactic_capacity A numeric value representing the lactic capacity (in J/kg).
#' @inheritParams sprint_lactic_capacity
#'
#' @returns A numeric vector representing the average lactic energy (in J/kg) as a function of running duration.
#' @export
#'
#' @examples
#'
#' lactic_capacity <- 1350 # Example alactic capacity in J/kg
#' duration <- seq(0.1, 10, by = 0.1) # Example durations from 0.1 to 10 seconds
#'
#' lactic_energy <- sprint_lactic_duration_model(duration, lactic_capacity)
#' lactic_energy
#'
#'

sprint_lactic_duration_model <- function(duration, lactic_capacity, t1 = 20, t2 = 1500) {
  knorm <- bi_exponential_knorm(t1 = t1, t2 = t2)

  return(lactic_capacity * knorm * (1 - exp(-(duration - 3) / t1)) * exp(-(duration - 3) / t2))
}



#' Sprint Lactic Duration Model Fit
#'
#' Performs a non-linear least squares fitting of the lactic energy
#' model to the lactic energy over running duration data.
#'
#' @param lactic_energy_duration A tibble with the following columns: duration (s), lactic_energy (J/kg), corresponding to the average lactic energy expenditure over running duration. This data is used to fit the lactic energy model.
#' @inheritParams sprint_lactic_capacity
#'
#' @returns A fitted model object of class "nls" representing the lactic energy model fitted to the lactic energy duration data.
#' @export
#'
#' @examples
#' men_lactic_energy <- sprint_lactic_energy_duration_graubner_nixdorf()
#' men_lactic_energy
#'
#'  # Fit lactic model to energy
#'
#'  sprint_lactic_duration_model_fit(men_lactic_energy)
#'
sprint_lactic_duration_model_fit <- function(lactic_energy_duration, t1 = 20, t2 = 1500) {
  # Fit the alactic power model to the alactic power duration data
  fit <- minpack.lm::nlsLM(lactic_energy ~ sprint_lactic_duration_model(duration, lactic_capacity, t1 = t1, t2 = t2),
    data = lactic_energy_duration,
    start = list(lactic_capacity = 1300)
  )

  return(fit)
}


#' Sprint Lactic Duration Model Fit Residual Standard Error
#'
#' Performs a non-linear least squares fitting of the lactic energy model to the lactic energy over
#' running duration data and returns the residual standard error of the fit.
#'
#' @inheritParams sprint_lactic_duration_model_fit
#'
#' @returns A numeric value representing the residual standard error of the fitted lactic energy model.
#' @export
#'
#' @examples
#'
#' men_lactic_energy <- sprint_lactic_energy_duration_graubner_nixdorf()
#' men_lactic_energy
#'
#'  # Fit lactic model to energy
#'
#'  sprint_lactic_duration_model_fit_rse(men_lactic_energy)
#'
sprint_lactic_duration_model_fit_rse <- function(lactic_energy_duration, t1 = 20, t2 = 1500) {
  # Fit the lactic energy model to the lactic energy duration data
  fit <- sprint_lactic_duration_model_fit(lactic_energy_duration, t1 = t1, t2 = t2)

  return(summary(fit)$sigma)
}


#' Lactic Capacity from Lactic Energy Duration Data
#'
#' Extracts the lactic capacity from the fitted lactic energy model based on the lactic energy duration data.
#'
#' @inheritParams sprint_lactic_duration_model_fit
#'
#'
#' @returns A numeric value representing the lactic capacity (in J/kg) extracted from the fitted lactic energy model.
#' @export
#'
#' @examples
#' men_lactic_energy <- sprint_lactic_energy_duration_graubner_nixdorf()
#' men_lactic_energy
#'
#'  # Extract lactic capacity from the data
#'
#'  sprint_lactic_capacity(men_lactic_energy)
#'
sprint_lactic_capacity <- function(lactic_energy_duration, t1 = 20, t2 = 1500) {
  # Fit the lactic energy model to the lactic energy duration data
  fit <- sprint_lactic_duration_model_fit(lactic_energy_duration, t1 = t1, t2 = t2)

  # Extract the alactic capacity from the fit
  lactic_capacity <- coef(fit)["lactic_capacity"]

  return(lactic_capacity)
}




#' Find Sprint Maximal Lactic Power
#'
#' Knowing an athlete maximal lactic capacity, the function finds
#' the maximal lactic power that can be sustained over a given sprint duration
#'
#' @param duration A numeric value representing the sprint duration (in seconds).
#' @inheritParams sprint_lactic_duration_model
#' @inheritParams sprint_bioenergetic_model
#'
#' @returns A numeric value representing the maximal lactic power (in W/kg) that can be sustained over the given sprint duration.
#' @export
#'
#' @examples
#'
#' duration <- 20
#' lactic_capacity <- 1350
#'
#' find_max_la(duration, lactic_capacity)
#'
find_max_la <- function(duration, lactic_capacity, t1 = 20, t2 = 1500, k1 = 2.75, k2 = 35) {

  lactic_energy <- sprint_lactic_duration_model(duration, lactic_capacity, t1 = t1, t2 = t2)


  # Define the objective function for optimization
  objective_function <- function(max_la) {
    # Numerical integration of pgly over [0, time]
    integrated_energy <- stats::integrate(
      function(t) sprint_bioenergetic_model(t, maximal_alactic_power = 0, maximal_lactic_power = max_la, k1 = k1, k2 = k2, output = "lactic power" ),
      lower = 0, upper = duration,
      rel.tol = 1e-8
    )$value

    # Return the squared error
    (integrated_energy - lactic_energy)^2
  }

  # Optimize max_la to minimize the objective function
  result <- stats::optim(
    par = 1, # Initial guess for max_la
    fn = objective_function,
    method = "Brent",
    lower = 0, upper = 1000 # Define suitable bounds for max_la
  )

  # Return the optimized value of max_la
  return(result$par)
}

