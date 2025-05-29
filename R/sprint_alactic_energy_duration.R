#' Sprint Alactic Energy and Power Over Running Duration
#'
#' Computes the alactic energy and average power over the duration of a sprint. The function is based on a provided sprint power data frame
#' containing instantaneous power contribution of tha alactic, lactic and aerobic energy pathways. Such data frame can be derived using Briand et al.'s (2025)
#' sprint bioenergetic model
#'
#' @param sprint_power_data A tibble with a time column (time, in s) and instantaneous alactic power (power_alactic, in W/kg) over the sprint, as computed using the sprint bioenergetic model proposed by Briand et al. (2025).
#'
#' @returns A tibble with the following columns: duration (s), alactic_energy (J/kg), alactic_power (W/kg).
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
#' # compute alactic energy vs duration for Bolt's 100 m performance
#'
#'  sprint_alactic_energy_duration(bolt_modeled_data)

sprint_alactic_energy_duration <- function(sprint_power_data) {
  # extract duration (last time value of the sprint_power_data time colmunn)

  duration <- max(sprint_power_data$time)

  # compute lactic energy

  alactic_energy <- energy_alactic(sprint_power_data)

  # Compute average lactic power

  average_alactic_power <- alactic_energy / duration

  # return table with proper data

  result <- tibble::tibble(
    duration = duration,
    alactic_energy = alactic_energy,
    alactic_power = average_alactic_power
  )

  return(result)
}



#' Sprint Alactic Energy and Power Over Running Duration (Graubner and Nixdorf dataset)
#'
#' Computes the alactic energy and average power over the duration of a sprint for the Graubner and Nixdorf dataset.
#' The default data is the `graubner_nixdorf_sprints` dataset, which contains sprint performances of elite athletes.
#' The `athlete_sex` parameter enables to compute alactic energy and power over running duration for either male or female 100, 200 and 400 m events.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#' @param athlete_sex A character corresponding the athlete sex for the analysis. Default is "male" and computes energy and power over the Graubner and Nixdorf's (2011) men's 100, 200 and 400 m performances. It also sets the maximal aerobic power to 24.5 W/kg. Similarly, setting athlete_sex to "female" performs the computation on women's performances and sets maximal aerobic power to 21 W/kg.
#' @param mu A double. Parameter setting the peak of the log-normal distribution.Default is -0.4
#' @param sigma A double. Parameter setting the decay of the log-normal distribution. Default is 1
#' @param k1 A double. Time constant of the first rising exponential (s). Default is 2.75
#' @param k2 A double. Time constant of the second decaying exponential (s). Default is 35
#' @param dt A double representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#'
#' @returns A tibble with duration (s), alactic_energy (J/kg), alactic_power (W/kg).
#' @export
#'
#' @examples
#'
#' sprint_alactic_energy_duration_graubner_nixdorf()
#'
sprint_alactic_energy_duration_graubner_nixdorf <- function(data = graubner_nixdorf_sprints, athlete_sex = "male", mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, dt = 0.01) {
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
    alactic_energy = numeric(),
    alactic_power = numeric(),
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
      dt = dt
    )


    sprint_power_data <- sprint_bioenergetic_model_data(sprint_data,
      mu = mu,
      sigma = sigma,
      k1 = k1,
      k2 = k2,
      maximal_aerobic_power = map
    )

    # compute alactic energy vs duration for each event

    sprint_alactic_energy_duration_data <- sprint_alactic_energy_duration(sprint_power_data) |>
      dplyr::mutate(athlete_sex = athlete_sex)

    # add to table

    table <- dplyr::bind_rows(table, sprint_alactic_energy_duration_data)
  }

  return(table)
}


#' Sprint Alactic Duration Model
#'
#' Computes the alactic power as a function of duration using a log-normal distribution model.
#' Other than duration, the alactic capacity is the only parameter of the model. The alactic capacity corresponds to the maximal energy available through the alactic energy pathway.
#'
#' @param duration A numeric vector representing the duration of the sprint (in seconds).
#' @param alactic_capacity A numeric value representing the alactic capacity (in J/kg).
#' @param mu_al A double representing the peak of the log-normal distribution. Default is 1.75.
#' @param sigma_al A double representing the decay of the log-normal distribution. Default is 1.5.
#'
#' @returns A numeric vector representing the average alactic power (in W/kg) as a function of running duration.
#' @export
#'
#' @examples
#'
#' alactic_capacity <- 350 # Example alactic capacity in J/kg
#' duration <- seq(0.1, 10, by = 0.1) # Example durations from 0.1 to 10 seconds
#'
#' alactic_power <- sprint_alactic_duration_model(duration, alactic_capacity)
#' alactic_power
#'
#'
sprint_alactic_duration_model <- function(duration, alactic_capacity, mu_al = 1.75, sigma_al = 1.5) {
  return(alactic_capacity / duration * exp(-(log(duration) - mu_al)^2 / (2 * sigma_al^2)))
}


#' Sprint Alactic Duration Model Fit
#'
#' Performs a non-linear least squares fitting of the alactic power model to the alactic power duration data.
#'
#' @param alactic_power_duration A tibble with the following columns: duration (s), alactic_power (W/kg), corresponding to the average alactic power output over the running duration. This data is used to fit the alactic power model.
#' @param mu_al A double representing the peak of the log-normal distribution. Default is 1.75.
#' @param sigma_al A double representing the decay of the log-normal distribution. Default is 1.5.
#'
#' @returns A fitted model object of class "nls" representing the alactic power model fitted to the alactic power duration data.
#' @export
#'
#' @examples
#' men_alactic_energy <- sprint_alactic_energy_duration_graubner_nixdorf()
#' men_alactic_energy
#'
#'  # Fit alactic model to power
#'
#'  sprint_alactic_duration_model_fit(men_alactic_energy)
#'
sprint_alactic_duration_model_fit <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5) {
  # Fit the alactic power model to the alactic power duration data
  fit <- minpack.lm::nlsLM(alactic_power ~ sprint_alactic_duration_model(duration, alactic_capacity, mu_al = mu_al, sigma_al = sigma_al),
    data = alactic_power_duration,
    start = list(alactic_capacity = 300)
  )

  return(fit)
}


#' Sprint Alactic Duration Model Fit Residual Standard Error
#'
#' Performs a non-linear least squares fitting of the alactic power model to the alactic power duration data and returns the residual standard error.
#'
#' @param alactic_power_duration A tibble with the following columns: duration (s), alactic_power (W/kg), corresponding to the average alactic power output over the running duration. This data is used to fit the alactic power model.
#' @param mu_al A double representing the peak of the log-normal distribution. Default is 1.75.
#' @param sigma_al A double representing the decay of the log-normal distribution. Default is 1.5.
#'
#' @returns A numeric value representing the residual standard error of the fitted alactic power model.
#' @export
#'
#' @examples
#'
#' men_alactic_energy <- sprint_alactic_energy_duration_graubner_nixdorf()
#' men_alactic_energy
#'
#'  # Fit alactic model to power
#'
#'  sprint_alactic_duration_model_fit_rse(men_alactic_energy)
#'
sprint_alactic_duration_model_fit_rse <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5) {
  # Fit the alactic power model to the alactic power duration data
  fit <- sprint_alactic_duration_model_fit(alactic_power_duration, mu_al = mu_al, sigma_al = sigma_al)

  return(summary(fit)$sigma)
}


#' Alactic Capacity from Alactic Power Duration Data
#'
#' Computes the alactic capacity from alactic power duration data by fitting the alactic power over running duration model to the data.
#'
#' @param alactic_power_duration A tibble with the following columns: duration (s), alactic_power (W/kg), corresponding to the average alactic power output over the running duration. This data is used to fit the alactic power model.
#' @param mu_al A double representing the peak of the log-normal distribution. Default is 1.75.
#' @param sigma_al A double representing the decay of the log-normal distribution. Default is 1.5.
#'
#' @returns A numeric value representing the alactic capacity (in J/kg) estimated from the alactic power duration data.
#' @export
#'
#' @examples
#'
#' men_alactic_energy <- sprint_alactic_energy_duration_graubner_nixdorf()
#' men_alactic_energy
#'
#'  # Fit alactic model to power
#'
#'  sprint_alactic_capacity(men_alactic_energy)
#'
sprint_alactic_capacity <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5) {
  # Fit the alactic power model to the alactic power duration data
  fit <- sprint_alactic_duration_model_fit(alactic_power_duration, mu_al = mu_al, sigma_al = sigma_al)

  # Extract the alactic capacity from the fit
  alactic_capacity <- coef(fit)["alactic_capacity"]

  return(alactic_capacity)
}
