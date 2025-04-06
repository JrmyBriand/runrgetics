#' @importFrom utils globalVariables
utils::globalVariables(c("power_aerobic", "power_anaerobic", "power_lactic", "power_alactic"))

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
#' men_100 <- graubner_nixdorf_sprints |>
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
sprint_motion_acceleration_data <- function(sprint_motion_data) {
  acceleration_limit <- which(sprint_motion_data$velocity == max(sprint_motion_data$velocity))

  acceleration_data <- sprint_motion_data[1:acceleration_limit, ]

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
#' men_100 <- graubner_nixdorf_sprints |>
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
sprint_motion_deceleration_data <- function(sprint_motion_data) {
  # compute aerobic and anaerobic power

  sprint_motion_data_mod <- sprint_motion_data |>
    mutate(
      power_aerobic = sprint_approx_aerobic_power(time = time),
      power_anaerobic = power - power_aerobic
    )

  acceleration_limit <- which(sprint_motion_data$velocity == max(sprint_motion_data$velocity))

  deceleration_data <- sprint_motion_data[(acceleration_limit + 1):nrow(sprint_motion_data), ]

  return(deceleration_data)
}




#' Sprint Approximate Aerobic Power
#'
#' @param time A vector of time points (s)
#' @param k_aer A double. Time constant of the exponential rise to the maximal aerobic power (s). Default is 23 s.
#' @param maximal_aerobic_power A double. Maximal aerobic power or MAP (W/kg). Default is 24.5 W/kg corresponding to World class male sprinter observed MAP. The equivalent for female is 21 W/kg
#' @param basal_metabolic_rate A double. Basal metabolic rate (W/kg). Default is 1.2 W/kg
#'
#' @returns A vector of the approximate aerobic power (W/kg) at each time point
#' @export
#'
#' @examples
#'
#' sprint_approx_aerobic_power(time = seq(0, 10, by = 0.1))
#'
sprint_approx_aerobic_power <- function(time, k_aer = 23, maximal_aerobic_power = 24.5, basal_metabolic_rate = 1.2) {
  return((maximal_aerobic_power - basal_metabolic_rate) * (1 - exp(-time / k_aer)))
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
sprint_acceleration_approx_lactic_power <- function(time, maximal_lactic_power, k_an = 2) {
  return(maximal_lactic_power * (1 - exp(-time / k_an)))
}



#' Sprint Approximate Power Distributions
#'
#' Computes approximate aerobic, lactic and alactic power distributions from sprint motion data.
#'
#' @param sprint_motion_data A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power
#' @param maximal_aerobic_power A double. Maximal aerobic power or MAP (W/kg). Default is 24.5 W/kg corresponding to World class male sprinter observed MAP. The equivalent for female is 21 W/kg
#' @param basal_metabolic_rate A double. Basal metabolic rate (W/kg). Default is 1.2 W/kg
#'
#' @returns A tibble with the following columns: time, velocity, acceleration, distance, cost of running, power, power_aerobic, power_anaerobic, power_lactic, power_alactic
#'
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom stats lm
#' @importFrom stats predict
#'
#'
#' @export
#'
#'
#' @examples
#'
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints |>
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
#' sprint_approx_power_distributions(sprint_data,
#'   maximal_aerobic_power = 24.5,
#'   basal_metabolic_rate = 1.2
#' )
#'
sprint_approx_power_distributions <- function(sprint_motion_data, maximal_aerobic_power = 24.5, basal_metabolic_rate = 1.2) {
  # calculate aerobic and anaerobic power

  sprint_motion_data_mod <- sprint_motion_data |>
    mutate(
      power_aerobic = sprint_approx_aerobic_power(time = time, maximal_aerobic_power = maximal_aerobic_power, basal_metabolic_rate = basal_metabolic_rate),
      power_anaerobic = power - power_aerobic
    )


  # extract acceleration data
  acceleration_data <- sprint_motion_acceleration_data(sprint_motion_data_mod)

  # extract deceleration data
  deceleration_data <- sprint_motion_deceleration_data(sprint_motion_data_mod)

  # estimate the maximal lactic power as the anaerobic power at the end of the acceleration

  maximal_lactic_power <- acceleration_data$power_anaerobic[nrow(acceleration_data)]

  # compute the aerobic power alactic and lactic contribution in the acceleration

  acceleration_data <- acceleration_data |>
    dplyr::mutate(
      power_lactic = sprint_acceleration_approx_lactic_power(time = time, maximal_lactic_power = maximal_lactic_power),
      power_alactic = power_anaerobic - power_lactic,
    )

  # Construct a linear model for the alactic power

  power_alactic_end_acceleration <- acceleration_data$power_alactic[nrow(acceleration_data)]
  power_alactic_end_sprint <- 0
  alactic_powers <- c(power_alactic_end_acceleration, power_alactic_end_sprint)
  times <- c(deceleration_data$time[1], deceleration_data$time[nrow(deceleration_data)])

  alactic_model <- stats::lm(alactic_powers ~ times)

  # Compute the alactic power at each time point

  deceleration_data <- deceleration_data |>
    dplyr::mutate(
      power_alactic = stats::predict(alactic_model, newdata = tibble::tibble(times = time)),
      power_lactic = power_anaerobic - power_alactic
    )

  # invert power alactic and power_lactic columns in deceleration data

  deceleration_data <- deceleration_data |>
    dplyr::relocate(power_lactic, .before = power_alactic)

  # Bind acceleration and deceleration data together

  sprint_energetics_data <- dplyr::bind_rows(acceleration_data, deceleration_data)

  return(sprint_energetics_data)
}

#' Log-normal Fit on Approximate Alactic Power Distribution
#'
#' Fits a log-normal distribution to the approximate alactic power distribution.
#'
#' @param sprint_approx_power_distribution A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_alactic (W/kg)
#'
#' @returns A list with the following elements: pal_max, sigma, mu
#'
#' @importFrom minpack.lm nlsLM
#'
#' @export
#'
#' @examples
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints |>
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
#' sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
#'   maximal_aerobic_power = 24.5,
#'   basal_metabolic_rate = 1.2
#' )
#'
#' # Fit the log-normal distribution to the alactic power distribution
#'
#' fit_approx_alactic_power_model(sprint_approx_power_distributions)
#'
fit_approx_alactic_power_model <- function(sprint_approx_power_distribution) {
  log_norm_dist <- minpack.lm::nlsLM(power_alactic ~ pal_max * exp(-(log(time) - mu)^2 / (2 * sigma^2)), data = sprint_approx_power_distribution, start = list(pal_max = 160, sigma = 1, mu = 1))

  return(list(
    pal_max = coef(log_norm_dist)[1],
    sigma = coef(log_norm_dist)[2],
    mu = coef(log_norm_dist)[3]
  ))
}

#' Sprint Alactic Power Model (on Approximate Alactic Power Distribution)
#'
#' Estimates the alactic power at each time point using the alactic power model based on a log-normal function and
#' the approximate alactic power estimations through the course of the sprint.
#'
#' @param time A vector of time points (s)
#' @param maximal_alactic_power A double. Maximal alactic power over the race duration (W/kg).
#' @param mu A double. Parameter setting the peak of the log-normal distribution.
#' @param sigma A double. Parameter setting the decay of the log-normal distribution.
#'
#' @returns A vector of the approximate alactic power (W/kg) at each time point
#' @export
#'
#' @examples
#'
#' maximal_alactic_power <- 160
#' mu <- 1
#' sigma <- 1
#'
#' time <- seq(1, 10, 0.1)
#'
#' sprint_approx_alactic_power_model(time, maximal_alactic_power, mu, sigma)
#'
sprint_approx_alactic_power_model <- function(time,
                                              maximal_alactic_power,
                                              mu,
                                              sigma) {
  return(maximal_alactic_power * exp(-(log(time) - mu)^2 / (2 * sigma^2)))
}




#' Bi-exponential Fit on Approximate lactic Power Distribution
#'
#' @param sprint_approx_power_distribution A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_lactic (W/kg)
#'
#' @returns A list with the following elements: p_la_max, k1, k2
#'
#' @importFrom minpack.lm nlsLM
#'
#' @export
#'
#'
#' @examples
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints |>
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
#' sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
#'   maximal_aerobic_power = 24.5,
#'   basal_metabolic_rate = 1.2
#' )
#'
#' # Fit the log-normal distribution to the alactic power distribution
#'
#' fit_approx_lactic_power_model(sprint_approx_power_distributions)
#'
fit_approx_lactic_power_model <- function(sprint_approx_power_distribution) {
  bi_exp_dist <- minpack.lm::nlsLM(
    power_lactic ~ sprint_approx_lactic_power_model(
      time = time,
      maximal_lactic_power = p_la_max,
      k1 = k1,
      k2 = k2
    ),
    data = sprint_approx_power_distribution, start = list(p_la_max = 60, k1 = 2.5, k2 = 35)
  )


  return(list(
    p_la_max = coef(bi_exp_dist)[1],
    k1 = coef(bi_exp_dist)[2],
    k2 = coef(bi_exp_dist)[3]
  ))
}

#' Sprint Lactic Power Model (on Approximate Lactic Power Distribution)
#'
#' Estimates lactic power at each time point using the lactic power model based on a bi_exponential
#' and approximate lactic power through the course of the sprint
#'
#' @param time A vector of time points (s)
#' @param maximal_lactic_power A double. Maximal lactic power over the race duration (W/kg).
#' @param k1 A double. Time constant of the first rising exponential (s).
#' @param k2 A double. Time constant of the second decaying exponential (s).
#'
#' @returns A vector of the approximate lactic power (W/kg) at each time point
#' @export
#'
#'
#' @examples
#'
#' maximal_lactic_power <- 60
#' k1 <- 2.5
#' k2 <- 35
#'
#' time <- seq(1, 10, 0.1)
#'
#' sprint_approx_lactic_power_model(time, maximal_lactic_power, k1, k2)
#'
sprint_approx_lactic_power_model <- function(time, maximal_lactic_power, k1, k2) {
  k_norm <- (k1 + k2) / (k2 * (k1 / (k1 + k2))^(k1 / k2))

  return(maximal_lactic_power * k_norm * (1 - exp(-time / k1)) * exp(-time / k2))
}
