#' Sprint Bioenergetic Model
#'
#' @param time A numeric vector representing time points (in seconds) at which to calculate the power output.
#' @param maximal_alactic_power A double corresponding to the maximal alactic power attained over the sprint (W/kg)
#' @param maximal_lactic_power A double corresponding to the maximal lactic power attained over the sprint (W/kg)
#' @param mu A double. Parameter setting the peak of the log-normal distribution.Default is -0.4
#' @param sigma A double. Parameter setting the decay of the log-normal distribution. Default is 1
#' @param k1 A double. Time constant of the first rising exponential (s). Default is 2.75
#' @param k2 A double. Time constant of the second decaying exponential (s). Default is 35
#' @param maximal_aerobic_power A double. Maximal aerobic power (W/kg). Default is 24.5
#' @param output A string. The type of power output to return. Options are "total power", "alactic power", "lactic power", and "aerobic power". Default is "total power".
#'
#' @returns A double representing the power output (W/kg) at a given time point.
#' @export
#'
#' @examples
#'
#'
#' time <- seq(0, 30, by = 0.01)
#' maximal_alactic_power <- 100
#' maximal_lactic_power <- 50
#'
#'
#' sprint_bioenergetic_model(time, maximal_alactic_power, maximal_lactic_power)
#'
sprint_bioenergetic_model <- function(time, maximal_alactic_power, maximal_lactic_power, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35 , maximal_aerobic_power = 24.5, output = "total power") {


  #glycolytic normalisation constant


  knorm <- bi_exponential_knorm(k1, k2)

  # Calculate power contributions for each time point
  palact <- maximal_alactic_power * exp(-(log(time) - mu)^2 / (2 * sigma^2))
  pgly <- maximal_lactic_power *knorm* (1 - exp(-time / k1)) * exp(-time / k2)
  paer <- (maximal_aerobic_power - 1.2) * (1 - exp(-time / 23))

  # Total sprint power
  p_total <- palact + pgly + paer

  if (output == "total power") {
    return(p_total)
  }
  if (output == "alactic power") {
    return(palact)
  }
  if (output == "lactic power") {
    return(pgly)
  }
  if (output == "aerobic power") {
    return(paer)
  }

}


#' Fit Sprint Bioenergetic Model to Sprint Motion Data
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @param maximal_aerobic_power A double corresponding to the estimated maximal aerobic power of the sprinter (W/kg). Default is 24.5.
#'
#' @returns An object of class \code{nls} and \code{nls.lm}, as returned by \code{nlsLM()}. This object contains the fitted model and can be used with generic functions like \code{summary()}, \code{predict()}, and \code{coef()}.
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
#' # Fit sprint bioenergetic model to sprint motion data
#'
#' fit <- sprint_bioenergetic_model_fit(bolt_100m_motion_data)
#' fit
#'
sprint_bioenergetic_model_fit <- function(sprint_motion_data, maximal_aerobic_power = 24.5){

  fit <- minpack.lm::nlsLM(
    power ~ sprint_bioenergetic_model(time, maximal_alactic_power, maximal_lactic_power, maximal_aerobic_power = maximal_aerobic_power),
    data = sprint_motion_data,
    start = list(maximal_alactic_power = 110, maximal_lactic_power = 50),
    lower = c(maximal_alactic_power = 0, maximal_lactic_power= 0),
    upper = c(maximal_alactic_power = Inf, maximal_lactic_power = Inf)
  )

  return(fit)

}


#' Extract Sprint Maximal Alactic Power from Sprint Motion Data
#'
#' Applies the sprint bioenergetic model to extract the sprint maximal alactic power from the sprint motion data.
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @param maximal_aerobic_power A double corresponding to the estimated maximal aerobic power of the sprinter (W/kg). Default is 24.5.
#'
#' @returns A double representing the sprint maximal alactic power (W/kg) extracted from the sprint motion data.
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
#' # extract maximal alactic power
#'
#' maximal_alactic_power <- sprint_bioenergetic_model_max_al(bolt_100m_motion_data)
#' maximal_alactic_power
#'
#'
sprint_bioenergetic_model_max_al <- function(sprint_motion_data, maximal_aerobic_power = 24.5){

  fit <- sprint_bioenergetic_model_fit(sprint_motion_data, maximal_aerobic_power)

  params <- stats::coef(fit)

  return(params["maximal_alactic_power"])

}


#' Extract Sprint Maximal Lactic Power from Sprint Motion Data
#'
#' Applies the sprint bioenergetic model to extract the sprint maximal lactic power from the sprint motion data.
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @param maximal_aerobic_power A double corresponding to the estimated maximal aerobic power of the sprinter (W/kg). Default is 24.5.
#'
#' @returns A double representing the sprint maximal alactic power (W/kg) extracted from the sprint motion data.
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
#' # extract maximal lactic power
#'
#' maximal_lactic_power <- sprint_bioenergetic_model_max_la(bolt_100m_motion_data)
#' maximal_lactic_power
#'
#'
sprint_bioenergetic_model_max_la <- function(sprint_motion_data, maximal_aerobic_power = 24.5){

  fit <- sprint_bioenergetic_model_fit(sprint_motion_data, maximal_aerobic_power)

  params <- stats::coef(fit)

  return(params["maximal_lactic_power"])


}


