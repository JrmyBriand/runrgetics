#' Title
#'
#' @param sprint_power_data
#'
#' @returns
#' @export
#'
#' @examples
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

#' Title
#'
#' @param data
#' @param athlete_sex
#' @param mu
#' @param sigma
#' @param k1
#' @param k2
#' @param dt
#'
#' @returns
#' @export
#'
#' @examples
sprint_lactic_energy_duration_graubner_nixdorf <- function(data = graubner_nixdorf_sprints, athlete_sex = "male", mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, dt = 0.01) {
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
      dt = dt
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


#' Title
#'
#' @param duration
#' @param lactic_capacity
#' @param k1
#' @param k2
#'
#' @returns
#' @export
#'
#' @examples
sprint_lactic_duration_model <- function(duration, lactic_capacity, k1 = 20, k2 = 2000) {
  knorm <- bi_exponential_knorm(t1 = k1, t2 = k2)

  return(lactic_capacity * knorm * (1 - exp(-(duration - 3) / k1)) * exp(-(duration - 3) / k2))
}


#' Title
#'
#' @param lactic_energy_duration
#' @param k1
#' @param k2
#'
#' @returns
#' @export
#'
#' @examples
sprint_lactic_duration_model_fit <- function(lactic_energy_duration, k1 = 20, k2 = 2000) {
  # Fit the alactic power model to the alactic power duration data
  fit <- minpack.lm::nlsLM(lactic_energy ~ sprint_lactic_duration_model(duration, lactic_capacity, k1 = k1, k2 = k2),
    data = lactic_energy_duration,
    start = list(lactic_capacity = 1300)
  )

  return(fit)
}


#' Title
#'
#' @param lactic_energy_duration
#' @param k1
#' @param k2
#'
#' @returns
#' @export
#'
#' @examples
sprint_lactic_duration_model_fit_rse <- function(lactic_energy_duration, k1 = 20, k2 = 2000) {
  # Fit the lactic energy model to the lactic energy duration data
  fit <- sprint_lactic_duration_model_fit(lactic_energy_duration, k1 = k1, k2 = k2)

  return(summary(fit)$sigma)
}


#' Title
#'
#' @param lactic_energy_duration
#' @param k1
#' @param k2
#'
#' @returns
#' @export
#'
#' @examples
sprint_lactic_capacity <- function(lactic_energy_duration, k1 = 20, k2 = 2000) {
  # Fit the lactic energy model to the lactic energy duration data
  fit <- sprint_lactic_duration_model_fit(lactic_energy_duration, k1 = k1, k2 = k2)

  # Extract the alactic capacity from the fit
  lactic_capacity <- coef(fit)["lactic_capacity"]

  return(lactic_capacity)
}
