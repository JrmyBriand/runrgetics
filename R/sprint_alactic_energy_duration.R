

#' Title
#'
#' @param sprint_power_data
#'
#' @returns
#' @export
#'
#' @examples
sprint_alactic_energy_duration <- function(sprint_power_data){

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



#' Title
#'
#' @param data
#' @param athlete_sex
#'
#' @returns
#' @export
#'
#' @examples
sprint_alactic_energy_duration_graubner_nixdorf <- function(data = graubner_nixdorf_sprints, athlete_sex = "male", mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, dt = 0.01){

  if(athlete_sex == "male"){
    dat <- data |>
      dplyr::filter(event == "Men's 100 m"|
                      event == "Men's 200 m"|
                      event == "Men's 400 m")

    map <- 24.5

  }

  if(athlete_sex == "female"){
    dat <- data |>
      dplyr::filter(event == "Women's 100 m"|
                      event == "Women's 200 m"|
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


    sprint_power_data  <-  sprint_bioenergetic_model_data(sprint_data,
                                                          mu = mu,
                                                          sigma = sigma,
                                                          k1 = k1,
                                                          k2 = k2,
                                                          maximal_aerobic_power = map)

    # compute alactic energy vs duration for each event

    sprint_alactic_energy_duration_data <- sprint_alactic_energy_duration(sprint_power_data) |>
      dplyr::mutate(athlete_sex = athlete_sex)

    # add to table

    table <- dplyr::bind_rows(table, sprint_alactic_energy_duration_data)

  }

  return(table)


}


#' Title
#'
#' @param duration
#' @param alactic_capacity
#' @param mu_al
#' @param sigma_al
#'
#' @returns
#' @export
#'
#' @examples
sprint_alactic_duration_model <- function(duration, alactic_capacity, mu_al = 1.75, sigma_al = 1.5){
 return( alactic_capacity/duration*exp(-(log(duration) - mu_al)^2/(2*sigma_al^2)))

}


#' Title
#'
#' @param alactic_power_duration
#' @param mu_al
#' @param sigma_al
#'
#' @returns
#' @export
#'
#' @examples
sprint_alactic_duration_model_fit <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5){

  # Fit the alactic power model to the alactic power duration data
  fit <- minpack.lm::nlsLM(alactic_power ~ sprint_alactic_duration_model(duration, alactic_capacity, mu_al = mu_al, sigma_al = sigma_al),
             data = alactic_power_duration,
             start = list(alactic_capacity = 300))

  return(fit)

}


#' Title
#'
#' @param alactic_power_duration
#' @param mu_al
#' @param sigma_al
#'
#' @returns
#' @export
#'
#' @examples
sprint_alactic_duration_model_fit_rse <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5){

  # Fit the alactic power model to the alactic power duration data
  fit <- sprint_alactic_duration_model_fit(alactic_power_duration, mu_al = mu_al, sigma_al = sigma_al)

  return(summary(fit)$sigma)



}


#' Title
#'
#' @param alactic_power_duration
#' @param mu_al
#' @param sigma_al
#'
#' @returns
#' @export
#'
#' @examples
sprint_alactic_capacity <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5){

  # Fit the alactic power model to the alactic power duration data
  fit <- sprint_alactic_duration_model_fit(alactic_power_duration, mu_al = mu_al, sigma_al = sigma_al)

  # Extract the alactic capacity from the fit
  alactic_capacity <- coef(fit)["alactic_capacity"]

  return(alactic_capacity)

}
