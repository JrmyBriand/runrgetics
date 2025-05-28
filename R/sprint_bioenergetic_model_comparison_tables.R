
#' Sprint Bioenergetic Model Goodness of Fit Metrics
#'
#' Computes a set of metrics that can be used to assess the sprint bioenergetic model's goodness of fit to the observed metabolic power data.
#' These metrics include adjusted R-squared, total energy contribution, percentage difference in energy contribution, estimated maximum metabolic power,
#' percentage difference in maximum metabolic power, modeled recovered distance, and percentage difference in recovered distance. The metrics were reported in
#' Briand et al. (2025) and are used to compare the sprint bioenergetic model to the observed metabolic power data from di Prampero et al. (2005, 2018) derived from Graubner and Nixdorf (2011) data.
#'
#' @param sprint_power_data A tibble with at least the following columns: time (s), power (W/kg), power_mod (W/kg), corresponding respectively to the power estimated by di Prampero et al.'s (2005, 2018) equivalent slope approach, and the power estimated using Briand et al.'s (2025) sprint bioenergetic model.
#' @param event A character corresponding to the name of the events from which the sprint power data was derived.
#' @param mu A double. Parameter setting the peak of the log-normal distribution.Default is -0.4
#' @param sigma A double. Parameter setting the decay of the log-normal distribution. Default is 1
#' @param k1 A double. Time constant of the first rising exponential (s). Default is 2.75
#' @param k2 A double. Time constant of the second decaying exponential (s). Default is 35
#' @param maximal_aerobic_power A double corresponding to the estimated maximal aerobic power of the sprinter (W/kg). Default is 24.5.
#' @param dt A double representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#'
#' @returns A tibble with the following columns: event, adjusted_Rsquared, energy_total, energy_percent_diff, estimated_max_power, percentage_difference_max_power, distance_modeled_recovered, distance_percent_diff.
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
#'
#' # compute the power estimated from the sprint bioenergetic model
#'
#' bolt_modeled_data <- sprint_bioenergetic_model_data(bolt_100m_motion_data)
#'
#' sprint_bioenergetic_model_gof_metrics(bolt_modeled_data, event = "Men's 100 m")
#'
sprint_bioenergetic_model_gof_metrics <- function(sprint_power_data, event,  mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, maximal_aerobic_power = 24.5, dt = 0.01){

  # compute metrics from the sprint power data

  #adjusted_Rsquared

  adj_Rsquared <- sprint_bioenergetic_model_adj_R2(
    sprint_motion_data = sprint_power_data,
    mu = mu,
    sigma = sigma,
    k1 = k1,
    k2 = k2,
    maximal_aerobic_power = maximal_aerobic_power
  )

  # Total Energy Contribution

  energy_total <- energy_total(sprint_power_data, type = "power bioenergetic model")

  energy_slope_approach <- energy_total(sprint_power_data, type = "power")

  #percentage difference

  energy_percent_diff <- ( energy_total - energy_slope_approach) / energy_slope_approach * 100


  # Maximum Metabolic Power

 slope_approach_max_power <- sprint_maximum_metabolic_power(sprint_power_data, type = "power")


  # maximal metabolic power from the sprint bioenergetic model
  bioenergetic_max_power <- sprint_maximum_metabolic_power(sprint_power_data, type = "power bioenergetic model")

  # percentage difference between the two maximum metabolic power estimates

  percent_diff_max_power <- (bioenergetic_max_power - slope_approach_max_power ) / slope_approach_max_power  * 100

  # traveled distance


  distance_rec <- sprint_recover_distance(sprint_power_data$power, dt = dt)

  distance_mod_rec<- sprint_recover_distance(sprint_power_data$power_mod, dt = dt)

  # Distance percentage_difference

  dist_percent_diff <- ( distance_mod_rec - distance_rec  ) / distance_rec * 100

  # return a tibble with the metrics

  metrics <- tibble::tibble(
    event = event,
    adjusted_Rsquared = round(adj_Rsquared, 2),
    energy_total = round(energy_total, 2),
    energy_percent_diff = round(energy_percent_diff,2),
    estimated_max_power = round(bioenergetic_max_power,2),
    percentage_difference_max_power = round(percent_diff_max_power ,2),
    distance_modeled_recovered = round(distance_mod_rec,2),
    distance_percent_diff = round(dist_percent_diff,2)
  )

  return(metrics)
}



#' Sprint Bioenergetic Model Goodness of Fit Table
#'
#' Computes a table of metrics assessing the sprint bioenergetic model's goodness of fit on metabolic power data computed
#' using di Prampero et al.'s (2005, 2018) equivalent slope approach, as presented
#' in Briand et al. (2025). The table presents the metrics for the men's and women's 100, 200 and 400 m
#' provided in Graubner and Nixdorf's (2011) dataset. The table includes metrics such as adjusted R-squared, total energy contribution, percentage difference in energy contribution, estimated maximum metabolic power, percentage difference in maximum metabolic power, modeled recovered distance, and percentage difference in recovered distance. The metrics are computed for each event in the provided data set.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#' @param mu A double. Parameter setting the peak of the log-normal distribution.Default is -0.4
#' @param sigma A double. Parameter setting the decay of the log-normal distribution. Default is 1
#' @param k1 A double. Time constant of the first rising exponential (s). Default is 2.75
#' @param k2 A double. Time constant of the second decaying exponential (s). Default is 35
#' @param dt A double representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#'
#' @returns A tinytable object with the following columns: event, adjusted_Rsquared, energy_total, energy_percent_diff, estimated_max_power, percentage_difference_max_power, distance_modeled_recovered, distance_percent_diff.
#' @export
#'
#' @examples
#'
#' sprint_briand_article_gof_table()
#'
sprint_briand_article_gof_table <- function(data = graubner_nixdorf_sprints, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, dt = 0.01){

  # events
  events <- unique(data$event)


  # initialize table

  table <- tibble::tibble(
    event = character(),
    adjusted_Rsquared = numeric(),
    energy_total = numeric(),
    energy_percent_diff = numeric(),
    estimated_max_power = numeric(),
    percentage_difference_max_power = numeric(),
    distance_modeled_recovered = numeric(),
    distance_percent_diff = numeric()
  )

  for (i in events) {
    event_data <- data |>
      dplyr::filter(event == i)


    sprint_data <- sprint_motion_model_data(
      mean_velocity_splits = event_data$velocity,
      time_splits = event_data$splits,
      distance = event_data$distance,
      reaction_time = event_data$reaction_time[1],
      maximal_velocity = event_data$maximal_velocity[1],
      dt = dt
    )

    # set a max aerobic power of 24.5 for men's event and 21 for Women's event

    if (i == "Men's 100 m" | i == "Men's 200 m" | i == "Men's 400 m") {
      map <- 24.5
    } else {
      map <- 21
    }


    sprint_power_data  <-  sprint_bioenergetic_model_data(sprint_data,
                                                          mu = mu,
                                                          sigma = sigma,
                                                          k1 = k1,
                                                          k2 = k2,
                                                          maximal_aerobic_power = map)

    metrics <- sprint_bioenergetic_model_gof_metrics(sprint_power_data, i, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = map, dt = dt)

    table <- dplyr::bind_rows(table, metrics)

  }

  # improve the name of the columns


  colnames(table) <- c(
    "Event",
    "Adjusted R-squared",
    "Total Energy Contribution (kJ)",
    "Energy Contribution Difference (%)",
    "Estimated Max Power (W/kg)",
    "Max Power Difference (%)",
    "Modeled Distance (m)",
    "Distance Difference (%)"
  )

  return(tinytable::tt(table, notes = "Metrics comparing the sprint bioenergetics model to observed metabolic power from di Prampero et al.'s (2005; 2018) methods and Graubner and Nixdorf's (2011) data."))


}


#' Sprint Event Metabolic Pathways Energy Contributions
#'
#' Computes the energy contributions of the alactic, lactic and aerobic pathways during a sprint event using the sprint bioenergetic model.
#' The function also provides the total energy expended during the sprint and the percentage contributions of each pathway to the total energy expenditure.
#'
#'
#' @param sprint_power_data A tibble with at least the following columns: time (s), power_alactic (W/kg), power_lactic (W/kg) and power_aerobic (W/kg), as well as power (W/kg), power_mod (W/kg), corresponding respectively to the power estimated by di Prampero et al.'s (2005, 2018) equivalent slope approach, and the power estimated using Briand et al.'s (2025) sprint bioenergetic model.
#' @param event_name A character corresponding to the name of the event from which the sprint power data was derived.
#'
#' @returns A tibble with the following columns: event, alactic_energy, lactic_energy, aerobic_energy, total_energy, alactic_percentage, lactic_percentage, aerobic_percentage.
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
#' sprint_energy_contributions(bolt_modeled_data, event = "Men's 100 m")
#'
#'
sprint_energy_contributions <- function(sprint_power_data, event_name){


  # alactic energy
  al_energy <-energy_alactic(sprint_power_data)

  #lactic_energy

  la_energy <- energy_lactic(sprint_power_data)

  # aerobic energy

  ae_energy <- energy_aerobic(sprint_power_data)

  # total energy

  tot_energy <- energy_total(sprint_power_data, type = "power bioenergetic model")

  # alactic percentage

  alactic_percentage <- alactic_energy_percentage(sprint_power_data, type = "power bioenergetic model")

  # lactic percentage

  lactic_percentage <- lactic_energy_percentage(sprint_power_data, type = "power bioenergetic model")

  # aerobic percentage

  aerobic_percentage <- aerobic_energy_percentage(sprint_power_data, type = "power bioenergetic model")


  # create a tibble with the results

  energy_contributions <- tibble::tibble(
    event = event_name,
    alactic_energy = round(al_energy, 2),
    lactic_energy = round(la_energy, 2),
    aerobic_energy = round(ae_energy, 2),
    total_energy = round(tot_energy, 2),
    alactic_percentage = round(alactic_percentage, 2),
    lactic_percentage = round(lactic_percentage, 2),
    aerobic_percentage = round(aerobic_percentage, 2)
  )

  return(energy_contributions)


}


#' Sprint Metabolic Pathways Energy Contributions Table
#'
#' Computes a table of energy contributions of the alactic, lactic and aerobic pathways during sprint events using the sprint bioenergetic model.
#' The function provides the total energy expended during the sprint and the percentage contributions of each pathway to the total energy expenditure.
#' The table computes the energy contribution in the men's and women's 100, 200 and 400 m and corresponds to Table 3 from Briand et al. (2025).
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#' @param mu A double. Parameter setting the peak of the log-normal distribution.Default is -0.4
#' @param sigma A double. Parameter setting the decay of the log-normal distribution. Default is 1
#' @param k1 A double. Time constant of the first rising exponential (s). Default is 2.75
#' @param k2 A double. Time constant of the second decaying exponential (s). Default is 35
#' @param dt A double representing the time step at which power is provided (in seconds). Default is 0.01 seconds.
#'
#' @returns A tinytable object with the following columns: event, alactic_energy, lactic_energy, aerobic_energy, total_energy, alactic_percentage, lactic_percentage, aerobic_percentage.
#' @export
#'
#' @examples
#'
#' sprint_energy_cont_briand_article_table ()
#'
#'
sprint_energy_cont_briand_article_table <- function(data = graubner_nixdorf_sprints, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, dt = 0.01){

  # events
  events <- unique(data$event)


  # initialize table

  table <- tibble::tibble(
    event = character(),
    alactic_energy = numeric(),
    lactic_energy = numeric(),
    aerobic_energy = numeric(),
    total_energy = numeric(),
    alactic_percentage = numeric(),
    lactic_percentage = numeric(),
    aerobic_percentage = numeric()
  )

  for (i in events) {
    event_data <- data |>
      dplyr::filter(event == i)


    sprint_data <- sprint_motion_model_data(
      mean_velocity_splits = event_data$velocity,
      time_splits = event_data$splits,
      distance = event_data$distance,
      reaction_time = event_data$reaction_time[1],
      maximal_velocity = event_data$maximal_velocity[1],
      dt = dt
    )

    # set a max aerobic power of 24.5 for men's event and 21 for Women's event

    if (i == "Men's 100 m" | i == "Men's 200 m" | i == "Men's 400 m") {
      map <- 24.5
    } else {
      map <- 21
    }


    sprint_power_data  <-  sprint_bioenergetic_model_data(sprint_data,
                                                          mu = mu,
                                                          sigma = sigma,
                                                          k1 = k1,
                                                          k2 = k2,
                                                          maximal_aerobic_power = map)

    metrics <- sprint_energy_contributions(sprint_power_data, i)

    table <- dplyr::bind_rows(table, metrics)

  }

  # improve the name of the columns

  colnames(table) <- c(
    "Event",
    "Alactic Energy (J/kg)",
    "Lactic Energy (J/kg)",
    "Aerobic Energy (J/kg)",
    "Total Energy (J/kg)",
    "Alactic Percentage (%)",
    "Lactic Percentage (%)",
    "Aerobic Percentage (%)"
  )

  return(tinytable::tt(table, notes = "Energy contributions of three energy systems per sprint distance. Percentage values reflect total energy expended per sprint."))



}

