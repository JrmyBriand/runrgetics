
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


  distance_rec <- sprint_recover_distance(sprint_power_data$power, dt = 0.01)

  distance_mod_rec<- sprint_recover_distance(sprint_power_data$power_mod, dt = 0.01)

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



sprint_briand_article_gof_table <- function(data = graubner_nixdorf_sprints, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35){

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
      maximal_velocity = event_data$maximal_velocity[1]
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

    metrics <- sprint_bioenergetic_model_gof_metrics(sprint_power_data, i, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = map)

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
    "Modeled Recovered Distance (m)",
    "Distance Difference (%)"
  )

  return(tinytable::tt(table, notes = "Metrics comparing the sprint bioenergetics model to observed metabolic power from di Prampero et al.’s (2005; 2018) methods and Graubner and Nixdorf’s (2011) data. "))


}


sprint_energy_contributions <- function(sprint_power_data, event_name){



}


sprint_energy_cont_briand_article_table <- function(){



}

