#' @importFrom utils globalVariables
utils::globalVariables(c("time", "velocity", "acceleration", "power", "color", "splits"))

#' Plot sprint distance as a function of time
#'
#' Plot of modeled sprint distance as a function of time with points
#' representing time splits measured at the corresponding distances.
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#' @param color A string with the color of the plot's line and points. Default is set at "darkgreen"
#'
#' @returns A ggplot object
#' @import ggplot2
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples plot_sprint_distance(
#'   mean_velocity_splits = c(0, 5.77, 9.99),
#'   time_splits = c(0, 1.88, 2.88),
#'   distance = c(0, 10, 20),
#'   reaction_time = 0.146,
#'   maximal_velocity = 12.34
#' )
#'
plot_sprint_distance <- function(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = NA, dt = 0.01, color = "darkgreen") {
  # get sprint motion metrics

  sprint_data <- sprint_motion_model_data(
    mean_velocity_splits = mean_velocity_splits,
    time_splits = time_splits,
    distance = distance,
    reaction_time = reaction_time,
    maximal_velocity = maximal_velocity
  )

  # the sprint data considers data without the reaction time

  observed <- tibble::tibble(
    time = time_splits - reaction_time,
    distance = distance
  )


  # Distance plot
  plot <- ggplot2::ggplot() +
    # Male predictions
    ggplot2::geom_line(
      data = sprint_data,
      aes(x = time, y = distance),
      color = color, linetype = "solid"
    ) +
    # Male observed data
    ggplot2::geom_point(
      data = observed,
      aes(x = time, y = distance),
      shape = 16, size = 2, color = color
    ) +
    labs(x = "Time (s)", y = "Distance (m)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )

  return(plot)
}


#' Plot sprint velocity as a function of time
#'
#' Plot of modeled sprint velocity as a function of time with points
#' representing time splits measured at the corresponding distances.
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#' @param color A string with the color of the plot's line and points. Default is set at "darkred"
#'
#'
#' @returns A ggplot object
#'
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
#'
#' @examples plot_sprint_velocity(
#'   mean_velocity_splits = c(0, 5.77, 9.99),
#'   time_splits = c(0, 1.88, 2.88),
#'   distance = c(0, 10, 20),
#'   reaction_time = 0.146,
#'   maximal_velocity = 12.34
#' )
#'
#'
plot_sprint_velocity <- function(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = NA, dt = 0.01, color = "darkred") {
  # get sprint motion metrics

  sprint_data <- sprint_motion_model_data(
    mean_velocity_splits = mean_velocity_splits,
    time_splits = time_splits,
    distance = distance,
    reaction_time = reaction_time,
    maximal_velocity = maximal_velocity
  )

  # the sprint data considers data without the reaction time (the points start at 0, 0, no reaction time)

  time_velocity <- find_time_velocity(time_splits, reaction_time = reaction_time) - reaction_time

  observed <- tibble::tibble(
    time = time_velocity,
    velocity = mean_velocity_splits
  )


  # Velocity plot
  plot <- ggplot2::ggplot() +
    # Male predictions
    ggplot2::geom_line(
      data = sprint_data,
      aes(x = time, y = velocity),
      color = color, linetype = "solid"
    ) +
    # Male observed data
    ggplot2::geom_point(
      data = observed,
      aes(x = time, y = velocity),
      shape = 16, size = 2, color = color
    ) +
    ggplot2::labs(x = "Time (s)", y = "Velocity (m/s)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )

  return(plot)
}


#' Plot sprint acceleration as a function of time
#'
#' Plot of modeled sprint acceleration as a function of time.
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#' @param color A string with the color of the plot's line and points. Default is set at "blue"
#'
#'
#' @returns A ggplot object
#'
#' @import ggplot2
#' @export
#'
#' @examples  plot_sprint_acceleration(
#'   mean_velocity_splits = c(0, 5.77, 9.99),
#'   time_splits = c(0, 1.88, 2.88),
#'   distance = c(0, 10, 20),
#'   reaction_time = 0.146,
#'   maximal_velocity = 12.34
#' )
#'
plot_sprint_acceleration <- function(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = NA, dt = 0.01, color = "blue") {
  # get sprint motion metrics

  sprint_data <- sprint_motion_model_data(
    mean_velocity_splits = mean_velocity_splits,
    time_splits = time_splits,
    distance = distance,
    reaction_time = reaction_time,
    maximal_velocity = maximal_velocity
  )


  # Acceleration plot
  plot <- ggplot2::ggplot() +
    # Male predictions
    ggplot2::geom_line(
      data = sprint_data,
      aes(x = time, y = acceleration),
      color = color, linetype = "solid", linewidth = 1
    ) +
    ggplot2::labs(x = "Time (s)", y = "Acceleration (m/s\u00B2)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )

  return(plot)
}


#' Plot sprint metabolic power as a function of time
#'
#'
#' Plot of modeled sprint metabolic as a function of time. The power computation is based on the method
#' proposed by di Prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#' @param color A string with the color of the plot's line and points. Default is set at "purple"
#'
#'
#' @returns A ggplot object
#'
#' @import ggplot2
#' @export
#'
#' @examples plot_sprint_metabolic_power(
#'   mean_velocity_splits = c(0, 5.77, 9.99),
#'   time_splits = c(0, 1.88, 2.88),
#'   distance = c(0, 10, 20),
#'   reaction_time = 0.146,
#'   maximal_velocity = 12.34
#' )
#'
plot_sprint_metabolic_power <- function(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = NA, dt = 0.01, color = "purple") {
  # get sprint motion metrics

  sprint_data <- sprint_motion_model_data(
    mean_velocity_splits = mean_velocity_splits,
    time_splits = time_splits,
    distance = distance,
    reaction_time = reaction_time,
    maximal_velocity = maximal_velocity
  )


  # power plot
  plot <- ggplot2::ggplot() +
    # Male predictions
    ggplot2::geom_line(
      data = sprint_data,
      aes(x = time, y = power),
      color = color, linetype = "solid", linewidth = 1
    ) +
    ggplot2::labs(x = "Time (s)", y = "Power (W/kg)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )

  return(plot)
}


#' Plot Sprint Motion Analysis
#'
#' Plot of modeled sprint distance, velocity, acceleration and power as a function of time.
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#'
#' @returns A ggplot object
#'
#' @import ggplot2
#' @import patchwork
#'
#' @export
#'
#' @examples plot_sprint_motion_analysis(
#'   mean_velocity_splits = c(0, 5.77, 9.99),
#'   time_splits = c(0, 1.88, 2.88),
#'   distance = c(0, 10, 20),
#'   reaction_time = 0.146,
#'   maximal_velocity = 12.34
#' )
#'
#'
plot_sprint_motion_analysis <- function(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = NA, dt = 0.01) {
  dist_plot <- plot_sprint_distance(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = maximal_velocity)
  velocity_plot <- plot_sprint_velocity(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = maximal_velocity)
  acceleration_plot <- plot_sprint_acceleration(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = maximal_velocity)
  power_plot <- plot_sprint_metabolic_power(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = maximal_velocity)

  combined_plot <- (dist_plot + velocity_plot) / (acceleration_plot + power_plot)

  return(combined_plot)
}




#' Sex comparison velocity plot
#'
#' Generates plots to compare velocity as a function of time
#' for both male and females as presented in Briand et al. (2025), using
#' sprint data from Graubner and Nixdorf (2011).
#'
#' @param sprint_data_male A data frame with sprint motion model data extracted from males data
#' @param sprint_data_female A data frame with sprint motion model data extracted from females data
#' @param observed_data_male A data frame with columns "splits", "distance", "velocity" and "reaction_time" as in \code{\link{graubner_nixdorf_sprints}} data set
#' @param observed_data_female A data frame with columns "splits", "distance", "velocity" and "reaction_time" as in \code{\link{graubner_nixdorf_sprints}} data set
#' @param color A string with the color of the plot's line and points. Default is set at "darkgreen"
#'
#' @returns A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#'
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints  |>
#'   dplyr::filter(event == "Men's 100 m")
#'
#' women_100 <- graubner_nixdorf_sprints  |>
#'   dplyr::filter(event == "Women's 100 m")
#'
#' # Get the sprint motion data for both men and women
#'
#' sprint_data_men_100 <- sprint_motion_model_data(
#'   mean_velocity_splits = men_100$velocity,
#'   time_splits = men_100$splits,
#'   distance = men_100$distance,
#'   reaction_time = men_100$reaction_time[1],
#'   maximal_velocity = men_100$maximal_velocity[1]
#' )
#'
#' sprint_data_women_100 <- sprint_motion_model_data(
#'   mean_velocity_splits = women_100$velocity,
#'   time_splits = women_100$splits,
#'   distance = women_100$distance,
#'   reaction_time = women_100$reaction_time[1],
#'   maximal_velocity = women_100$maximal_velocity[1]
#' )
#'
#'
#' #Generate the plots
#'
#' sex_comparison_plot_velocity(
#'   sprint_data_male = sprint_data_men_100,
#'   sprint_data_female = sprint_data_women_100,
#'   observed_data_male = men_100,
#'   observed_data_female = women_100,
#'   color = "darkgreen") + ggplot2::ggtitle("Sex Comparison of the 100 m Instantaneous Velocity")

sex_comparison_plot_velocity <- function(sprint_data_male, sprint_data_female, observed_data_male, observed_data_female, color = "darkgreen" ){

  # modify the time of the male and female observed data

  # male reaction time
  rt_m <- observed_data_male$reaction_time[1]


  observed_data_male <- observed_data_male |>
    dplyr::mutate(time = find_time_velocity(splits, rt_m) - rt_m)

  # female reaction time
  rt_f <- observed_data_female$reaction_time[1]

  observed_data_female <-  observed_data_female |>
    dplyr::mutate(time = find_time_velocity(splits, rt_f) - rt_f)

  # Velocity plot
  plot <- ggplot2::ggplot() +
    # Male predictions
    ggplot2::geom_line(data = sprint_data_male,
              aes(x = time, y = velocity),
              color = color, linetype = "solid") +
    # Female predictions
    ggplot2::geom_line(data = sprint_data_female,
              aes(x = time, y = velocity),
              color = color, linetype = "dashed") +
    # Male observed data
    ggplot2::geom_point(data = observed_data_male,
               aes(x = time, y = velocity),
               shape = 16, size = 2, color = color) +
    # Female observed data
    ggplot2::geom_point(data = observed_data_female,
               aes(x = time, y = velocity),
               shape = 17, size = 2, color = color)+
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Time (s)", y = "Velocity (m/s)") +
    ggplot2::scale_y_continuous(limits = c(0, max(observed_data_male$velocity + 0.2))) +
    ggplot2::theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))

  return(plot)
}

#' Sex comparison metabolic power plot
#'
#' Generates plots to compare metabolic power as a function of time
#' for both male and females as presented in Briand et al. (2025), using
#' sprint data from Graubner and Nixdorf (2011).
#'
#' @param sprint_data_male A data frame with sprint motion model data extracted from males data
#' @param sprint_data_female A data frame with sprint motion model data extracted from females data
#' @param color A string with the color of the plot's line and points. Default is set at "darkgreen"
#' @returns A ggplot object
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' # Extract the data for the 100 m
#' men_100 <- graubner_nixdorf_sprints  |>
#'   dplyr::filter(event == "Men's 100 m")
#'
#' women_100 <- graubner_nixdorf_sprints  |>
#'   dplyr::filter(event == "Women's 100 m")
#'
#' # Get the sprint motion data for both men and women
#'
#' sprint_data_men_100 <- sprint_motion_model_data(
#'   mean_velocity_splits = men_100$velocity,
#'   time_splits = men_100$splits,
#'   distance = men_100$distance,
#'   reaction_time = men_100$reaction_time[1],
#'   maximal_velocity = men_100$maximal_velocity[1]
#' )
#'
#' sprint_data_women_100 <- sprint_motion_model_data(
#'   mean_velocity_splits = women_100$velocity,
#'   time_splits = women_100$splits,
#'   distance = women_100$distance,
#'   reaction_time = women_100$reaction_time[1],
#'   maximal_velocity = women_100$maximal_velocity[1]
#' )
#'
#' # Generate the plot
#'
#' sex_comparison_plot_power(
#' sprint_data_male = sprint_data_men_100,
#' sprint_data_female = sprint_data_women_100
#' ) + ggplot2::ggtitle("Sex Comparison of the 100 m Instantaneous Metabolic Power")


#'
sex_comparison_plot_power <- function(sprint_data_male, sprint_data_female, color = "darkgreen"){

  # Metabolic Power plot
  plot <- ggplot2::ggplot() +
    # Male predictions
    ggplot2::geom_line(data = sprint_data_male,
              aes(x = time, y = power),
              color = color, linetype = "solid", linewidth = 1) +  # Increased line width
    # Female predictions
    ggplot2::geom_line(data = sprint_data_female,
              aes(x = time, y = power),
              color = color, linetype = "dashed", linewidth = 1) +  # Increased line width
    ggplot2::labs(x = "Time (s)", y = "Power (W/kg)") +
    ggplot2::scale_y_continuous(limits = c(0, max(sprint_data_male$power + 10))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
      )

  return(plot)


}
