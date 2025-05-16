utils::globalVariables(c("power_mod"))

#' Plot Sprint Bioenergetic Model Power
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @param maximal_aerobic_power A double representing the maximal aerobic power (W/kg) of the athlete. Default is 24.5 W/kg.
#' @param scale_max_power A double representing the maximum y-axis value for the plot. Default is 160 W/kg.
#'
#' @returns A ggplot object comparing the modeled metabolic power from the sprint bioenergetic model to the observed metabolic power from the sprint motion data.
#' @export
#'
#' @examples
#'
#' #' # Extract Bolt's 100 m data from Graubner and Nixdorf data set.
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
#' plot_sprint_bioenergetic_model(
#' sprint_motion_data = bolt_100m_motion_data
#' )
#'
#'
#'
plot_sprint_bioenergetic_model <- function(sprint_motion_data, maximal_aerobic_power = 24.5, scale_max_power = 160){


  # compute modeled data from the three energy pathways

  modeled_data <- sprint_bioenergetic_model_data(sprint_motion_data, maximal_aerobic_power)

  plot <- ggplot2::ggplot(modeled_data, aes(x = time)) +
    # Add model components
    ggplot2::geom_line(aes(y = power_aerobic, color = "Aerobic Power"), linewidth = 1) +
    ggplot2::geom_line(aes(y = power_alactic, color = "Alactic Power"), linewidth = 1) +
    ggplot2::geom_line(aes(y = power_lactic, color = "Lactic Power"), linewidth= 1) +
    ggplot2::geom_line(aes(y = power_mod, color = "Modeled Metabolic Power"), linewidth = 1) +
    ggplot2::geom_line(aes(y = power, color = "Observed Metabolic Power"), alpha = 0.6, linewidth = 1) +
    # Customize the plot
    ggplot2::labs(
      x = "Time (s)",
      y = "Power (W/kg)",
      color = "Legend"
    ) +
    # Set colors and order
    ggplot2::scale_color_manual(
      values = c(
        "Aerobic Power" = "red",
        "Alactic Power" = "purple",
        "Lactic Power" = "blue",
        "Modeled Metabolic Power" = "green",
        "Observed Metabolic Power" = "black"
      ),
      breaks = c(
        "Aerobic Power",
        "Alactic Power",
        "Lactic Power",
        "Modeled Metabolic Power",
        "Observed Metabolic Power"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.75)
    )+
    ggplot2::scale_y_continuous(limits = c(0, scale_max_power))

  return(plot)

}

