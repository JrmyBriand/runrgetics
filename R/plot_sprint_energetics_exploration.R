#' @importFrom utils globalVariables
utils::globalVariables(c("power_alactic_model", "power_lactic_model"))



#' Sprint Approximate Power Distribution Plot
#'
#' Plots the approximate power distribution of the three metabolic pathways contribution
#' to the total power during a sprint.
#'
#' @param sprint_approx_power_distribution A tibble with the following columns: time (s), power (W/kg), power_alactic (W/kg), power_lactic (W/kg), power_aerobic (W/kg)
#'
#' @returns A ggplot object representing the approximate power distribution of the three metabolic pathways contribution to the total power during a sprint.
#'
#' @import ggplot2
#'
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
#' sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
#'   maximal_aerobic_power = 24.5,
#'   basal_metabolic_rate = 1.2
#' )
#'
#'
#' plot_sprint_approx_power_dist(sprint_approx_power_distributions)
#'
plot_sprint_approx_power_dist <- function(sprint_approx_power_distribution) {
  plot <- ggplot2::ggplot(sprint_approx_power_distribution, aes(x = time)) +
    ggplot2::geom_line(aes(y = power, color = "Total Power"), linewidth = 1.2) +
    ggplot2::geom_line(aes(y = power_alactic, color = "Alactic Power"), linewidth = 1.2) +
    ggplot2::geom_line(aes(y = power_lactic, color = "Lactic Power"), linewidth = 1.2) +
    ggplot2::geom_line(aes(y = power_aerobic, color = "Aerobic Power"), linewidth = 1.2) +
    ggplot2::scale_color_manual(
      name = "Legend",
      values = c(
        "Total Power" = "#984EA3", # purple (colorblind friendly)
        "Alactic Power" = "#55A868", # green (colorblind friendly)
        "Lactic Power" = "#4C72B0", # blue (colorblind friendly)
        "Aerobic Power" = "#E76F51" # red-orange (colorblind friendly)
      ),
      breaks = c("Total Power", "Alactic Power", "Lactic Power", "Aerobic Power")
    ) +
    ggplot2::labs(
      x = "Time (s)",
      y = "Power (W/kg)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold")
    )




  return(plot)
}


#' Alactic Power Model on Approximate Power Distribution Plot
#'
#' Plots the alactic power model on the approximate power distribution of the anaerobic alactic contribution
#'
#' @param sprint_approx_power_distribution A tibble with the following columns: time (s), power_alactic (W/kg)
#'
#' @returns A ggplot object comparing the alactic power model on the approximate alactic power distribution during a sprint.
#'
#' @import ggplot2
#' @import dplyr
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
#' plot_sprint_approx_alactic_power_model(sprint_approx_power_distributions)
#'
plot_sprint_approx_alactic_power_model <- function(sprint_approx_power_distribution) {
  # fit the alactic power model on the data

  alactic_parameters <- fit_approx_alactic_power_params(sprint_approx_power_distribution)

  p_al_max <- alactic_parameters$pal_max
  mu_al <- alactic_parameters$mu
  sigma_al <- alactic_parameters$sigma

  # Generate a "modeled" column to the data

  model_data <- sprint_approx_power_distribution |>
    dplyr::mutate(
      power_alactic_model = sprint_approx_alactic_power_model(
        time = time,
        maximal_alactic_power = p_al_max,
        mu = mu_al,
        sigma = sigma_al
      )
    )

  # plot data and model

  plot <- ggplot2::ggplot(model_data, aes(x = time)) +
    ggplot2::geom_line(aes(y = power_alactic, color = "Approximate Alactic Power"), linewidth = 1.2) +
    ggplot2::geom_line(aes(y = power_alactic_model, color = "Alactic Power Model"), linewidth = 1.2) +
    ggplot2::scale_color_manual(
      name = "Legend",
      values = c(
        "Approximate Alactic Power" = "#4D4D4D",
        "Alactic Power Model" = "#55A868"
      ),
      breaks = c("Approximate Alactic Power", "Alactic Power Model")
    ) +
    ggplot2::labs(
      x = "Time (s)",
      y = "Power (W/kg)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold")
    )

  return(plot)
}


#' Lactic Power Model on Approximate Power Distribution Plot
#'
#' Plots the lactic power model on the approximate power distribution of the anaerobic lactic contribution
#'
#' @param sprint_approx_power_distribution A tibble with the following columns: time (s), power_lactic (W/kg)
#'
#' @returns A ggplot object comparing the lactic power model on the approximate lactic power distribution during a sprint.
#'
#' @import ggplot2
#' @import dplyr
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
#' plot_sprint_approx_lactic_power_model(sprint_approx_power_distributions)
#'
plot_sprint_approx_lactic_power_model <- function(sprint_approx_power_distribution) {
  # calculate parameters
  lactic_parameters <- fit_approx_lactic_power_params(sprint_approx_power_distribution)

  p_la_max <- lactic_parameters$p_la_max
  k1 <- lactic_parameters$k1
  k2 <- lactic_parameters$k2

  # Generate a "modeled" column to the data

  model_data <- sprint_approx_power_distribution |>
    dplyr::mutate(
      power_lactic_model = sprint_approx_lactic_power_model(
        time = time,
        maximal_lactic_power = p_la_max,
        k1 = k1,
        k2 = k2
      )
    )

  # plot data and model

  plot <- ggplot2::ggplot(model_data, aes(x = time)) +
    ggplot2::geom_line(aes(y = power_lactic, color = "Approximate Lactic Power"), linewidth = 1.2) +
    ggplot2::geom_line(aes(y = power_lactic_model, color = "Lactic Power Model"), linewidth = 1.2) +
    ggplot2::scale_color_manual(
      name = "Legend",
      values = c(
        "Approximate Lactic Power" = "#7F7F7F",
        "Lactic Power Model" = "#4C72B0"
      ),
      breaks = c("Approximate Lactic Power", "Lactic Power Model")
    ) +
    ggplot2::labs(
      x = "Time (s)",
      y = "Power (W/kg)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.35),
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold")
    )

  return(plot)
}
