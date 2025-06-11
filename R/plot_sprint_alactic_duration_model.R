
utils::globalVariables(c("duration", "alactic_power", "sex"))

#' Plot Sprint Alactic Power vs Duration with Model Overlay
#'
#' Creates a ggplot visualizing observed alactic power output as a function of sprint duration,
#' overlaid with a model prediction curve based on fitted alactic power duration model.
#'
#' @param alactic_power_duration A data frame or tibble containing columns `duration` (in seconds)
#'   and `alactic_power` (in W/kg), such output can be obtained through the use of Briand et al.' (2025) sprint bioenergetic model
#'   and of the `sprint_alactic_energy_duration` function .
#' @param mu_al A double representing the peak of the log-normal distribution. Default is 1.75.
#' @param sigma_al A double representing the decay of the log-normal distribution. Default is 1.5.
#' @param linetype Line type used for the model curve (default = `"solid"`). Accepts any valid ggplot2 line type.
#' @param line_color Color of the model prediction line (default = `"purple"`).
#' @param point_color Color of the observed data points (default = `"darkorchid4"`).
#' @param point_shape Shape of the observed data points (default = 16, i.e., solid circle).
#'
#' @return A `ggplot` object showing the observed alactic power-duration data and the fitted model curve.
#'
#' @export
#'
#' @examples
#' # Example with sample sprint data
#' data <- tibble::tibble(
#'   duration = c(2, 4, 6, 8),
#'   alactic_power = c(350, 320, 300, 290)
#' )
#' plot_sprint_alactic_duration(data)
#'
#' # Other example with graubner_nixdorf_sprints data
#'
#' data <- sprint_alactic_energy_duration_graubner_nixdorf()
#'
#' plot_sprint_alactic_duration(data)
#'


plot_sprint_alactic_duration <- function(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5, linetype = "solid", line_color = "purple", point_color = "darkorchid4", point_shape = 16) {

  # Fit to get maximal lactic capacity

  alactic_capacity <- sprint_alactic_capacity(alactic_power_duration, mu_al = mu_al, sigma_al = sigma_al)

  # Generate model data

  model_data <- tibble::tibble(
    duration = seq(0.01, max(alactic_power_duration$duration), length.out = 600)
  )

  # compute power

  model_data <- model_data |>
    dplyr::mutate(
      power_alactic_model = sprint_alactic_duration_model(
        duration = duration,
        alactic_capacity = alactic_capacity,
        mu_al = mu_al,
        sigma_al = sigma_al
      )
    )

  # Create the plot

  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = alactic_power_duration, aes(x = duration, y = alactic_power), shape = point_shape, color = point_color, size = 3) +
    ggplot2::geom_line(data = model_data, aes(x = duration, y = power_alactic_model),linetype = linetype, color = line_color, linewidth = 1) +
    ggplot2::labs(
      x = "Duration (s)",
      y = "Power (W/kg)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16)
    )

  return(plot)

}


#' Generate model predictions for the sprint alactic power-duration model
#'
#' Computes the modeled alactic power output across a sequence of durations,
#' based on the estimated alactic capacity from empirical data and log-normal decay alactic power duration model.
#'
#' @param sex_label A string label (e.g., "male", "Female") used for grouping the output.
#' @inheritParams sprint_alactic_capacity
#'
#' @return A tibble with `duration`, `power`, and `sex` columns representing the predicted model values.
#' @export
#'
#' @examples
#' data <- sprint_alactic_energy_duration_graubner_nixdorf()
#' get_alactic_model_data(data, sex_label = "male")
get_alactic_model_data <- function(alactic_power_duration, sex_label, mu_al = 1.75, sigma_al = 1.5) {
  alactic_capacity <- sprint_alactic_capacity(alactic_power_duration, mu_al = mu_al, sigma_al = sigma_al)

  tibble::tibble(duration = seq(0.01, max(alactic_power_duration$duration), length.out = 600)) |>
    dplyr::mutate(
      power = sprint_alactic_duration_model(
        duration = duration,
        alactic_capacity = alactic_capacity,
        mu_al = mu_al,
        sigma_al = sigma_al
      ),
      sex = sex_label
    )
}


#' Plot alactic power-duration model fits for male and female sprinters
#'
#' Provides observed alactic power values and model predictions
#' for male and female athletes using the log-normal decay model.The function using the graubner_nixdorf_sprints data set
#' generates the same Figure as the one presented in Briand et al. 2025.
#'
#' @param data A data frame containing sprint alactic power estimations (default: `graubner_nixdorf_sprints`).
#' @param mu_al A double representing the peak of the log-normal distribution. Default is 1.75.
#' @param sigma_al A double representing the decay of the log-normal distribution. Default is 1.5.
#' @param line_color Color for model line (default: "purple").
#' @param point_color Color for data points (default: "darkorchid4").
#'
#' @return A ggplot object with overlaid male and female model fits and alactic power over different running durations.
#' @export
#'
#' @examples
#' plot_sprint_alactic_duration_briand_article()
plot_sprint_alactic_duration_briand_article <- function(
    data = graubner_nixdorf_sprints,
    mu_al = 1.75,
    sigma_al = 1.5,
    line_color = "purple",
    point_color = "darkorchid4"
) {

  # Separate male and female data
  male_data <- sprint_alactic_energy_duration_graubner_nixdorf(athlete_sex = "male")
  female_data <- sprint_alactic_energy_duration_graubner_nixdorf(athlete_sex = "female")

  male_data$sex <- "male"
  female_data$sex <- "female"

  # Model predictions
  male_model <- get_alactic_model_data(male_data, sex_label = "male", mu_al = mu_al, sigma_al = sigma_al)
  female_model <- get_alactic_model_data(female_data, sex_label = "female", mu_al = mu_al, sigma_al = sigma_al)

  # Combine data
  all_data <- dplyr::bind_rows(male_data, female_data)
  all_model <- dplyr::bind_rows(male_model, female_model)

  # Aesthetic mappings
  shape_map <- c("male" = 16, "female" = 17)
  linetype_map <- c("male" = "solid", "female" = "dashed")

  # Plot
  ggplot2::ggplot(all_data, ggplot2::aes(x = duration, y = alactic_power)) +
    ggplot2::geom_point(ggplot2::aes(shape = sex), color = point_color, size = 3) +
    ggplot2::geom_line(
      data = all_model,
      ggplot2::aes(x = duration, y = power, linetype = sex),
      color = line_color,
      linewidth = 1
    ) +
    ggplot2::scale_shape_manual(values = shape_map) +
    ggplot2::scale_linetype_manual(values = linetype_map) +
    ggplot2::labs(
      x = "Duration (s)",
      y = "Power (W/kg)",
      shape = "Sex",
      linetype = "Sex"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14)
    )
}





