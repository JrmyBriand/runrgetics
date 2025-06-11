
utils::globalVariables(c("duration", "energy", "energy_lactic_model", "lactic_energy", "sex", "kindermann_lactate", "accumulated_lactate"))


#' Plot Sprint Lactic Energy vs Duration with Model Overlay
#'
#' Creates a ggplot visualizing observed lactic energy expenditure as a function of sprint duration,
#' overlaid with a model prediction curve based on fitted lactic energy duration model.
#'
#' @param lactic_energy_duration A data frame or tibble containing columns `duration` (in seconds)
#'   and `lactic_energy` (in J/kg), such output can be obtained through the use of Briand et al.' (2025) sprint bioenergetic model
#'   and of the `sprint_lactic_energy_duration` function .
#' @inheritParams sprint_lactic_capacity
#' @param linetype Line type used for the model curve (default = `"solid"`). Accepts any valid ggplot2 line type.
#' @param line_color Color of the model prediction line (default = `"#457B9D"`).
#' @param point_color Color of the observed data points (default = `"darkblue"`).
#' @param point_shape Shape of the observed data points (default = 16, i.e., solid circle).
#'
#' @return A `ggplot` object showing the observed lactic energy-duration data and the fitted model curve.
#'
#' @export
#'
#' @examples
#' # Example with sample sprint data
#'
#' data <- sprint_lactic_energy_duration_graubner_nixdorf()
#'
#' plot_sprint_lactic_duration(data)
#'


plot_sprint_lactic_duration <- function(lactic_energy_duration, t1 = 20, t2 = 1500, linetype = "solid", line_color = "#457B9D", point_color = "darkblue", point_shape = 16) {

  # Fit to get maximal lactic capacity

  lactic_capacity <- sprint_lactic_capacity(lactic_energy_duration, t1 = t1, t2 = t2)

  # Generate model data

  model_data <- tibble::tibble(
    duration = seq(0.01, 100, length.out = 600)
  )

  # compute power

  model_data <- model_data |>
    dplyr::mutate(
      energy_lactic_model = sprint_lactic_duration_model(
        duration = duration,
        lactic_capacity = lactic_capacity,
        t1 = t1,
        t2 = t2
      )
    )

  # Create the plot

  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = lactic_energy_duration, aes(x = duration, y = lactic_energy), shape = point_shape, color = point_color, size = 3) +
    ggplot2::geom_line(data = model_data, aes(x = duration, y = energy_lactic_model),linetype = linetype, color = line_color, linewidth = 1) +
    ggplot2::labs(
      x = "Duration (s)",
      y = "Energy (J/kg)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16)
    )

  return(plot)

}


#' Generate model predictions for the sprint lactic energy-duration model
#'
#' Computes the modeled lactic energy output across a sequence of durations,
#' based on the estimated lactic capacity from empirical data and bi-exponential lactic energy-duration model.
#'
#' @param sex_label A string label (e.g., "male", "Female") used for grouping the output.
#' @param max_duration Maximum duration for which to compute the model (default = 100 s).
#' @inheritParams sprint_lactic_capacity
#'
#' @return A tibble with `duration`, `energy`, and `sex` columns representing the predicted model values.
#' @export
#'
#' @examples
#' data <- sprint_lactic_energy_duration_graubner_nixdorf()
#' get_lactic_model_data(data, sex_label = "male")

get_lactic_model_data <- function(lactic_energy_duration, sex_label, t1 = 20, t2 = 1500, max_duration = 100) {

    lactic_capacity <- sprint_lactic_capacity(lactic_energy_duration, t1 = t1, t2 = t2)


  tibble::tibble(duration = seq(0.01, max_duration, length.out = 600)) |>
    dplyr::mutate(
     energy = sprint_lactic_duration_model(
        duration = duration,
        lactic_capacity = lactic_capacity,
        t1 = t1,
        t2 = t2
      ),
      sex = sex_label
    )
}


#' Plot lactic energy-duration model fits for male and female sprinters
#'
#' Provides observed lactic energy values and model predictions
#' for male and female athletes using the bi-exponential lactic energy-duration model.The function using the `graubner_nixdorf_sprints` data set
#' generates the same Figure as the one presented in Briand et al. 2025.
#'
#' @param data A data frame containing sprint lactic energy estimations (default: `graubner_nixdorf_sprints`).
#' @param line_color Color for model line (default: "#457B9D").
#' @param point_color Color for data points (default: "darkblue").
#' @inheritParams sprint_lactic_capacity
#' @inheritParams sprint_lactic_energy_duration_graubner_nixdorf
#'
#' @return A ggplot object with overlaid male and female model fits and lactic energy over different running durations.
#' @export
#'
#' @examples
#' plot_sprint_lactic_duration_briand_article()
plot_sprint_lactic_duration_briand_article <- function(
    data = graubner_nixdorf_sprints,
    t1 = 20,
    t2 = 1500,
    line_color = "#457B9D",
    point_color = "darkblue",
    mu = -0.4,
    sigma = 1,
    k1 = 2.75,
    k2 = 35,
    dt = 0.01,
    cost_running_flat = 3.8,
    slope_equation = "original"
) {

  # Separate male and female data
  male_data <- sprint_lactic_energy_duration_graubner_nixdorf(athlete_sex = "male",
                                                              mu = mu,
                                                              sigma = sigma,
                                                              k1 = k1,
                                                              k2 = k2,
                                                              dt = dt,
                                                              cost_running_flat = cost_running_flat,
                                                              slope_equation = slope_equation)

  female_data <- sprint_lactic_energy_duration_graubner_nixdorf(athlete_sex = "female",
                                                                mu = mu,
                                                                sigma = sigma,
                                                                k1 = k1,
                                                                k2 = k2,
                                                                dt = dt,
                                                                cost_running_flat = cost_running_flat,
                                                                slope_equation = slope_equation)

  male_data$sex <- "male"
  female_data$sex <- "female"

  # Model predictions
  male_model <- get_lactic_model_data(male_data, sex_label = "male", t1 = t1, t2 = t2)
  female_model <- get_lactic_model_data(female_data, sex_label = "female", t1 = t1, t2 = t2)

  # Combine data
  all_data <- dplyr::bind_rows(male_data, female_data)
  all_model <- dplyr::bind_rows(male_model, female_model)

  # Aesthetic mappings
  shape_map <- c("male" = 16, "female" = 17)
  linetype_map <- c("male" = "solid", "female" = "dashed")

  # Plot
  ggplot2::ggplot(all_data, ggplot2::aes(x = duration, y = lactic_energy)) +
    ggplot2::geom_point(ggplot2::aes(shape = sex), color = point_color, size = 3) +
    ggplot2::geom_line(
      data = all_model,
      ggplot2::aes(x = duration, y = energy, linetype = sex),
      color = line_color,
      linewidth = 1
    ) +
    ggplot2::scale_shape_manual(values = shape_map) +
    ggplot2::scale_linetype_manual(values = linetype_map) +
    ggplot2::labs(
      x = "Duration (s)",
      y = "Energy (J/kg)",
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



#' Plot lactic energy-duration model fits on Kindermann's (1977) accumulated blood lactate data
#'
#' Provides observed lactic energy values and model predictions
#' for male and female athletes using the bi-exponential lactic energy-duration model. Lactic energy
#' is estimated using the conversion function `convert_acc_lactate_to_lactic_energy`. The accumulated blood lactate values are
#' provided in the `kindermann_lactate` data set. The function
#' generates a similar Figure as the one presented in Briand et al. 2025.
#'
#' @param data A data frame containing `accumulated_lactate` in mmol/L associated with different running `duration` in s (default: `kindermann_lactate`).
#' @param line_color Color for model line (default: "#457B9D").
#' @param point_color Color for data points (default: "darkblue").
#' @inheritParams sprint_lactic_capacity
#'
#' @return A ggplot object with overlaid male and female model fits and lactic energy over different running durations.
#' @export
#'
#' @examples
#' plot_sprint_lactic_duration_kindermann()

plot_sprint_lactic_duration_kindermann <- function(
    data = kindermann_lactate,
    t1 = 20,
    t2 = 1500,
    line_color = "#457B9D",
    point_color = "darkblue"
) {


  # Separate male and female data
  male_data <- data |>
    dplyr::mutate(lactic_energy = convert_acc_lactate_to_lactic_energy(accumulated_lactate, sex = "male"))

  female_data <- data |>
    dplyr::mutate(lactic_energy = convert_acc_lactate_to_lactic_energy(accumulated_lactate, sex = "female"))

  male_data$sex <- "male"
  female_data$sex <- "female"

  # Model predictions
  male_model <- get_lactic_model_data(male_data, sex_label = "male", t1 = t1, t2 = t2, max_duration = max(data$duration))
  female_model <- get_lactic_model_data(female_data, sex_label = "female", t1 = t1, t2 = t2, max_duration = max(data$duration))

  # Combine data
  all_data <- dplyr::bind_rows(male_data, female_data)
  all_model <- dplyr::bind_rows(male_model, female_model)

  # Aesthetic mappings
  shape_map <- c("male" = 16, "female" = 17)
  linetype_map <- c("male" = "solid", "female" = "dashed")

  # Plot
  ggplot2::ggplot(all_data, ggplot2::aes(x = duration, y = lactic_energy)) +
    ggplot2::geom_point(ggplot2::aes(shape = sex), color = point_color, size = 3) +
    ggplot2::geom_line(
      data = all_model,
      ggplot2::aes(x = duration, y = energy, linetype = sex),
      color = line_color,
      linewidth = 1
    ) +
    ggplot2::scale_shape_manual(values = shape_map) +
    ggplot2::scale_linetype_manual(values = linetype_map) +
    ggplot2::labs(
      x = "Duration (s)",
      y = "Energy (J/kg)",
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


