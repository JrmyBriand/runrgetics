#' @importFrom utils globalVariables
utils::globalVariables(c(
  "time", "metabolic_power", "external_power", "external_mechanical_efficiency",
  "acceleration", "velocity", "heartrate"
))

#' Plot metabolic running power over time
#'
#' @param running_power_data_frame A data frame with `time` (s) and
#'   `metabolic_power` (W/kg) columns, e.g. from [compute_metabolic_running_power()].
#' @param line_color Colour of the power line.
#'
#' @returns A ggplot object of metabolic power versus time.
#' @export
#'
#' @examples
#' run <- compute_metabolic_running_power(
#'   tibble::tibble(time = 0:9,
#'                  velocity = c(0, 2, 4, 6, 7, 7.5, 7.6, 7.6, 7.5, 7.4)))
#' plot_running_metabolic_power(run)
plot_running_metabolic_power <- function(running_power_data_frame, line_color = "darkblue") {
  # Plot the running power data
  ggplot2::ggplot(running_power_data_frame) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = metabolic_power), color = line_color, size = 1.2) +
    ggplot2::labs(
      title = "Running Metabolic Power Over Time",
      x = "Time (s)",
      y = "Metabolic Power (W/kg)"
    ) +
    ggplot2::theme_minimal()
}




#' Plot metabolic running power from an activity file
#'
#' @param file_path Path to the activity file.
#' @param file_type Either `"fit"` (default) or `"csv"`.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#' @param line_color Colour of the power line.
#'
#' @returns A ggplot object of metabolic power versus time.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_activity_metabolic_power(f)
#' }
plot_activity_metabolic_power <- function(file_path,
                                          file_type = "fit",
                                          cost_running_flat = 3.6,
                                          slope_equation = "extended",
                                          smoothing = "none",
                                          window = 3,
                                          order = 2,
                                          cfreq = 0.1,
                                          line_color = "darkblue"){

  # compute metabolic power data

  metabolic_power <- compute_activity_metabolic_running_power(file_path = file_path,
                                                              file_type = file_type,
                                                              cost_running_flat = cost_running_flat,
                                                              slope_equation = slope_equation,
                                                              smoothing = smoothing,
                                                              window = window,
                                                              order = order,
                                                              cfreq = cfreq)

  # plot the metabolic power

  plot <- plot_running_metabolic_power(metabolic_power, line_color = line_color)

  return(plot)

}


#' Plot external running power over time
#'
#' @param external_power_data_frame A data frame with `time` (s) and
#'   `external_power` (W/kg) columns, e.g. from [compute_external_running_power()].
#' @param line_color Colour of the power line.
#'
#' @returns A ggplot object of external power versus time.
#' @export
#'
#' @examples
#' run <- compute_external_running_power(
#'   tibble::tibble(time = 0:9,
#'                  velocity = c(0, 2, 4, 6, 7, 7.5, 7.6, 7.6, 7.5, 7.4)))
#' plot_running_external_power(run)
plot_running_external_power <- function(external_power_data_frame, line_color = "darkred"){

  # Plot the running power data
  ggplot2::ggplot(external_power_data_frame) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = external_power), size = 1.2, color = line_color) +
    ggplot2::labs(
      title = "Running External Power Over Time",
      x = "Time (s)",
      y = "External Power (W/kg)"
    ) +
    ggplot2::theme_minimal()


}


#' Plot external mechanical efficiency over time
#'
#' @param power_data A data frame with `time` (s) and `external_mechanical_efficiency`
#'   columns, e.g. from [compute_external_mechanical_efficiency()].
#' @param line_color Colour of the efficiency line.
#'
#' @returns A ggplot object of external mechanical efficiency versus time.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_external_mechanical_efficiency(compute_activity_power_efficiency_data(f))
#' }
plot_external_mechanical_efficiency <- function(power_data, line_color = "darkgreen"){

  # Plot the running power data
  ggplot2::ggplot(power_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = external_mechanical_efficiency), size = 1.2, color = line_color) +
    ggplot2::labs(
      title = "External Mechanical Efficiency",
      x = "Time (s)",
      y = "Efficiency"
    ) +
    ggplot2::theme_minimal()



}


#' Plot metabolic and external power with efficiency
#'
#' @param power_data A data frame with `time`, `metabolic_power`, `external_power`
#'   and `external_mechanical_efficiency` columns, e.g. from
#'   [compute_external_mechanical_efficiency()].
#' @param color_metabolic Colour of the metabolic-power line.
#' @param color_external Colour of the external-power line.
#' @param color_efficiency Colour of the efficiency line.
#'
#' @returns A patchwork of the power and efficiency plots.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_metabolic_external_power_comparison(compute_activity_power_efficiency_data(f))
#' }
plot_metabolic_external_power_comparison <- function(power_data,
                                                     color_metabolic = "darkblue" ,
                                                     color_external = "darkred",
                                                     color_efficiency = "darkgreen"){


  # power plot

  power <- ggplot2::ggplot(power_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = metabolic_power), color = color_metabolic, size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = external_power), color = color_external, size = 1.2) +
    ggplot2::labs(
      title = "Power (Metabolic and External)",
      x = "Time (s)",
      y = "Power (W/kg)"
    ) +
    ggplot2::theme_minimal()


  #efficiency plot

  eff <-  ggplot2::ggplot(power_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = external_mechanical_efficiency), size = 1.2, color = color_efficiency) +
    ggplot2::labs(
      title = "External Mechanical Efficiency",
      x = "Time (s)",
      y = "Efficiency"
    ) +
    ggplot2::theme_minimal()


  # combine plots

  combined_plot <- (power) / (eff)

  return(combined_plot)

}


#' Plot external running power from an activity file
#'
#' @param file_path Path to the activity file.
#' @param file_type Either `"fit"` (default) or `"csv"`.
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#' @param line_color Colour of the power line.
#'
#' @returns A ggplot object of external power versus time.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_activity_external_power(f)
#' }
plot_activity_external_power <- function(file_path,
                                         file_type = "fit",
                                         smoothing = "none",
                                         window = 3,
                                         order = 2,
                                         cfreq = 0.1,
                                         line_color = "darkred"){

 external_power <- compute_activity_external_running_power(file_path = file_path,
                                                              file_type = file_type,
                                                              smoothing = smoothing,
                                                              window = window,
                                                              order = order,
                                                              cfreq = cfreq)

  # plot the metabolic power

  plot <- plot_running_external_power(external_power, line_color = line_color)


  return(plot)
}


#' Plot a combined power / efficiency analysis from an activity file
#'
#' @param file_path Path to the activity file.
#' @param file_type Either `"fit"` (default) or `"csv"`.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#' @param color_metabolic Colour of the metabolic-power line.
#' @param color_external Colour of the external-power line.
#' @param color_efficiency Colour of the efficiency line.
#'
#' @returns A patchwork of the power and efficiency plots.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_activity_power_analysis(f)
#' }
plot_activity_power_analysis <- function(file_path,
                                         file_type = "fit",
                                         cost_running_flat = 3.6,
                                         slope_equation = "extended",
                                         smoothing = "none",
                                         window = 3,
                                         order = 2,
                                         cfreq = 0.1,
                                         color_metabolic = "darkblue",
                                         color_external = "darkred",
                                         color_efficiency = "darkgreen"){


  power_efficiency_data <- compute_activity_power_efficiency_data(file_path = file_path,
                                                            file_type = file_type,
                                                            cost_running_flat = cost_running_flat,
                                                            slope_equation = slope_equation,
                                                            smoothing = smoothing,
                                                            window = window,
                                                            order = order,
                                                            cfreq = cfreq)

  plot <- plot_metabolic_external_power_comparison(power_efficiency_data,
                                                   color_metabolic = color_metabolic,
                                                   color_external = color_external,
                                                   color_efficiency = color_efficiency)

  return(plot)


}


#' Plot a full running analysis (power, motion, efficiency, heart rate)
#'
#' @param running_data A data frame with `time`, `metabolic_power`, `external_power`,
#'   `external_mechanical_efficiency`, `velocity`, `acceleration` and `heartrate`
#'   columns.
#' @param color_metabolic_power Colour of the metabolic-power line.
#' @param color_external_power Colour of the external-power line.
#' @param color_velocity Colour of the velocity line.
#' @param color_acceleration Colour of the acceleration line.
#' @param color_heart_rate Colour of the heart-rate line.
#' @param color_efficiency Colour of the efficiency line.
#'
#' @returns A patchwork of the power, motion, efficiency and heart-rate plots.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_running_analysis(compute_activity_power_efficiency_data(f))
#' }
plot_running_analysis <- function(running_data,
                                  color_metabolic_power = "darkblue",
                                  color_external_power = "darkred",
                                  color_velocity = "purple",
                                  color_acceleration = "darkorange",
                                  color_heart_rate = "red",
                                  color_efficiency = "darkgreen"){

  # power plot

  power <- ggplot2::ggplot(running_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = metabolic_power), color = color_metabolic_power, size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = external_power), color = color_external_power, size = 1.2) +
    ggplot2::labs(
      title = "Power (Metabolic and External)",
      x = "Time (s)",
      y = "Power (W/kg)"
    ) +
    ggplot2::theme_minimal()


  # velocity acceleration plot

  motion <- ggplot2::ggplot(running_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = acceleration), color = color_acceleration, size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = velocity), color = color_velocity, size = 1.2) +
    ggplot2::labs(
      title = "Motion (Velocity and Acceleration)",
      x = "Time (s)",
      y = "Velocity (m/s)/Acceleration (m/s2)"
    ) +
    ggplot2::theme_minimal()

  # efficiency plot

  efficiency <-   ggplot2::ggplot(running_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = external_mechanical_efficiency), size = 1.2, color = color_efficiency) +
    ggplot2::labs(
      title = "External Mechanical Efficiency",
      x = "Time (s)",
      y = "Efficiency"
    ) +
    ggplot2::theme_minimal()

  # heart rate plot

  heart_rate <- ggplot2::ggplot(running_data) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = heartrate), size = 1.2, color = color_heart_rate) +
    ggplot2::labs(
      title = "Heart Rate",
      x = "Time (s)",
      y = "BPM"
    ) +
    ggplot2::theme_minimal()


  combined_plot <- (power + motion)/(efficiency + heart_rate)

  return(combined_plot)

}



#' Plot a full running analysis from an activity file
#'
#' @param file_path Path to the activity file.
#' @param file_type Either `"fit"` (default) or `"csv"`.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#' @param color_metabolic_power Colour of the metabolic-power line.
#' @param color_external_power Colour of the external-power line.
#' @param color_velocity Colour of the velocity line.
#' @param color_acceleration Colour of the acceleration line.
#' @param color_heart_rate Colour of the heart-rate line.
#' @param color_efficiency Colour of the efficiency line.
#'
#' @returns A patchwork of the power, motion, efficiency and heart-rate plots.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) plot_activity_running_analysis(f)
#' }
plot_activity_running_analysis <- function(file_path,
                                           file_type = "fit",
                                           cost_running_flat = 3.6,
                                           slope_equation = "extended",
                                           smoothing = "none",
                                           window = 3,
                                           order = 2,
                                           cfreq = 0.1,
                                           color_metabolic_power = "darkblue",
                                           color_external_power = "darkred",
                                           color_velocity = "purple",
                                           color_acceleration = "darkorange",
                                           color_heart_rate = "red",
                                           color_efficiency = "darkgreen"){


  # get data

  running_data <- compute_activity_power_efficiency_data(file_path = file_path,
                                                         file_type = file_type,
                                                         cost_running_flat = cost_running_flat,
                                                         slope_equation = slope_equation,
                                                         smoothing = smoothing,
                                                         window = window,
                                                         order = order,
                                                         cfreq = cfreq)


  # draw the plot

  plot <- plot_running_analysis(running_data,
                                color_metabolic_power = color_metabolic_power,
                                color_external_power = color_external_power,
                                color_velocity = color_velocity,
                                color_acceleration = color_acceleration,
                                color_heart_rate = color_heart_rate,
                                color_efficiency = color_efficiency)

  return(plot)

}
