#' Sprint Bioenergetic Model Residual Standard Error
#'
#' Computes the residual standard error of the sprint bioenergetic model non-linear least squares fit. The fit is based on sprint metabolic power data derived from
#' available time splits, using the approach proposed by di Prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @inheritParams sprint_bioenergetic_model_data
#'
#' @returns A double with the residual standard error of the model fit.
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
#' sprint_bioenergetic_model_rse(bolt_100m_motion_data)
#'
sprint_bioenergetic_model_rse <- function(sprint_motion_data,
                                          mu = -0.4,
                                          sigma = 1,
                                          k1 = 2.75,
                                          k2 = 35,
                                          maximal_aerobic_power = 24.5) {

  model <- sprint_bioenergetic_model_fit(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)

  rse <- summary(model)$sigma

  return(rse)
}

#' Sprint Bioenergetic Model Root Mean Squared Error
#'
#' Computes the root mean squared error of the sprint bioenergetic model non-linear least squares fit. The fit is based on sprint metabolic power data derived from
#' available time splits, using the approach proposed by di Prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @inheritParams sprint_bioenergetic_model_data
#'
#' @returns A double with the root mean squared error of the model fit.
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
#' sprint_bioenergetic_model_rmse(bolt_100m_motion_data)
#'
sprint_bioenergetic_model_rmse <- function(sprint_motion_data, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, maximal_aerobic_power = 24.5) {
  model <- sprint_bioenergetic_model_fit(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)

  residuals_vals <- stats::resid(model)

  rmse <- sqrt(mean(residuals_vals^2))

  return(rmse)
}

#' Sprint Bioenergetic Model R-squared
#'
#' Computes the R-squared of the sprint bioenergetic model non-linear least squares fit. The fit is based on sprint metabolic power data derived from
#' available time splits, using the approach proposed by di Prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @inheritParams sprint_bioenergetic_model_data
#'
#' @returns A double with the R-squared of the model fit.
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
#' sprint_bioenergetic_model_R2(bolt_100m_motion_data)
#'
sprint_bioenergetic_model_R2 <- function(sprint_motion_data, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, maximal_aerobic_power = 24.5) {
  model <- sprint_bioenergetic_model_fit(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)

  fitted_vals <- stats::fitted(model)


  ss_res <- sum(residuals(model)^2)
  ss_tot <- ss_tot <- sum((fitted_vals - mean(fitted_vals))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  r_squared

  return(r_squared)
}


#' Sprint Bioenergetic Model Adjusted R-squared
#'
#' Computes the adjusted R-squared of the sprint bioenergetic model non-linear least squares fit. The fit is based on sprint metabolic power data derived from
#' available time splits, using the approach proposed by di Prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @inheritParams sprint_bioenergetic_model_data
#'
#' @return Adjusted R-squared of the sprint bioenergetic model.
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
#' sprint_bioenergetic_model_adj_R2(bolt_100m_motion_data)
#'
sprint_bioenergetic_model_adj_R2 <- function(sprint_motion_data, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, maximal_aerobic_power = 24.5) {
  model <- sprint_bioenergetic_model_fit(
    sprint_motion_data,
    mu = mu,
    sigma = sigma,
    k1 = k1,
    k2 = k2,
    maximal_aerobic_power = maximal_aerobic_power
  )

  fitted_vals <- stats::fitted(model)
  observed_vals <- sprint_motion_data$power

  ss_res <- sum((observed_vals - fitted_vals)^2)
  ss_tot <- sum((observed_vals - mean(observed_vals))^2)

  r_squared <- 1 - (ss_res / ss_tot)

  n <- length(observed_vals)
  p <- 5 # Number of model parameters: mu, sigma, k1, k2, MAP

  adj_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))

  return(adj_r_squared)
}





#' Sprint Bioenergetic Model Diagnostics
#'
#' Computes diagnostic metrics and plots for the sprint bioenergetic model non-linear least squares fit. The diagnostic is meant to vaidate if the non-linear least-squares fit assumptions are met and to contextualise
#' potential patterns in the residuals. The fit is based on sprint metabolic power data derived from
#' available time splits, using the approach proposed by di Prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#'
#' @param sprint_motion_data A tibble with the following columns: time (s), velocity (m/s), acceleration (m/s^2), distance (m), cost of running (J/kg/m) and power (W/kg).
#' @param histogram_binwidth A numeric value representing the binwidth for the histogram of residuals (default is 1)
#' @inheritParams sprint_bioenergetic_model_data
#'
#' @returns A list containing the RMSE, RSE, R-squared, adjusted R-squared, and a list of ggplot objects for each diagnostic plot (observed vs fitted power, residuals vs fitted power, Q-Q plots and histogram of residuals).
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
#' sprint_bioenergetic_model_diagnostic(bolt_100m_motion_data, maximal_aerobic_power = 24.5)
#'
sprint_bioenergetic_model_diagnostic <- function(sprint_motion_data, mu = -0.4, sigma = 1, k1 = 2.75, k2 = 35, maximal_aerobic_power = 24.5, histogram_binwidth = 1) {
  # Compute basic GOF metrics
  rse <- sprint_bioenergetic_model_rse(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)
  rmse <- sprint_bioenergetic_model_rmse(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)
  r_squared <- sprint_bioenergetic_model_R2(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)
  adj_r_squared <- sprint_bioenergetic_model_adj_R2(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)

  model <- sprint_bioenergetic_model_fit(sprint_motion_data, mu = mu, sigma = sigma, k1 = k1, k2 = k2, maximal_aerobic_power = maximal_aerobic_power)

  # Compute fitted and residuals
  fitted_vals <- stats::fitted(model)
  residuals_vals <- stats::resid(model)

  # Create dataframe for plotting
  df_diag <- tibble::tibble(
    time = sprint_motion_data$time,
    observed = sprint_motion_data$power,
    fitted = fitted_vals,
    residuals = residuals_vals
  )


  # Plot 1: Observed vs. Fitted
  p1 <- ggplot2::ggplot(df_diag, aes(x = time)) +
    ggplot2::geom_point(aes(y = observed), color = "blue", size = 2) +
    ggplot2::geom_line(aes(y = fitted), color = "red", linewidth = 1) +
    ggplot2::labs(
      title = "Observed vs. Fitted Power",
      x = "Time (s)", y = "Metabolic Power (W/kg)"
    ) +
    ggplot2::theme_minimal()

  # Plot 2: Residuals vs. Fitted
  p2 <- ggplot2::ggplot(df_diag, aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = "Residuals vs. Fitted Values",
      x = "Fitted Metabolic Power (W/kg)", y = "Residuals (W/kg)"
    ) +
    ggplot2::theme_minimal()

  # Plot 3: Q-Q Plot
  p3 <- ggplot2::ggplot(df_diag, aes(sample = residuals)) +
    ggplot2::stat_qq(size = 2) +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q Plot of Residuals") +
    ggplot2::theme_minimal()

  # Plot 4: Histogram of Residuals
  p4 <- ggplot2::ggplot(df_diag, aes(x = residuals)) +
    ggplot2::geom_histogram(binwidth = histogram_binwidth, fill = "steelblue", color = "white") +
    ggplot2::labs(title = "Histogram of Residuals", x = "Residuals (W/kg)", y = "Count") +
    ggplot2::theme_minimal()

  # Return everything
  return(list(
    RMSE = rmse,
    RSE = rse,
    R.squared = r_squared,
    adj.R.squared = adj_r_squared,
    plots = list(
      observed_vs_fitted = p1,
      residuals_vs_fitted = p2,
      qq_plot = p3,
      residual_hist = p4
    )
  ))
}
