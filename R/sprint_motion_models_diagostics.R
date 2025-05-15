#' @importFrom utils globalVariables
utils::globalVariables(c("observed_velocity", "fitted_velocity",
                        "residuals", "observed" ))

#' Sprint Acceleration Velocity Model (Non-Linear Least-Squares Approach)
#'
#' Fits a non-linear least-squares model to the sprint acceleration velocity data.
#'
#' @param time A vector of time points (in s)
#' @param velocity A vector of estimated instantaneous velocity associated to each time points (in m/s)
#' @param reaction_time A numeric value representing the reaction time (in s)
#'
#' @returns A fitted nlsLM model object.
#' @export
#'
#' @examples
#'
#' men_200 <- graubner_nixdorf_sprints |>
#'   dplyr::filter(event == "Men's 200 m")
#'
#' time <- find_time_velocity(men_200$splits, men_200$reaction_time[1])
#' velocity <- men_200$velocity
#' reaction_time <- men_200$reaction_time[1]
#'
#' # generate the model
#' sprint_velocity_least_squares_model(time = time, velocity = velocity, reaction_time = reaction_time)
#'
sprint_velocity_least_squares_model <- function(time, velocity, reaction_time) {
  model <- minpack.lm::nlsLM(
    velocity ~ sprint_acc_velocity_model(time, tau, vf, reaction_time = reaction_time),
    start = list(tau = 0.5, vf = 12),
    data = data.frame(time = time, velocity = velocity)
  )

  return(model)
}


#' Test the Assumptions of the Least Squares Acceleration Velocity Model
#'
#' Tests the assumptions of the least squares acceleration velocity model by plotting observed vs. fitted values,
#' residuals vs. fitted values, Q–Q plot of residuals, and histogram of residuals.
#'
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param sprint_total_distance A numeric value representing the distance (in m) (default is 100 m)
#' @param histogram_binwidth A numeric value representing the binwidth for the histogram of residuals (default is 0.1)
#'
#' @returns A list containing the RMSE, RSE, and a list of ggplot objects for each diagnostic plot.
#' @export
#'
#' @examples
#'
#' men_200 <- graubner_nixdorf_sprints |>
#'   dplyr::filter(event == "Men's 200 m")
#'
#' diagnose_velocity_model_acc(
#' mean_velocity_splits = men_200$velocity,
#' time_splits = men_200$splits,
#' distance = men_200$distance,
#' reaction_time = men_200$reaction_time[1],
#' sprint_total_distance = 200
#'  )
#'
#'
diagnose_velocity_model_acc <- function(mean_velocity_splits, time_splits, distance, reaction_time ,sprint_total_distance = 100, histogram_binwidth = 0.1) {

  dat <- tibble::tibble(
    distance = distance,
    velocity = mean_velocity_splits,
    splits = time_splits
  )

  if(sprint_total_distance > 200 ) {

    dat <- dat |>
      dplyr::filter(distance <= 200)
  }


  time <- find_time_velocity(dat$splits, reaction_time)


  model <- sprint_velocity_least_squares_model(time, dat$velocity, reaction_time)

  # Compute fitted and residuals
  fitted_vals <- stats::fitted(model)
  residuals_vals <- stats::resid(model)

  # Create dataframe for plotting
  df_diag <- tibble::tibble(
    time = time,
    observed = dat$velocity,
    fitted = fitted_vals,
    residuals = residuals_vals
  )

  # Compute metrics
  rmse <- sqrt(mean(residuals_vals^2))
  rse <- summary(model)$sigma

  # Plot 1: Observed vs. Fitted
  p1 <- ggplot2::ggplot(df_diag, aes(x = time)) +
    ggplot2::geom_point(aes(y = observed), color = "blue", size = 2) +
    ggplot2::geom_line(aes(y = fitted), color = "red", linewidth = 1) +
    ggplot2::labs(
      title = "Observed vs. Fitted Velocity",
      x = "Time (s)", y = "Velocity (m/s)"
    ) +
    ggplot2::theme_minimal()

  # Plot 2: Residuals vs. Fitted
  p2 <- ggplot2::ggplot(df_diag, aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = "Residuals vs. Fitted Values",
      x = "Fitted Velocity (m/s)", y = "Residuals (m/s)"
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
    ggplot2::labs(title = "Histogram of Residuals", x = "Residuals (m/s)", y = "Count") +
    ggplot2::theme_minimal()

  # Return everything
  return(list(
    RMSE = rmse,
    RSE = rse,
    plots = list(
      observed_vs_fitted = p1,
      residuals_vs_fitted = p2,
      qq_plot = p3,
      residual_hist = p4
    )
  ))
}


#' Test the Assumptions of the Sprint Motion Model Fit
#'
#' @param mean_velocity_splits A vector with mean velocity splits over each distance interval (m/s)
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#' @param maximal_velocity A double representing the maximal velocity (in m/s)
#' @param dt Time step of the model. Default is set at 0.01 s
#'
#' @returns A list containing the RMSE, RSE, a tibble with observed vs fitted velocities and a list of ggplot objects for each diagnostic plot.
#' @export
#'
#' @examples
#'
#' men_200 <- graubner_nixdorf_sprints |>
#'   dplyr::filter(event == "Men's 200 m")
#'
#' diagnose_sprint_model(
#' mean_velocity_splits = men_200$velocity,
#' time_splits = men_200$splits,
#' distance = men_200$distance,
#' reaction_time = men_200$reaction_time[1],
#' maximal_velocity =  men_200$maximal_velocity[1]
#'  )
#'
#'
diagnose_sprint_model <- function(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity = NA, dt = 0.01) {
  # Run your model
  model_df <- sprint_motion_model_data(mean_velocity_splits, time_splits, distance, reaction_time, maximal_velocity, dt)

  # Get modeled velocities at the original split time points (not interpolated)
  times_velocity <- find_time_velocity(time_splits, reaction_time)
  fit_df <- tibble::tibble(
    time = times_velocity - reaction_time
  )

  # Get fitted velocity at those exact times
  fit_df$fitted_velocity <- stats::approx(x = model_df$time, y = model_df$velocity, xout = fit_df$time)$y

  # Calculate residuals
  fit_df$observed_velocity <- mean_velocity_splits
  fit_df$residuals <- fit_df$observed_velocity - fit_df$fitted_velocity

  # RMSE
  rmse <- sqrt(mean(fit_df$residuals^2))

  # RSE = sqrt(SSE / df)
  sse <- sum(fit_df$residuals^2)
  df <- length(fit_df$residuals) - 2 # assuming 2 fitted parameters (tau, vmax)
  rse <- sqrt(sse / df)

  # === PLOTS ===

  p1 <- ggplot2::ggplot(fit_df, aes(x = observed_velocity, y = fitted_velocity)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_point(size = 3) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Observed vs. Fitted Velocity", x = "Observed", y = "Fitted") +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(fit_df, aes(x = fitted_velocity, y = residuals)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(title = "Residuals vs. Fitted Values", x = "Fitted Velocity", y = "Residuals") +
    ggplot2::theme_minimal()

  p3 <- ggplot2::ggplot(fit_df, aes(sample = residuals)) +
    ggplot2::stat_qq(size = 2) +
    ggplot2::stat_qq_line(linetype = "dashed", color = "grey") +
    ggplot2::labs(title = "Q-Q Plot of Residuals") +
    ggplot2::theme_minimal()

  # Return a list with results and plots
  return(list(
    residuals_df = fit_df,
    RMSE = rmse,
    RSE = rse,
    plots = list(
      observed_vs_fitted = p1,
      residuals_vs_fitted = p2,
      qq_plot = p3
    )
  ))
}
