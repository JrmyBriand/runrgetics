#' @importFrom utils globalVariables
utils::globalVariables(c(
  "graubner_nixdorf_sprints",
  "event",
  "pal_max_optimal",
  "pal_max_formalized",
  "mu_optimal",
  "mu_formalized",
  "sigma_optimal",
  "sigma_formalized",
  "aic_optimal",
  "aic_formalized",
  "bic_optimal",
  "bic_formalized",
  "r_squared_optimal",
  "r_squared_formalized",
  "rse_optimal",
  "rse_formalized",
  "p_la_max_optimal",
  "p_la_max_formalized",
  "k1_optimal",
  "k1_formalized",
  "k2_optimal",
  "k2_formalized"
))


#' Optimal Parameters for the Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the optimal parameters for the alactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the optimal parameters for the alactic model: event (character), pal_max (W/kg), mu and sigma.
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_optimal_params()
#'
sprint_alactic_model_optimal_params <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table

  table <- tibble::tibble(
    event = character(),
    pal_max = numeric(),
    mu = numeric(),
    sigma = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    # Fit the log-normal distribution to the alactic power distribution

    params <- fit_approx_alactic_power_params(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      pal_max = params$pal_max,
      mu = params$mu,
      sigma = params$sigma
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  # return table

  return(table)
}


#' Table of Optimal Parameters for the Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' A formatted table of the computed optimal parameters for the alactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the optimal parameters for the alactic model: event (character), pal_max (W/kg), mu and sigma.
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_optimal_params_table()
#'
sprint_alactic_model_optimal_params_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_alactic_model_optimal_params(data)

  # round the columns

  table$pal_max <- round(table$pal_max, 2)
  table$mu <- round(table$mu, 2)
  table$sigma <- round(table$sigma, 2)

  # rename the columns (provide greek letter for mu and sigma)

  names(table)[names(table) == "mu"] <- "\u03bc <br> "
  names(table)[names(table) == "sigma"] <- "\u03c3 <br> "
  names(table)[names(table) == "pal_max"] <- "Maximal Alactic<br> Power (W/kg)"
  names(table)[names(table) == "event"] <- "Event <br> "


  return(tinytable::tt(table, notes = "Alactic optimal fitted parameters over different events"))
}


#' Parameters of formalized/simplified Log-normal Fit on Alactic Power Distribution
#'
#' Extracts the parameters of a simplified/formalized log-normal fit on the approximate alactic power distribution. The formalized model is a model where the mu and sigma parameters of the log-normal disctribution are set at -0.4 and 1 respectively.
#' The only fitted value is therefore the maximal alactic power. The set value correspond to a compromise of the observed fitted values over the different sprint events.
#'
#' @param sprint_approx_power_distribution A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_alactic (W/kg)
#' @param mu A double. Parameter setting the peak of the log-normal distribution.
#' @param sigma A double. Parameter setting the decay of the log-normal distribution.
#'
#' @returns A list with the following elements: pal_max, sigma, mu
#'
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
#' # Fit the log-normal distribution to the alactic power distribution
#'
#' fit_alactic_power_params(sprint_approx_power_distributions)
#'
fit_alactic_power_params <- function(sprint_approx_power_distribution, mu = -0.4, sigma = 1) {
  log_norm_dist <- minpack.lm::nlsLM(power_alactic ~ pal_max * exp(-(log(time) - mu)^2 / (2 * sigma^2)), data = sprint_approx_power_distribution, start = list(pal_max = 160))

  return(list(
    pal_max = coef(log_norm_dist)[1],
    sigma = sigma,
    mu = mu
  ))
}


#' Parameters for the Formalized/Simplified Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the optimal parameters for the formalized/simplified alactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m. The formalized model is a model where the mu and sigma parameters of the log-normal disctribution are set at -0.4 and 1 respectively.
#' The only fitted value is therefore the maximal alactic power. The set value correspond to a compromise of the observed fitted values over the different sprint events.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the parameters for the formalized/simplified alactic model: event (character), pal_max (W/kg), mu and sigma.
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_params()
#'
sprint_alactic_model_params <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table

  table <- tibble::tibble(
    event = character(),
    pal_max = numeric(),
    mu = numeric(),
    sigma = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    # Fit the log-normal distribution to the alactic power distribution

    params <- fit_alactic_power_params(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      pal_max = params$pal_max,
      mu = params$mu,
      sigma = params$sigma
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  # return table

  return(table)
}


#' Table of Parameters for the Formalized/Simplified Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' A formatted table of the computed optimal parameters for the formalized/simplified alactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m. The formalized model is a model where the mu and sigma parameters of the log-normal disctribution are set at -0.4 and 1 respectively.
#' The only fitted value is therefore the maximal alactic power. The set value correspond to a compromise of the observed fitted values over the different sprint events.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the optimal parameters for the alactic model: event (character), pal_max (W/kg), mu and sigma.
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_params_table()
#'
sprint_alactic_model_params_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_alactic_model_params(data)

  # round the columns

  table$pal_max <- round(table$pal_max, 2)
  table$mu <- round(table$mu, 2)
  table$sigma <- round(table$sigma, 2)

  # rename the columns (provide greek letter for mu and sigma)

  names(table)[names(table) == "mu"] <- "\u03bc <br> "
  names(table)[names(table) == "sigma"] <- "\u03c3 <br> "
  names(table)[names(table) == "pal_max"] <- "Maximal Alactic<br> Power (W/kg)"
  names(table)[names(table) == "event"] <- "Event <br> "


  return(tinytable::tt(table, notes = "Formalized Alactic Model fitted parameters over different sprint events"))
}





#' Goodness of Fit Metrics for the Optimal Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the goodness of fit metrics (AIC, BIC and R-squared) for the optmal alactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m. The optimal model is a model where the maximal alactic power as well as mu and sigma parameters of the log-normal distribution are fitted to the data.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the goodness of fit metrics for the optimal alactic model: event (character), AIC, BIC, residual squared error and R-squared.
#' @export
#'
#' @examples
#'
#'
#' sprint_alactic_model_optimal_gof_metrics()
sprint_alactic_model_optimal_gof_metrics <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table
  table <- tibble::tibble(
    event = character(),
    aic = numeric(),
    bic = numeric(),
    r_squared = numeric(),
    rse = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    gof_metrics <- fit_approx_alactic_gof_metrics(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      aic = gof_metrics$AIC,
      bic = gof_metrics$BIC,
      r_squared = gof_metrics$r_squared,
      rse = gof_metrics$residual_se
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  return(table)
}




#' Formatted Table of Goodness of Fit Metrics for the Optimal Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' Provides a formatted table of the goodness of fit metrics of the optimal alactic model on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the goodness of fit metrics for the optimale alactic model: event (character), AIC, BIC, residual squared error and R-squared.
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_optimal_gof_metrics_table()
#'
sprint_alactic_model_optimal_gof_metrics_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_alactic_model_optimal_gof_metrics(data)

  # round the columns

  table$aic <- round(table$aic)
  table$bic <- round(table$bic)
  table$r_squared <- round(table$r_squared, 2)
  table$rse <- round(table$rse, 2)

  # rename the columns (provide greek letter for mu and sigma)


  names(table)[names(table) == "event"] <- "Event <br> "
  names(table)[names(table) == "aic"] <- "AIC <br> "
  names(table)[names(table) == "bic"] <- "BIC <br> "
  names(table)[names(table) == "r_squared"] <- "R-squared <br> "
  names(table)[names(table) == "rse"] <- "RSE <br> "

  return(tinytable::tt(table, notes = "Optimal alactic model goodness of fit metrics over different events"))
}


#' Goodness of Fit (Gof) of Alactic Power Model (Sprint)
#'
#' Computes the goodness of fit metrics for the formalized sprint alactic power model (with mu set to -0.4, and sigma set to 1)
#'
#' @param data A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_alactic (W/kg)
#' @param mu A double. Parameter setting the peak of the log-normal distribution.
#' @param sigma A double. Parameter setting the decay of the log-normal
#'
#' @returns A list with the following elements: r_squared, residual_se, AIC, BIC
#'
#' @export
#'
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
#' # Fit the log-normal distribution to the alactic power distribution
#'
#' fit_alactic_gof_metrics(sprint_approx_power_distributions)
#'
fit_alactic_gof_metrics <- function(data, mu = -0.4, sigma = 1) {
  # run the model


  model <- minpack.lm::nlsLM(power_alactic ~ pal_max * exp(-(log(time) - mu)^2 / (2 * sigma^2)), data = data, start = list(pal_max = 160))


  # Extract residuals and fitted values
  residuals <- residuals(model)
  fitted <- fitted(model)
  observed <- data$power_alactic

  # Sum of squares
  ss_res <- sum(residuals^2)
  ss_tot <- sum((observed - mean(observed))^2)

  # Metrics
  r_squared <- 1 - (ss_res / ss_tot)
  residual_se <- sqrt(ss_res / df.residual(model))
  aic_val <- AIC(model)
  bic_val <- BIC(model)

  # Return as list
  return(list(
    r_squared = r_squared,
    residual_se = residual_se,
    AIC = aic_val,
    BIC = bic_val
  ))
}


#' Goodness of Fit Metrics for the formalised Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the goodness of fit metrics (AIC, BIC and R-squared) for the alactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the goodness of fit metrics for the formalized/simplified alactic model: event (character), AIC, BIC, residual squared error and R-squared.
#' @export
#'
#' @examples
#'
#'
#' sprint_alactic_model_gof_metrics()
#'
sprint_alactic_model_gof_metrics <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table
  table <- tibble::tibble(
    event = character(),
    aic = numeric(),
    bic = numeric(),
    r_squared = numeric(),
    rse = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    gof_metrics <- fit_alactic_gof_metrics(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      aic = gof_metrics$AIC,
      bic = gof_metrics$BIC,
      r_squared = gof_metrics$r_squared,
      rse = gof_metrics$residual_se
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  return(table)
}

#' Formatted Table of Goodness of Fit Metrics for the Formalized/Simplified Alactic Model (On Berlin 2009 Sprint Performances)
#'
#' Provides a formatted table of the goodness of fit metrics of the formalized/simplified alactic model on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the goodness of fit metrics for the optimale alactic model: event (character), AIC, BIC, residual squared error and R-squared.
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_gof_metrics_table()
#'
sprint_alactic_model_gof_metrics_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_alactic_model_gof_metrics(data)

  # round the columns

  table$aic <- round(table$aic)
  table$bic <- round(table$bic)
  table$r_squared <- round(table$r_squared, 2)
  table$rse <- round(table$rse, 2)

  # rename the columns (provide greek letter for mu and sigma)


  names(table)[names(table) == "event"] <- "Event <br> "
  names(table)[names(table) == "aic"] <- "AIC <br> "
  names(table)[names(table) == "bic"] <- "BIC <br> "
  names(table)[names(table) == "r_squared"] <- "R-squared <br> "
  names(table)[names(table) == "rse"] <- "RSE <br> "

  return(tinytable::tt(table, notes = "Formalized/simplified Alactic model goodness of fit metrics over different sprint events"))
}

#' Table of Comparison of the Optimal and Formalized/Simplified Alactic Model Parameters and Goodness of Fit Metrics (On Berlin 2009 Sprint Performances)
#'
#' Provides a table comparing the optimal and formalized/simplified alactic model fitted parameters and goodness of fit metrics over different events. The optimal model
#' is a model where the maximal alactic power as well as mu and sigma parameters of the log-normal distribution are fitted to the data.
#' The formalized model is a model where the mu and sigma parameters of the log-normal distribution are set at -0.4 and 1 respectively.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble comparing the optimal and formalized/simplified alactic model fitted parameters and goodness of fit metrics over different events: event (character), pal_max (W/kg), mu, sigma, AIC, BIC and R-squared.
#'
#' @export
#'
#' @examples
#'
#' sprint_alactic_model_gof_comp()
#'
sprint_alactic_model_gof_comp <- function(data = graubner_nixdorf_sprints) {
  # For each event, compare the parameters and the gof metrics of the optimal as well as the formalized/simplified version of the model.

  # get parameters and gof of the optimized model

  optimal_params <- sprint_alactic_model_optimal_params(data)
  optimal_gof <- sprint_alactic_model_optimal_gof_metrics(data)

  # get parameters and gof of the formalized/simplified model

  formalized_params <- sprint_alactic_model_params(data)
  formalized_gof <- sprint_alactic_model_gof_metrics(data)


  # merge the table and put the columns of each parameters side by side comparing optimal and formalized/simplified model

  table <- dplyr::left_join(optimal_params, formalized_params, by = "event", suffix = c("_optimal", "_formalized")) |>
    dplyr::left_join(optimal_gof, by = "event") |>
    dplyr::left_join(formalized_gof, by = "event", suffix = c("_optimal", "_formalized")) |>
    dplyr::select(
      event,
      pal_max_optimal,
      pal_max_formalized,
      mu_optimal,
      mu_formalized,
      sigma_optimal,
      sigma_formalized,
      aic_optimal,
      aic_formalized,
      bic_optimal,
      bic_formalized,
      r_squared_optimal,
      r_squared_formalized,
      rse_optimal,
      rse_formalized
    )

  return(table)
}

#' Formated Table of Comparison of the Optimal and Formalized/Simplified Alactic Model Parameters and Goodness of Fit Metrics (On Berlin 2009 Sprint Performances)
#'
#' Provides a formatted table comparing the optimal and formalized/simplified alactic model fitted parameters and goodness of fit metrics over different events. The optimal model
#' is a model where the maximal alactic power as well as mu and sigma parameters of the log-normal distribution are fitted to the data.
#' The formalized model is a model where the mu and sigma parameters of the log-normal distribution are set at -0.4 and 1 respectively.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable comparing the optimal and formalized/simplified alactic model fitted parameters and goodness of fit metrics over different events: event (character), pal_max (W/kg), mu, sigma, AIC, BIC and R-squared.
#' @export
#'
#' @examples
#' sprint_alactic_model_gof_comp_table()
#'
sprint_alactic_model_gof_comp_table <- function(data = graubner_nixdorf_sprints) {
  # create a tidy table for the comparison of the optimal and formalized/simplified model

  table <- sprint_alactic_model_gof_comp(data)

  # round the columns

  table$pal_max_optimal <- round(table$pal_max_optimal, 2)
  table$pal_max_formalized <- round(table$pal_max_formalized, 2)
  table$mu_optimal <- round(table$mu_optimal, 2)
  table$mu_formalized <- round(table$mu_formalized, 2)
  table$sigma_optimal <- round(table$sigma_optimal, 2)
  table$sigma_formalized <- round(table$sigma_formalized, 2)
  table$aic_optimal <- round(table$aic_optimal)
  table$aic_formalized <- round(table$aic_formalized)
  table$bic_optimal <- round(table$bic_optimal)
  table$bic_formalized <- round(table$bic_formalized)
  table$r_squared_optimal <- round(table$r_squared_optimal, 2)
  table$r_squared_formalized <- round(table$r_squared_formalized, 2)

  # rename the columns (provide greek letter for mu and sigma)

  names(table)[names(table) == "pal_max_optimal"] <- "Optimal"
  names(table)[names(table) == "pal_max_formalized"] <- "Formalized"
  names(table)[names(table) == "mu_optimal"] <- "Optimal"
  names(table)[names(table) == "mu_formalized"] <- "Formalized"
  names(table)[names(table) == "sigma_optimal"] <- "Optimal"
  names(table)[names(table) == "sigma_formalized"] <- "Formalized"
  names(table)[names(table) == "aic_optimal"] <- "Optimal"
  names(table)[names(table) == "aic_formalized"] <- "Formalized"
  names(table)[names(table) == "bic_optimal"] <- "Optimal"
  names(table)[names(table) == "bic_formalized"] <- "Formalized"
  names(table)[names(table) == "r_squared_optimal"] <- "Optimal"
  names(table)[names(table) == "r_squared_formalized"] <- "Formalized"
  names(table)[names(table) == "rse_optimal"] <- "Optimal"
  names(table)[names(table) == "rse_formalized"] <- "Formalized"

  names(table)[names(table) == "event"] <- "Event <br> "

  # display a distinct cell color for the formalized and optimal models

  table <- table |>
    tinytable::tt(
      notes = "Comparison of the optimal and formalized/simplified alactic model fitted parameters and goodness of fit metrics over different events"
    ) |>
    tinytable::style_tt(
      i = 1:7,
      j = c(2, 4, 6, 8, 10, 12, 14),
      background = "#FF9999"
    ) |>
    tinytable::style_tt(
      i = 1:7,
      j = c(3, 5, 7, 9, 11, 13, 15),
      background = "#99CCFF"
    ) |>
    tinytable::group_tt(j = list(
      "Maximal Alactic<br> Power (W/kg)" = 2:3,
      "\u03bc" = 4:5,
      "\u03c3" = 6:7,
      "AIC" = 8:9,
      "BIC" = 10:11,
      "R-squared" = 12:13,
      "RSE" = 14:15
    ))

  return(table)
}




#' Optimal Parameters for the Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the optimal parameters for the Lactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the optimal parameters for the lactic model: event (character), p_la_max (W/kg), k1 and k2.
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_optimal_params()
#'
sprint_lactic_model_optimal_params <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table

  table <- tibble::tibble(
    event = character(),
    p_la_max = numeric(),
    k1 = numeric(),
    k2 = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    # Fit the log-normal distribution to the alactic power distribution

    params <- fit_approx_lactic_power_params(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      p_la_max = params$p_la_max,
      k1 = params$k1,
      k2 = params$k2
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  # return table

  return(table)
}


#' Table of Optimal Parameters for the Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' A formatted table of the computed optimal parameters for the lactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the optimal parameters for the lactic model: event (character), p_la_max (W/kg), k1 and k2.
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_optimal_params_table()
#'
sprint_lactic_model_optimal_params_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_lactic_model_optimal_params(data)

  # round the columns

  table$p_la_max <- round(table$p_la_max, 2)
  table$k1 <- round(table$k1, 2)
  table$k2 <- round(table$k2, 2)

  # rename the columns (provide greek letter for mu and sigma)

  names(table)[names(table) == "k1"] <- "k1 (s) <br> "
  names(table)[names(table) == "k2"] <- "k2 (s) <br> "
  names(table)[names(table) == "p_la_max"] <- "Maximal Lactic<br> Power (W/kg)"
  names(table)[names(table) == "event"] <- "Event <br> "


  return(tinytable::tt(table, notes = "Lactic model optimal fitted parameters over different events"))
}


#' Parameters of formalized/simplified Log-normal Fit on Lactic Power Distribution
#'
#' Extracts the parameters of a simplified/formalized bi-exponential fit on the approximate lactic power distribution. The formalized model is a model where the k1 and k2 parameters of the bi-exponential distribution are set at 2.75 and 35 s, respectively.
#' The only fitted value is therefore the maximal lactic power. The set value correspond to a compromise of the observed fitted values over the different sprint events.
#'
#' @param sprint_approx_power_distribution A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_lactic (W/kg)
#' @param k1 A double. Time constant of the first rising exponential (s).
#' @param k2 A double. Time constant of the second decaying exponential (s).
#'
#' @returns A list with the following elements: p_la_max, k1, k2
#'
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
#' # Fit the log-normal distribution to the alactic power distribution
#'
#' fit_lactic_power_params(sprint_approx_power_distributions)
#'
fit_lactic_power_params <- function(sprint_approx_power_distribution, k1 = 2.75, k2 = 35) {
  bi_exp_dist <- minpack.lm::nlsLM(
    power_lactic ~ sprint_approx_lactic_power_model(
      time = time,
      maximal_lactic_power = p_la_max,
      k1 = k1,
      k2 = k2
    ),
    data = sprint_approx_power_distribution, start = list(p_la_max = 60)
  )

  return(list(
    p_la_max = coef(bi_exp_dist)[1],
    k1 = k1,
    k2 = k2
  ))
}


#' Parameters for the Formalized/Simplified Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the optimal parameters for the formalized/simplified lactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m. The formalized model is a model where the k1 and k2 parameters of the bi-exponential distribution are set at 2.75 and 35 s, respectively.
#' The only fitted value is therefore the maximal lactic power. The set value correspond to a compromise of the observed fitted values over the different sprint events.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the parameters for the formalized/simplified lactic model: event (character), p_la_max (W/kg), k1 (s) and k2 (s).
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_params()
#'
sprint_lactic_model_params <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table

  table <- tibble::tibble(
    event = character(),
    p_la_max = numeric(),
    k1 = numeric(),
    k2 = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    # Fit the log-normal distribution to the alactic power distribution

    params <- fit_lactic_power_params(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      p_la_max = params$p_la_max,
      k1 = params$k1,
      k2 = params$k2
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  # return table

  return(table)
}


#' Table of Parameters for the Formalized/Simplified Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' A formatted table of the computed optimal parameters for the formalized/simplified lactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m. The formalized model is a model where the k1 and k2 parameters of the bi-exponential distribution are set at 2.75 and 35 s, respectively.
#' The only fitted value is therefore the maximal lactic power (W/kg). The set value correspond to a compromise of the observed fitted values over the different sprint events.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the optimal parameters for the formalized/simplified lactic model: event (character), p_la_max (W/kg), k1 (s) and k2(s).
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_params_table()
#'
sprint_lactic_model_params_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_lactic_model_params(data)

  # round the columns

  table$p_la_max <- round(table$p_la_max, 2)
  table$k1 <- round(table$k1, 2)
  table$k2 <- round(table$k2, 2)

  # rename the columns (provide greek letter for mu and sigma)

  names(table)[names(table) == "k1"] <- "k1 (s) <br> "
  names(table)[names(table) == "k2"] <- "k2 (s) <br> "
  names(table)[names(table) == "p_la_max"] <- "Maximal Lactic<br> Power (W/kg)"
  names(table)[names(table) == "event"] <- "Event <br> "


  return(tinytable::tt(table, notes = "Formalized lactic Model fitted parameters over different sprint events"))
}



#' Goodness of Fit Metrics for the Optimal Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the goodness of fit metrics (AIC, BIC and R-squared) for the optimal lactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m. The optimal model is a model where the maximal lactic power as well as k1 and k2 parameters of the bi-exponential distribution are fitted to the data.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the goodness of fit metrics for the optimal lactic model: event (character), AIC, BIC and R-squared.
#' @export
#'
#' @examples
#'
#'
#' sprint_lactic_model_optimal_gof_metrics()
sprint_lactic_model_optimal_gof_metrics <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table
  table <- tibble::tibble(
    event = character(),
    aic = numeric(),
    bic = numeric(),
    r_squared = numeric(),
    rse = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    gof_metrics <- fit_approx_lactic_gof_metrics(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      aic = gof_metrics$AIC,
      bic = gof_metrics$BIC,
      r_squared = gof_metrics$r_squared,
      rse = gof_metrics$residual_se
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  return(table)
}


#' Formatted Table of Goodness of Fit Metrics for the Optimal Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' Provides a formatted table of the goodness of fit metrics of the optimal lactic model on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the goodness of fit metrics for the optimal lactic model: event (character), AIC, BIC and R-squared.
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_optimal_gof_metrics_table()
#'
sprint_lactic_model_optimal_gof_metrics_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_lactic_model_optimal_gof_metrics(data)

  # round the columns

  table$aic <- round(table$aic)
  table$bic <- round(table$bic)
  table$r_squared <- round(table$r_squared, 3)
  table$rse <- round(table$rse, 2)

  # rename the columns (provide greek letter for mu and sigma)


  names(table)[names(table) == "event"] <- "Event <br> "
  names(table)[names(table) == "aic"] <- "AIC <br> "
  names(table)[names(table) == "bic"] <- "BIC <br> "
  names(table)[names(table) == "r_squared"] <- "R-squared <br> "
  names(table)[names(table) == "rse"] <- "RSE <br> "

  return(tinytable::tt(table, notes = "Optimal lactic model goodness of fit metrics over different events"))
}


#' Goodness of Fit (Gof) of Lactic Power Model (Sprint)
#'
#' Computes the goodness of fit metrics for the formalized sprint lactic power model (with k1 set to 2.75 s, and k2 set to 35 s)
#'
#' @param data A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_lactic (W/kg)
#' @param k1 A double. Time constant of the first rising exponential (s).
#' @param k2 A double. Time constant of the second decaying exponential (s).
#'
#' @returns A list with the following elements: r_squared, residual_se, AIC, BIC
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
#' # Fit the bi-exponential distribution to the lactic power distribution
#'
#' fit_lactic_gof_metrics(sprint_approx_power_distributions)
#'
fit_lactic_gof_metrics <- function(data, k1 = 2.75, k2 = 35) {
  # run the model


  model <- minpack.lm::nlsLM(
    power_lactic ~ sprint_approx_lactic_power_model(
      time = time,
      maximal_lactic_power = p_la_max,
      k1 = k1,
      k2 = k2
    ),
    data = data, start = list(p_la_max = 60)
  )

  # Extract residuals and fitted values
  residuals <- residuals(model)
  fitted <- fitted(model)
  observed <- data$power_lactic

  # Sum of squares
  ss_res <- sum(residuals^2)
  ss_tot <- sum((observed - mean(observed))^2)

  # Metrics
  r_squared <- 1 - (ss_res / ss_tot)
  residual_se <- sqrt(ss_res / df.residual(model))
  aic_val <- AIC(model)
  bic_val <- BIC(model)

  # Return as list
  return(list(
    r_squared = r_squared,
    residual_se = residual_se,
    AIC = aic_val,
    BIC = bic_val
  ))
}


#' Goodness of Fit Metrics for the formalised Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' Computes the goodness of fit metrics (AIC, BIC and R-squared) for the lactic model based on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble with the goodness of fit metrics for the formalized/simplified lactic model: event (character), AIC, BIC and R-squared.
#' @export
#'
#' @examples
#'
#'
#' sprint_lactic_model_gof_metrics()
#'
sprint_lactic_model_gof_metrics <- function(data = graubner_nixdorf_sprints) {
  # events
  events <- unique(data$event)

  # initialize table
  table <- tibble::tibble(
    event = character(),
    aic = numeric(),
    bic = numeric(),
    r_squared = numeric(),
    rse = numeric()
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


    sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
      maximal_aerobic_power = map,
      basal_metabolic_rate = 1.2
    )

    gof_metrics <- fit_lactic_gof_metrics(sprint_approx_power_distributions)

    # generate sub table

    subtable <- tibble::tibble(
      event = i,
      aic = gof_metrics$AIC,
      bic = gof_metrics$BIC,
      r_squared = gof_metrics$r_squared,
      rse = gof_metrics$residual_se
    )

    # add to table

    table <- dplyr::bind_rows(table, subtable)
  }

  return(table)
}

#' Formatted Table of Goodness of Fit Metrics for the Formalized/Simplified Lactic Model (On Berlin 2009 Sprint Performances)
#'
#' Provides a formatted table of the goodness of fit metrics of the formalized/simplified lactic model on the sprint data from Graubner and Nixdorf (2009) for the men's
#' and women's 100, 200 and 400 m.
#'
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable containing the goodness of fit metrics for the oformalized lactic model: event (character), AIC, BIC and R-squared.
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_gof_metrics_table()
#'
sprint_lactic_model_gof_metrics_table <- function(data = graubner_nixdorf_sprints) {
  # generates a tidyer output for the vignette or document

  # run the function

  table <- sprint_lactic_model_gof_metrics(data)

  # round the columns

  table$aic <- round(table$aic)
  table$bic <- round(table$bic)
  table$r_squared <- round(table$r_squared, 2)
  table$rse <- round(table$rse, 2)

  # rename the columns (provide greek letter for mu and sigma)


  names(table)[names(table) == "event"] <- "Event <br> "
  names(table)[names(table) == "aic"] <- "AIC <br> "
  names(table)[names(table) == "bic"] <- "BIC <br> "
  names(table)[names(table) == "r_squared"] <- "R-squared <br> "
  names(table)[names(table) == "rse"] <- "RSE <br> "

  return(tinytable::tt(table, notes = "Formalized/simplified lactic model goodness of fit metrics over different sprint events"))
}

#' Table of Comparison of the Optimal and Formalized/Simplified Lactic Models Parameters and Goodness of Fit Metrics (On Berlin 2009 Sprint Performances)
#'
#' Provides a table comparing the optimal and formalized/simplified lactic model fitted parameters and goodness of fit metrics over different events. The optimal model
#' is a model where the maximal lactic power as well as k1 and k2 parameters of the bi-exponential power distribution are fitted to the data.
#' The formalized model is a model where the k1 and k2 parameters of the bi-exponential distribution are set at 2.75 and 35 s, respectively.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tibble comparing the optimal and formalized/simplified lactic model fitted parameters and goodness of fit metrics over different events: event (character), p_la_max (W/kg), k1, k2, AIC, BIC and R-squared.
#'
#' @export
#'
#' @examples
#'
#' sprint_lactic_model_gof_comp()
#'
sprint_lactic_model_gof_comp <- function(data = graubner_nixdorf_sprints) {
  # For each event, compare the parameters and the gof metrics of the optimal as well as the formalized/simplified version of the model.

  # get parameters and gof of the optimized model

  optimal_params <- sprint_lactic_model_optimal_params(data)
  optimal_gof <- sprint_lactic_model_optimal_gof_metrics(data)

  # get parameters and gof of the formalized/simplified model

  formalized_params <- sprint_lactic_model_params(data)
  formalized_gof <- sprint_lactic_model_gof_metrics(data)


  # merge the table and put the columns of each parameters side by side comparing optimal and formalized/simplified model

  table <- dplyr::left_join(optimal_params, formalized_params, by = "event", suffix = c("_optimal", "_formalized")) |>
    dplyr::left_join(optimal_gof, by = "event") |>
    dplyr::left_join(formalized_gof, by = "event", suffix = c("_optimal", "_formalized")) |>
    dplyr::select(
      event,
      p_la_max_optimal,
      p_la_max_formalized,
      k1_optimal,
      k1_formalized,
      k2_optimal,
      k2_formalized,
      aic_optimal,
      aic_formalized,
      bic_optimal,
      bic_formalized,
      r_squared_optimal,
      r_squared_formalized,
      rse_optimal,
      rse_formalized
    )

  return(table)
}

#' Formatted Table of Comparison of the Optimal and Formalized/Simplified Lactic Models Parameters and Goodness of Fit Metrics (On Berlin 2009 Sprint Performances)
#'
#' Provides a formatted table comparing the optimal and formalized/simplified lactic models fitted parameters and goodness of fit metrics over different events. The optimal model
#' is a model where the maximal lactic power as well as k1 and k2 parameters of the bi-exponential power distribution are fitted to the data.
#' The formalized model is a model where the k1 and k2 parameters of the bi-exponential distribution are set at 2.75 and 35 s, respectively.
#'
#' @param data A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.
#'
#' @returns A tinytable comparing the optimal and formalized/simplified lactic models fitted parameters and goodness of fit metrics over different events: event (character), p_la_max (W/kg), k1 (s), k2 (s), AIC, BIC and R-squared.
#' @export
#'
#' @examples
#' sprint_lactic_model_gof_comp_table()
#'
sprint_lactic_model_gof_comp_table <- function(data = graubner_nixdorf_sprints) {
  # create a tidy table for the comparison of the optimal and formalized/simplified model

  table <- sprint_lactic_model_gof_comp(data)

  # round the columns

  table$p_la_max_optimal <- round(table$p_la_max_optimal, 2)
  table$p_la_max_formalized <- round(table$p_la_max_formalized, 2)
  table$k1_optimal <- round(table$k1_optimal, 2)
  table$k1_formalized <- round(table$k1_formalized, 2)
  table$k2_optimal <- round(table$k2_optimal, 2)
  table$k2_formalized <- round(table$k2_formalized, 2)
  table$aic_optimal <- round(table$aic_optimal)
  table$aic_formalized <- round(table$aic_formalized)
  table$bic_optimal <- round(table$bic_optimal)
  table$bic_formalized <- round(table$bic_formalized)
  table$r_squared_optimal <- round(table$r_squared_optimal, 2)
  table$r_squared_formalized <- round(table$r_squared_formalized, 2)
  table$rse_optimal <- round(table$rse_optimal, 2)
  table$rse_formalized <- round(table$rse_formalized, 2)

  # rename the columns

  names(table)[names(table) == "p_la_max_optimal"] <- "Optimal"
  names(table)[names(table) == "p_la_max_formalized"] <- "Formalized"
  names(table)[names(table) == "k1_optimal"] <- "Optimal"
  names(table)[names(table) == "k1_formalized"] <- "Formalized"
  names(table)[names(table) == "k2_optimal"] <- "Optimal"
  names(table)[names(table) == "k2_formalized"] <- "Formalized"
  names(table)[names(table) == "aic_optimal"] <- "Optimal"
  names(table)[names(table) == "aic_formalized"] <- "Formalized"
  names(table)[names(table) == "bic_optimal"] <- "Optimal"
  names(table)[names(table) == "bic_formalized"] <- "Formalized"
  names(table)[names(table) == "r_squared_optimal"] <- "Optimal"
  names(table)[names(table) == "r_squared_formalized"] <- "Formalized"
  names(table)[names(table) == "rse_optimal"] <- "Optimal"
  names(table)[names(table) == "rse_formalized"] <- "Formalized"

  names(table)[names(table) == "event"] <- "Event <br> "

  # display a distinct cell color for the formalized and optimal models

  table <- table |>
    tinytable::tt(
      notes = "Comparison of the optimal and formalized/simplified alactic model fitted parameters and goodness of fit metrics over different events"
    ) |>
    tinytable::style_tt(
      i = 1:7,
      j = c(2, 4, 6, 8, 10, 12, 14),
      background = "#FF9999"
    ) |>
    tinytable::style_tt(
      i = 1:7,
      j = c(3, 5, 7, 9, 11, 13, 15),
      background = "#99CCFF"
    ) |>
    tinytable::group_tt(j = list(
      "Maximal Lactic<br> Power (W/kg)" = 2:3,
      "k1" = 4:5,
      "k2" = 6:7,
      "AIC" = 8:9,
      "BIC" = 10:11,
      "R-squared" = 12:13,
      "RSE" = 14:15
    ))

  return(table)
}
