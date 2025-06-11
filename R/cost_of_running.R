#' Cost of Running (Sprint Version)
#'
#' Computes the instantaneous cost of running based on velocity and acceleration data,
#' based on the method of di prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#' In the sprint application, it is assumed that acceleration cannot be negative.
#' The observed slowdown in sprints is not due to a change in the runner's
#' inclination of the center of mass but rather the inability to generate the required power.
#'
#' @inheritParams cost_running
#'
#' @returns a vector of costs of running (J/kg/m)
#' @export
#'
#' @examples
#' cost_running_sprint(acceleration = c(2, 4, 5), velocity = c(2, 4, 5))
#'
cost_running_sprint <- function(acceleration, velocity, cost_running_flat = 3.6, slope_equation = "original") {
  # Set negative accelerations to zero
  acc_modified <- pmax(acceleration, 0)
  # Calculate Effective Slope
  effective_slope <- acc_modified / 9.8
  # Calculate effective mass
  effective_mass <- sqrt(effective_slope^2 + 1)

  if(slope_equation == "original") {
    # Calculate base metabolic cost using piecewise model
    cr <- ifelse(effective_slope >= 0.45,
                 # High acceleration phase (linear model)
                 (55.65 * effective_slope - 5.61) * effective_mass,
                 # Low acceleration phase (polynomial model)
                 (155.4 * effective_slope^5 - 30.4 * effective_slope^4 - 43.3 * effective_slope^3 +
                    46.3 * effective_slope^2 + 19.5 * effective_slope + cost_running_flat) * effective_mass
    )
  } else if(slope_equation == "extended") {
    cr <- 0.102*(acc_modified^2 + 96.2)^(1/2)*(4.03*acc_modified + cost_running_flat * exp(-0.408*acc_modified))
  } else {
    stop("slope_equation must be either 'original' or 'extended'")
  }

  # Add wind resistance component
  cr <- cr + 0.01 * velocity^2
  return(cr)
}


#' Cost of Running
#'
#' Computes the instantaneous cost of running based on velocity and acceleration data,
#' based on the method of di prampero et al. (2005, 2018) and as described in Briand et al. (2025).
#' The function considers scenarios where the deceleration is initiated by the athlete
#' changing the position of the center of mass. The function can be applied for example
#' on gps data derived from athletes performing changes of directions in a team sport
#' as in di Prampero and Osgnach (2018).
#'
#'
#' @param acceleration a vector of accelerations (m/s^2)
#' @param velocity a vector of velocities (m/s)
#' @param cost_running_flat a numeric value representing the cost of running on a flat surface (default is 3.6 J/kg/m, as suggested in Minetti et al. 2002)
#' @param slope_equation a string indicating the slope equation to use, either "original" or "extended". The "original" equation is based on the equation relating running at constant speed on different slopes porposed by Minetti et al. (2002), while the "extended" equation is based on similar equation for extended slopes proposed by Minetti and Pavei (2018).
#'
#' @returns a vector of costs of running (J/kg/m)
#' @export
#'
#' @examples
#' cost_running(acceleration = c(2, 4, 5), velocity = c(2, 4, 5))
#'
cost_running <- function(acceleration, velocity, cost_running_flat = 3.6, slope_equation = "original") {
  # Calculate effective slope
  effective_slope <- acceleration / 9.8
  # Calculate effective mass
  effective_mass <- sqrt(effective_slope^2 + 1)

  if(slope_equation == "original") {
    # Calculate base metabolic cost using piecewise model
    cr <- ifelse(effective_slope >= 0.45,
                 # High acceleration phase (linear model)
                 (55.65 * effective_slope - 5.61) * effective_mass,
                 # Low acceleration phase (polynomial model)
                 (155.4 * effective_slope^5 - 30.4 * effective_slope^4 - 43.3 * effective_slope^3 +
                    46.3 * effective_slope^2 + 19.5 * effective_slope + cost_running_flat) * effective_mass
    )
  } else if(slope_equation == "extended") {
    if(acceleration >= 0) {
      # cost running equation based on Minetti and Pavei 2018 for forward acceleration
      cr <- 0.102*(acceleration^2 + 96.2)^(1/2)*(4.03*acceleration + cost_running_flat * exp(-0.408*acceleration))
    } else {
      # cost running equation based on Minetti and Pavei 2018 for deceleration
      cr <- 0.102*(acceleration^2 + 96.2)^(1/2)*(-0.85*acceleration + cost_running_flat * exp(1.33*acceleration))
    }
  } else {
    stop("slope_equation must be either 'original' or 'extended'")
  }

  # Add wind resistance component
  cr <- cr + 0.01 * velocity^2
  return(cr)
}
