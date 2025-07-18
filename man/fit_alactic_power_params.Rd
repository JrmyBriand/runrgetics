% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprint_model_formalisation.R
\name{fit_alactic_power_params}
\alias{fit_alactic_power_params}
\title{Parameters of formalized/simplified Log-normal Fit on Alactic Power Distribution}
\usage{
fit_alactic_power_params(
  sprint_approx_power_distribution,
  mu = -0.4,
  sigma = 1
)
}
\arguments{
\item{sprint_approx_power_distribution}{A tibble containing approximate power distributions over the course of the sprint with at least the following columns: time (s), power_alactic (W/kg)}

\item{mu}{A double. Parameter setting the peak of the log-normal distribution.}

\item{sigma}{A double. Parameter setting the decay of the log-normal distribution.}
}
\value{
A list with the following elements: pal_max, sigma, mu
}
\description{
Extracts the parameters of a simplified/formalized log-normal fit on the approximate alactic power distribution. The formalized model is a model where the mu and sigma parameters of the log-normal disctribution are set at -0.4 and 1 respectively.
The only fitted value is therefore the maximal alactic power. The set value correspond to a compromise of the observed fitted values over the different sprint events.
}
\examples{
# Extract the data for the 100 m
men_100 <- graubner_nixdorf_sprints |>
  dplyr::filter(event == "Men's 100 m")


# Get the sprint motion data for both men and women

sprint_data <- sprint_motion_model_data(
  mean_velocity_splits = men_100$velocity,
  time_splits = men_100$splits,
  distance = men_100$distance,
  reaction_time = men_100$reaction_time[1],
  maximal_velocity = men_100$maximal_velocity[1]
)

sprint_approx_power_distributions <- sprint_approx_power_distributions(sprint_data,
  maximal_aerobic_power = 24.5,
  basal_metabolic_rate = 1.2
)

# Fit the log-normal distribution to the alactic power distribution

fit_alactic_power_params(sprint_approx_power_distributions)

}
