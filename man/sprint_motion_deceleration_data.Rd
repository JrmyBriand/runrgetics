% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprint_energetics_exploration.R
\name{sprint_motion_deceleration_data}
\alias{sprint_motion_deceleration_data}
\title{Sprint Motion Acceleration Data}
\usage{
sprint_motion_deceleration_data(sprint_motion_data)
}
\arguments{
\item{sprint_motion_data}{A tibble with the following columns: time, velocity, acceleration, distance, cost of running and power}
}
\value{
A tibble, corresponding to the sprint motion deceleration data, with the following columns: time, velocity, acceleration, distance, cost of running and power
}
\description{
Extracts data from a data frame composed of sprint motion data corresponding
to the sprint acceleration phase. The sprint motion data can be computed with the function
\code{\link{sprint_motion_model_data}}.
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

sprint_motion_deceleration_data(sprint_data)

}
