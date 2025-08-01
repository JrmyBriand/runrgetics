% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprint_perf_from_splits.R
\name{sprint_perf_from_split}
\alias{sprint_perf_from_split}
\title{Sprint Performance from Time Splits}
\usage{
sprint_perf_from_split(time_splits, distance, reaction_time = 0)
}
\arguments{
\item{time_splits}{A vector with time splits over each distance interval (s)}

\item{distance}{A vector with the distances at which time splits were measured (m)}

\item{reaction_time}{A double with the reaction time measured on the starting blocks (s)}
}
\value{
A tibble with the following columns: distance (m), performance (s)
}
\description{
Computes the sprint performance (time to cover the total distance) from time splits and
associated distance at which the time splits were measured.
}
\examples{

# extract men's 100 m data from graubner-nixdorf_sprints dataset

data <- graubner_nixdorf_sprints |>
          dplyr::filter(event == "Men's 100 m")

 sprint_perf_from_split(time_splits = data$splits,
 distance = data$distance,
 reaction_time = data$reaction_time[1])

}
