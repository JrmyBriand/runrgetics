% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprint_alactic_energy_duration.R
\name{sprint_alactic_capacity}
\alias{sprint_alactic_capacity}
\title{Alactic Capacity from Alactic Power Duration Data}
\usage{
sprint_alactic_capacity(alactic_power_duration, mu_al = 1.75, sigma_al = 1.5)
}
\arguments{
\item{alactic_power_duration}{A tibble with the following columns: duration (s), alactic_power (W/kg), corresponding to the average alactic power output over the running duration. This data is used to fit the alactic power model.}

\item{mu_al}{A double representing the peak of the log-normal distribution. Default is 1.75.}

\item{sigma_al}{A double representing the decay of the log-normal distribution. Default is 1.5.}
}
\value{
A numeric value representing the alactic capacity (in J/kg) estimated from the alactic power duration data.
}
\description{
Computes the alactic capacity from alactic power duration data by fitting the alactic power over running duration model to the data.
}
\examples{

men_alactic_energy <- sprint_alactic_energy_duration_graubner_nixdorf()
men_alactic_energy

 # Fit alactic model to power

 sprint_alactic_capacity(men_alactic_energy)

}
