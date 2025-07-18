% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprint_alactic_energy_duration.R
\name{sprint_alactic_duration_model_fit_rse}
\alias{sprint_alactic_duration_model_fit_rse}
\title{Sprint Alactic Duration Model Fit Residual Standard Error}
\usage{
sprint_alactic_duration_model_fit_rse(
  alactic_power_duration,
  mu_al = 1.75,
  sigma_al = 1.5
)
}
\arguments{
\item{alactic_power_duration}{A tibble with the following columns: duration (s), alactic_power (W/kg), corresponding to the average alactic power output over the running duration. This data is used to fit the alactic power model.}

\item{mu_al}{A double representing the peak of the log-normal distribution. Default is 1.75.}

\item{sigma_al}{A double representing the decay of the log-normal distribution. Default is 1.5.}
}
\value{
A numeric value representing the residual standard error of the fitted alactic power model.
}
\description{
Performs a non-linear least squares fitting of the alactic power model to the alactic power duration data and returns the residual standard error.
}
\examples{

men_alactic_energy <- sprint_alactic_energy_duration_graubner_nixdorf()
men_alactic_energy

 # Fit alactic model to power

 sprint_alactic_duration_model_fit_rse(men_alactic_energy)

}
