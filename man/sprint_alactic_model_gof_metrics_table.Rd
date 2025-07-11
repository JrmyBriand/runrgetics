% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprint_model_formalisation.R
\name{sprint_alactic_model_gof_metrics_table}
\alias{sprint_alactic_model_gof_metrics_table}
\title{Formatted Table of Goodness of Fit Metrics for the Formalized/Simplified Alactic Model (On Berlin 2009 Sprint Performances)}
\usage{
sprint_alactic_model_gof_metrics_table(data = graubner_nixdorf_sprints)
}
\arguments{
\item{data}{A tibble with the following: distance (m), splits (s), velocity (m/s), reaction_time (s), maximal_velocity (m/s) and event (character). Default is Graubner and Nixdorf (2009) sprint data.}
}
\value{
A tinytable containing the goodness of fit metrics for the optimale alactic model: event (character), AIC, BIC, residual squared error and R-squared.
}
\description{
Provides a formatted table of the goodness of fit metrics of the formalized/simplified alactic model on the sprint data from Graubner and Nixdorf (2009) for the men's
and women's 100, 200 and 400 m.
}
\examples{

sprint_alactic_model_gof_metrics_table()

}
