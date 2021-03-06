% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shaw_moi.R
\name{shaw_moi}
\alias{shaw_moi}
\title{Function to build SHAW moisture initial conditions for the soil profile.}
\usage{
shaw_moi(
  model_dir,
  method,
  moi_obs = NULL,
  soil1,
  iwrc = 3,
  site_file = paste0(model_dir, "/", basename(model_dir), ".sit")
)
}
\arguments{
\item{model_dir}{This is the directory where all inputs will be written.}

\item{method}{Options are "saturated", "residual", "half", "observed". If method is "observed", moi_obs requires a data frame with depths and observed moisture. Will then do stepwise interpolation.}

\item{moi_obs}{Observed data frame: column names must be depth and VWC.}

\item{soil1}{First line of the soils.}

\item{iwrc}{Flag used to define water retention curve (1 = Campbell; 2 = Brooks-Corey; 3 = van Genuchten).}

\item{site_file}{This is the site file - will be used to determine saturated or residual values.}
}
\description{
This function allows you to build SHAW files based on desired parameters.
}
\keyword{SHAW.}
\keyword{hydrology,}
