% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_half_life.R
\name{fit_half_life}
\alias{fit_half_life}
\title{Fitting the half life parameter to a curve activity vs time}
\usage{
fit_half_life(activity, my_time, half_life, shape)
}
\arguments{
\item{activity}{Numeric vector with activity in percentage (Ao should be percentage), activity could be any of the measured parameters, e.g. PSII efficiency, ETRmax, etc}

\item{my_time}{numeric vector with the duration of exposure (expressed in min or seconds)}

\item{half_life}{Starting value for the half_life parameter (numeric)}

\item{shape}{Starting value for the shape parameter (numeric)}
}
\value{
Returns a list with the two fitted parameters, i.e. half_life and shape
}
\description{
Fitting the half life parameter to a curve activity vs time.
}
\keyword{internal}
