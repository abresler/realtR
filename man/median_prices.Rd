% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/realtor.R
\name{median_prices}
\alias{median_prices}
\title{Get median market statistics for specified locations}
\usage{
median_prices(locations = NULL, return_message = TRUE, ...)
}
\arguments{
\item{locations}{vector of location , location name must contain
a city name and a comma ie "Brooklyn, NY"}

\item{return_message}{if \code{TRUE} returns a message}

\item{...}{}
}
\value{
a \code{data_frame}
}
\description{
Returns summary market information for the specified
location.  The locaiton name must be a city bounded by a comma
}
\examples{
median_prices(locations = c("Greenwich, CT", "New London, CT", "Woodside, CA", "Park Slope, Brooklyn, NY"))
}