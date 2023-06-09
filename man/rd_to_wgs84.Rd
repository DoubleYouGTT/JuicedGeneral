% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rijksdriehoek.R
\name{rd_to_wgs84}
\alias{rd_to_wgs84}
\title{Translate coordinates in rijksdriehoek (Dutch National Grid) system to WGS84}
\usage{
rd_to_wgs84(x, y)
}
\arguments{
\item{x}{vector of x-coordinates in rijksdriehoek system; or without y a
vector of length 2 containing the x and y coordinates.}

\item{y}{vector of y-coordinates in rijksdriehoek system}
}
\value{
A list with lamba (decimal degrees; longitude) and phi (decimal degrees;
latitude), or if just x was given a numeric vector of length 2 with lambda
and phi.
}
\description{
Translate coordinates in rijksdriehoek (Dutch National Grid) system to WGS84
}
