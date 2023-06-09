% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuicedConversion.R
\name{recastPOSIXct}
\alias{recastPOSIXct}
\title{Recast timezone}
\usage{
recastPOSIXct(x, tz)
}
\arguments{
\item{x}{The POSIXct time to change}

\item{tz}{The timezone to transform to}
}
\value{
The POSIXct time in the new timezone
}
\description{
Transform a timezone to another but keep the exact time the same (no calculation). This one is slow.
}
