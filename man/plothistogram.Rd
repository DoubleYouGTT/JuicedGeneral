% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuicedPlotting.R
\name{plothistogram}
\alias{plothistogram}
\title{Plot a histogram}
\usage{
plothistogram(
  datatable,
  plotsettings = NULL,
  returnplotly = FALSE,
  plotlysource = "A"
)
}
\arguments{
\item{datatable}{The datatable in which the data is stored to plot.}

\item{plotsettings}{The settings to use in the plot. Default=\code{NULL}, which loads default plotsettings
from \code{\link{getdefaultplotsettings}}.}

\item{returnplotly}{Set to \code{TRUE} to return the plotly variable to be used or modified in another setting (such as in
a dashboard). Default=\code{FALSE}, which displays the plot.}

\item{plotlysource}{Source attribute as defined in \code{\link[plotly]{plot_ly}}). Default=\code{"A"}.}
}
\value{
The plotly variable or an invisible \code{NULL}.
}
\description{
Plots a histogram with columns from a data.table.
}
\seealso{
Other plot functions: 
\code{\link{getdefaulthovertext}()},
\code{\link{getdefaultlabeltext}()},
\code{\link{getdefaultplotsettings}()},
\code{\link{makeplotsettings}()},
\code{\link{plot_makecolors}()},
\code{\link{plot_makemarkers}()},
\code{\link{plotcandlestick}()},
\code{\link{plotdensityfunctions}()},
\code{\link{plotecdffunctions}()},
\code{\link{plotpiechart}()},
\code{\link{plotscatterwithlink}()},
\code{\link{plotscatter}()},
\code{\link{sampledashboard}()},
\code{\link{updateplotsettings}()}
}
\concept{plot functions}
