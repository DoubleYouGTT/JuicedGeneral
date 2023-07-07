#This file implements data functions
#modified: June 2020
#author:   J. Kuckartz - joost.kuckartz@arcadis.com

#####################################
# DATA FUNCTIONS
#####################################

#' Get local minima and maxima
#' @param x Vector of values to get the local minima and maxima locations of.
#' @param threshold Integer threshold value to get wider spaced minima and maxima
#' the higher threshold is. Default = 1.
#' @return Named list with vector locations (like the \code{which} function) where
#' (local) minima and  maxima occur.
#' @family vector functions
#' @family data functions
#' @export
localminimamaxima <- function(x, threshold = 1) {
  # from https://stackoverflow.com/a/43061365
  up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    <- cbind(x,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
}

#' Get local maxima
#' @param x Vector of values to get the local maxima locations of.
#' @return The vector locations (like the \code{which} function) where
#' local maxima occur.
#' @family vector functions
#' @family data functions
#' @export
localmaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  # from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}

#' Get local minima
#' @param x Vector of values to get the local minima locations of.
#' @return The vector locations (like the \code{which} function) where
#' local minima occur.
#' @family vector functions
#' @family data functions
#' @export
localminima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  # from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}

#' Get some time series statistics
#' @param thetimes Data.table with the first column being the DateTime and the second column being the values,
#' or, a vector containing the DateTime only.
#' @param thedata Vector containing the values correlated to the DateTime. Must be of equal length to \code{thetimes}.
#' Default=\code{NULL}, which only uses the first parameter.
#' @return A named list with some time series statistics or \code{NULL} on failure. The "grouping" variables show
#' how many points would be within a specific time period, and how many bins would occur for the full time period.
#' This can be useful for calculating optimal statistics for data lasting a long time.
#' @seealso createbins
#' @family data functions
#' @family time series functions
#' @export
timeseriesstats <- function(thetimes,thedata=NULL) {
  if (any(class(thetimes)=="data.table")) {
    thedata=thetimes[[2]]
    thetimes=thetimes[[1]]
  }
  if (length(thedata)!=length(thetimes))
    return(NULL)

  retval=list()
  retval$data_totalpoints=length(thetimes)
  retval$time_oldest=min(thetimes)
  retval$time_newest=max(thetimes)
  retval$time_duration=difftime(retval$time_newest, retval$time_oldest)
  retval$time_frequency_hz=retval$data_totalpoints/as.numeric(retval$time_duration,units="secs")
  retval$time_period_s=1/retval$time_frequency_hz

  retval$grouping_minute_points=60*retval$time_frequency_hz
  retval$grouping_minute_bins=retval$data_totalpoints/retval$grouping_minute_points

  retval$grouping_hour_points=60*retval$grouping_minute_points
  retval$grouping_hour_bins=retval$data_totalpoints/retval$grouping_hour_points

  retval$grouping_day_points=60*retval$grouping_hour_points
  retval$grouping_day_bins=retval$data_totalpoints/retval$grouping_day_points

  retval$grouping_optimal="min"
  optimalbinv1=abs(retval$grouping_minute_points-retval$grouping_minute_bins)
  optimalbinv2=abs(retval$grouping_hour_points-retval$grouping_hour_bins)
  if (optimalbinv1>optimalbinv2)
    retval$grouping_optimal="hour"
  optimalbinv1=abs(retval$grouping_day_points-retval$grouping_day_bins)
  if (optimalbinv2>optimalbinv1)
    retval$grouping_optimal="day"

  return(retval)
}

#' Create time series bins
#'
#' Create time series bin by calculating statistical information for each bin containing data. It will use the rounded times
#' as start and end times, even if data is starting at the end of the bin. There must be both time and data provided.
#' @param thetimes Data.table with the first column being the DateTime and the second column being the values,
#' or, a vector containing the DateTime only.
#' @param thedata Vector containing the values correlated to the DateTime. Must be of equal length to \code{thetimes}.
#' Default=\code{NULL}, which only uses the first parameter.
#' @param binsize The bin size to split by. Can be \code{sec}, \code{min}, \code{hour}, \code{mday}, \code{mon}, \code{year},
#' \code{wday} or \code{yday}.
#' @return Data.table with the starting times of the bins and each column with statistical information about that bin.
#' @family data functions
#' @family time series functions
#' @export
createbins <- function(thetimes,thedata=NULL,binsize="min") {
  if (any(class(thetimes)=="data.table")) {
    thedata=thetimes[[2]]
    thetimes=thetimes[[1]]
  }
  if (length(thedata)!=length(thetimes))
    return(NULL)

  res=split(thedata, cut(thetimes,binsize))
  retval=data.table(DateTime=as.POSIXct(names(res)))

  #calculate
  calcval=sapply(res, length)
  names(calcval)=NULL
  retval[,Count:=calcval]                 #count


  suppressWarnings({                      #no warnings here
    calcval=sapply(res, mean)
    names(calcval)=NULL
    retval[,Average:=calcval]             #average
    calcval=sapply(res, min)
    names(calcval)=NULL
    retval[,Min:=calcval]                 #min
    calcval=sapply(res, max)
    names(calcval)=NULL
    retval[,Max:=calcval]                 #max
    calcval=sapply(res, sd)
    names(calcval)=NULL
    retval[,StDev:=calcval]               #standard deviation
  })

  #fix for empty bins
  retval[Count==0, Average:=NA]
  retval[Count==0, Min:=NA]
  retval[Count==0, Max:=NA]
  retval[Count==0, StDev:=NA]
  return(retval)
}

#' Remove offset
#' Remove offset value from time series data for a period that is considered non-moving.
#' @param dataset The data.table containing the data from which offset needs to be removed.
#' @param statictime The amount of seconds of the beginning of the dataset to take as static non-moving. Default=\code{60}.
#' @param timecolumn The name of the column that contains the time information in POSIXct.
#' Default=\code{NULL}, which takes the first column.
#' @param datacolumn The name of the column(s) that contain the data (numeric). Default=\code{NULL}, which takes all columns
#' that are not the time column.
#' @return The sensor data with the offset removed, or \code{NULL} on failure.
#' @family data functions
#' @family time series functions
#' @export
removeoffset <- function(dataset,statictime=60,timecolumn=NULL,datacolumn=NULL) {
  if (!any(class(dataset)=="data.table")) {
    message("Offset removal: the provided set was not a data.table.")
    return(NULL)
  }
  if (is.null(timecolumn)) {
    timecolumn=colnames(dataset)[1]
  }
  if (!any(class(dataset[,get(timecolumn)])=="POSIXct")) {
    message("Offset removal: the time column is not in POSIXct format.")
    return(NULL)
  }
  if (is.null(datacolumn)) {
    datacolumn=colnames(dataset)
    datacolumn=datacolumn[!timecolumn==datacolumn]
  }
  endtime=dataset[1,get(timecolumn)]+statictime                   #get end time
  locrange=c(1:max(which(dataset[,get(timecolumn)]<endtime)))     #get range to take as initial period

  for (colname in datacolumn) {                                   #loop all columns with data
    avgval=mean(dataset[locrange,get(colname)],na.rm=TRUE)        #get average value for initial time period
    dataset[,c(colname):=get(colname)-avgval]                     #remove average value for all data
  }
  return(dataset)
}
