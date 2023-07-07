#This file implements date and time functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# DATE AND TIME
#####################################

#' Fix dates
#'
#' Make a consistent date format from a provided variable. Has internal class detection so
#' can be called on already properly formatted dates (POSIXt) too.
#' For date and time use \code{\link{makeconsistentdatetimeformat}} function.
#' @param thedates Vector with dates to convert. Can work with POSIXt, Date, numeric and string.
#' If numeric is provided, it assumes Unix timestamp in seconds. If \code{NULL} is provided it returns \code{NULL} too.
#' @param asstring A string format (following \code{\link[base]{strptime}}) to convert the date to. Default=\code{NULL},
#' which returns the date in POSIXct format.
#' @param timezone String determining the timezone (from \code{as.POSIXct}). Default=\code{""},
#' which uses the local time zone.
#' @return The dates in POSIXct format with the provided timezone, or as a string through the provided format,
#' or \code{NULL} if the provided input is incorrect.
#' @family date and time functions
#' @export
makeconsistentdateformat <- function(thedates,asstring=NULL,timezone="") {
  if (length(thedates)==0) {
    return(NULL)
  }
  if (any(class(thedates)=="POSIXlt")) {                                  #just be sure to make lt ct
    thedates=as.POSIXct(thedates)
  }
  if (all(class(thedates)!="POSIXt")) {                                   #provided vector is not POSIXct
    if (class(thedates)=="Date") {                                          #provided vector is Date, that's easily convertible
      thedates=as.POSIXct(thedates, tz=timezone)
    } else if (class(thedates)=="numeric") {                                #provided vector is numeric, that must be UNIX time
      thedates=as.POSIXct(thedates, origin="1970-01-01", tz=timezone)
    } else {                                                                #assuming character now
      tryformats=c("%d%B%Y","%d-%m-%Y","%Y-%m-%d","%d/%m/%Y","%Y/%m/%d")      #various formats to test (note: order is important as 30/06/2017 can become 0030/06/20 with %Y/%m/%d)
      for (i in 1:length(tryformats)) {                                       #loop over the formats
        tryout=as.POSIXct(thedates[1],format=tryformats[i], tz=timezone[1])   #try if one is ok
        if (!is.na(tryout)) {                                                 #if it becomes a format
          thedates=as.POSIXct(thedates,format=tryformats[i], tz=timezone)     #do all of them
          break
        }
      }
    }
  }

  if (!is.null(asstring)) {
    thedates=strftime(thedates,asstring)
  }
  return(thedates)
}

#' Fix datetimes
#'
#' Make a consistent date format from a provided variable. Has internal class detection so
#' can be called on already properly formatted dates (POSIXt) too.
#' For date only use \code{\link{makeconsistentdateformat}} function.
#' @param thedates Vector with string datetimes to convert. Can work with POSIXt, Date, numeric and string.
#' If numeric is provided, it assumes Unix timestamp in seconds. If \code{NULL} is provided it returns \code{NULL} too.
#' @param asstring A string format (uses \code{strftime}) to convert the datetime to (default=\code{NULL},
#' which returns the date in POSIX format).
#' @param timezone String determining the timezone (from \code{as.POSIXct}). Default=\code{""},
#' which uses the local time zone.
#' @return The datetimes in POSIXct format with the provided timezone, or as a string through the provided format,
#' or \code{NULL} if the provided input is incorrect.
#' @family date and time functions
#' @export
makeconsistentdatetimeformat <- function(thedates,asstring=NULL,timezone="") {
  if (length(thedates)==0) {
    return(NULL)
  }
  if (any(class(thedates)=="POSIXlt")) {                                  #just be sure to make lt ct
    thedates=as.POSIXct(thedates)
  }
  if (all(class(thedates)!="POSIXt")) {                                   #provided vector is not POSIXct
    if (class(thedates)=="Date") {                                          #provided vector is Date, that's easily convertible
      thedates=as.POSIXct(thedates, tz=timezone)
    } else if (class(thedates)=="numeric") {                                #provided vector is numeric, that must be UNIX time
      thedates=as.POSIXct(thedates, origin="1970-01-01", tz=timezone)
    } else {
      # tryformats=c("%d%B%Y:%H:%M:%OS",
      #              "%d-%m-%YT%H:%M:%OS","%Y-%m-%dT%H:%M:%OS",
      #              "%d/%m/%YT%H:%M:%OS","%Y/%m/%dT%H:%M:%OS",
      #              "%d-%m-%Y %H:%M:%OS","%Y-%m-%d %H:%M:%OS",
      #              "%d.%m.%Y %H:%M:%OS","%Y.%m.%d %H:%M:%OS",
      #              "%d%m%yT%h%m%d")
      tryformats=c("%d%B%Y.%H.%M.%OS",
                   "%d-%m-%YT%H.%M.%OS","%Y-%m-%dT%H.%M.%OS",
                   "%d/%m/%YT%H.%M.%OS","%Y/%m/%dT%H.%M.%OS",
                   "%d-%m-%Y %H.%M.%OS","%Y-%m-%d %H.%M.%OS",
                   "%d.%m.%Y %H.%M.%OS","%Y.%m.%d %H.%M.%OS",
                   "%d%m%yT%h%m%d", "%Y%m%d%H%M%OS")
      #various formats to test (note: order is important as 30/06/2017 can become 0030/06/20 with %Y/%m/%d)

      thedates=gsub(":", ".", thedates)                                       #replace colons with dots (in case milliseconds is with :)
      for (i in 1:length(tryformats)) {                                       #loop over the formats
        tryout=as.POSIXct(thedates[1],format=tryformats[i], tz=timezone[1])   #try if one is ok
        if (!is.na(tryout)) {                                                 #if it becomes a format
          thedates=as.POSIXct(thedates,format=tryformats[i], tz=timezone)     #do all of them
          break
        }

      }
    }
  }
  if (!is.null(asstring)) {
    thedates=strftime(thedates,asstring)
  }
  return(thedates)
}

#' Find contained periods
#'
#' For a vector of start and end \code{POSIXct} datetimes, find where that time period is fully contained
#' within another start and end vector.
#' @param inputstartdates A vector of \code{POSIXct} datetimes representing start dates.
#' @param inputenddates A vector of \code{POSIXct} datetimes representing end dates.
#' @param checkstartdates A vector of \code{POSIXct} datetimes representing start dates that need to be checked.
#' Default=\code{NULL}, which assumes the \code{inputstartdates} to be checked.
#' @param checkenddates A vector of \code{POSIXct} datetimes representing end dates that need to be checked.
#' Default=\code{NULL}, which assumes the \code{inputenddates} to be checked.
#' @param returnfirstonly Set to \code{TRUE} if only the first found fully contained period needs to be returned.
#' Default=\code{FALSE} which returns all fully contained periods found.
#' @return A list of integer indices per element of \code{inputxxxdates} representing the indices of
#' \code{checkxxxdates} in which the input can fully fit. For example, \code{inputstartdates="2020-10-10 12:30"}
#' and \code{inputenddates="2020-11-01 15:30"} is fully contained within \code{checkstartdates="2020-09-20 15:45"}
#' and \code{checkenddates="2020-12-12 13:30"}. It will return a list of indices of the \code{checkstartdates}
#' variable if this is true. If \code{returnfirstonly=TRUE}, it only returns the first index of that list per element.
#' @family date and time functions
#' @export
findperiodcontained <- function(inputstartdates, inputenddates, checkstartdates=NULL, checkenddates=NULL, returnfirstonly=FALSE) {
  if (is.null(checkstartdates)) {
    checkstartdates <- inputstartdates
    checkenddates <- inputenddates
  }
  checktable=data.table(start_datetime=checkstartdates,
                        end_datetime=checkenddates)
  for (i in 1:length(inputstartdates)) {
    start_date_i <- inputstartdates[i]
    end_date_i <- inputenddates[i]
    foundindex <- which(checktable[,(start_date_i >= start_datetime & start_date_i < end_datetime & end_date_i > start_datetime & end_date_i <= end_datetime) & seq_len(.N) != i])
    if (returnfirstonly) {
      foundindex <- ifelse(length(foundindex), foundindex[1], as.integer(NA))
      checktable[i, contain_row := foundindex]
    } else {
      checktable[i, contain_row := list(as.list(foundindex))]
    }
  }
  return(checktable[,contain_row])
}

#' Find overlapping periods
#'
#' For a vector of start and end \code{POSIXct} datetimes, find where that time period is overlapping
#' within another start and end vector.
#' @param inputstartdates A vector of \code{POSIXct} datetimes representing start dates.
#' @param inputenddates A vector of \code{POSIXct} datetimes representing end dates.
#' @param checkstartdates A vector of \code{POSIXct} datetimes representing start dates that need to be checked.
#' Default=\code{NULL}, which assumes the \code{inputstartdates} to be checked.
#' @param checkenddates A vector of \code{POSIXct} datetimes representing end dates that need to be checked.
#' Default=\code{NULL}, which assumes the \code{inputenddates} to be checked.
#' @param returnfirstonly Set to \code{TRUE} if only the first found overlapping period needs to be returned.
#' Default=\code{FALSE} which returns all overlapping periods found.
#' @return A list of integer indices per element of \code{inputxxxdates} representing the indices of
#' \code{checkxxxdates} the input overlaps with. For example, \code{inputstartdates="2020-10-10 12:30"}
#' and \code{inputenddates="2020-11-01 15:30"} overlaps with \code{checkstartdates="2020-10-20 15:45"}
#' and \code{checkenddates="2020-12-12 13:30"}. It will return a list of indices of the \code{checkstartdates}
#' variable if this is true. If \code{returnfirstonly=TRUE}, it only returns the first index of that list per element.
#' @family date and time functions
#' @export
findperiodoverlapping <- function(inputstartdates, inputenddates, checkstartdates=NULL, checkenddates=NULL, returnfirstonly=FALSE) {
  if (is.null(checkstartdates)) {
    checkstartdates <- inputstartdates
    checkenddates <- inputenddates
  }
  checktable=data.table(start_datetime=checkstartdates,
                        end_datetime=checkenddates)
  for (i in 1:length(inputstartdates)) {
    start_date_i <- inputstartdates[i]
    end_date_i <- inputenddates[i]
    foundindex <- which(exceltable[,(start_date_i < end_datetime & end_date_i > start_datetime) & seq_len(.N) != i])
    if (returnfirstonly) {
      foundindex <- ifelse(length(foundindex), foundindex[1], as.integer(NA))
      checktable[i, contain_row := foundindex]
    } else {
      checktable[i, contain_row := list(as.list(foundindex))]
    }
  }
  return(checktable[,contain_row])
}

#' Sequence of days
#'
#' Get a sequence of days.
#' @param startdate The start date. Default=\code{NULL}, which assumes today.
#' @param enddate The end date. Default=\code{NULL}, which assumes today.
#' @param stepdate The sequence increment (default=\code{1}).
#' @param timezone String determining the timezone (from \code{as.POSIXct}). Default=\code{""},
#' which uses the local time zone.
#' @return The sequence in POSIXct format.
#' @family date and time functions
#' @export
seqdays <- function(startdate=NULL, enddate=NULL, stepdate=1,timezone="") {
  if (is.null(startdate)) {
    startdate=as.POSIXct(format(Sys.Date(),"%Y-%m-%d"),tz=timezone)
  } else {
    startdate=as.POSIXct(startdate,tz=timezone)
  }
  if (is.null(enddate)) {
    enddate=as.POSIXct(format(Sys.Date(),"%Y-%m-%d"),tz=timezone)
  } else {
    enddate=as.POSIXct(enddate,tz=timezone)
  }
  temp=FALSE
  if (startdate>enddate) {
    temp=startdate
    startdate=enddate
    enddate=temp
    temp=TRUE
  }
  if (startdate==enddate) {
    return(startdate)
  } else {
    theseq=seq(from=startdate, to=enddate, by = 86400)
    if (temp) {
      theseq=rev(theseq)
    }
    return(theseq)
  }
}
