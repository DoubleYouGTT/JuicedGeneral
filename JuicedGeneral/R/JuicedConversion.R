#This file implements conversion functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# CONVERSION FUNCTIONS
#####################################

#' Hex to binary
#'
#' Transform hexadecimal values (in string vector) to binary bits (also in string vector)
#' @param hexvalues String vector containing hexadecimal values.
#' @return String vector containing the related binary bits, or NA where the provided
#' string is invalid (not a hex value representation).
#' @examples
#' hexdat <- replicate(10, paste(format.hexmode(sample(16,4)-1),collapse=''))
#' hextobinary(hexdat)
#' @family conversion functions
#' @export
hextobinary <- function(hexvalues) {
  bin <- apply(outer(0:15,3:0,function(x,y) x%/%(2^y)%%2),1,paste0,collapse="")
  names(bin) <- format.hexmode( 0:15 )
  binaryvals=sapply( strsplit(hexvalues,'') , function(x) paste0( bin[x], collapse='' ) )
  binaryvals=ifelse(str_detect(binaryvals,"NA"),NA,binaryvals)
  return(binaryvals)
}

#' Find bits in binary
#'
#' Find which bits in a binary string are set to 1.
#' @param binaryvalues String vector containing binary bits.
#' @return List with the locations which bits are set to 1. If no bits are set,
#' or the provided binary value is invalid, the list value contains \code{integer(0)}.
#' @examples
#' bitsinbinary(c("0010","0110"))
#' bitsinbinary(c("1010","string","0110"))
#' @family conversion functions
#' @export
bitsinbinary <- function(binaryvalues) {
  binvals = sapply(lapply(strsplit(binaryvalues, NULL), rev), paste, collapse="")
  thebits=lapply(strsplit(binvals,''),function(x) which(x=="1"))
  return(thebits)
}

#' Converts degrees-minutes
#'
#' Converts longitude or latitude in degrees-minutes to degrees-decimals.
#' @param degrees The input longitude or latitude in degrees-minutes.
#' @return The longitude or latitude in degrees-decimals.
#' @family conversion functions
#' @export
convertcoords_degrees2decimal <- function(degrees){
  # X is a longitude or latitue in degrees-minutes.
  # X_new is a longitude or latitue in degrees-decimals.
  #
  # Waddenzee bij Duitsland (Google maps)
  # LAT 53.596697,
  # LNG 7.046938
  #
  # Grens Belgie/Nederland aan Noordzeekust (Google maps)
  # LAT 51.376190,
  # LNG 3.317080
  # Dutch LAT should be in the integer range 51 to 54)
  # Dutch LNG should be in the integer range  3 to  7)
  x_floor = floor(degrees)
  x_new = x_floor + (degrees-x_floor)*100/60
  return(x_new)
}

#' Recast timezone
#'
#' Transform a timezone to another but keep the exact time the same (no calculation). This one is slow.
#' @param x The POSIXct time to change
#' @param tz The timezone to transform to
#' @return The POSIXct time in the new timezone
#' @export
recastPOSIXct <- function(x, tz) {
  #from: https://stackoverflow.com/a/38262243/2710064
  return(as.POSIXct(as.character(x), origin = as.POSIXct("1970-01-01"), tz = tz))
}

#' Recast timezone (quick)
#'
#' Transform a timezone to another but keep the time the same (no calculation).
#' Uses a numeric difference calculation based on the timezone time difference
#' of the first item instead of character transformations, so that transformation
#' is faster. This might not always be the right way when dealing with timezone
#' values that have a change in daylight saving time.
#' @param x The POSIXct time to change
#' @param tz The timezone to transform to
#' @return The POSIXct time in the new timezone
#' @export
recasttimezone <- function(x, tz) {
  oldvalue=as.numeric(x[1])
  casted=as.POSIXct(as.character(x[1]), origin = as.POSIXct("1970-01-01"), tz = tz)     #from: https://stackoverflow.com/a/38262243/2710064
  newvalue=as.numeric(casted)
  difference=oldvalue-newvalue
  casted=x-difference
  attr(casted,"tzone")=tz
  return(casted)
}
