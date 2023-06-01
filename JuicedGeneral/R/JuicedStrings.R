#This file implements string functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# STRING FUNCTIONS
#####################################

#' Pad month with 0
#'
#' Simple function to pad zeros to a month
#' @param themonth Month value or string to pad
#' @return String with month value padded (always zero padded left, length 2)
#' @family string functions
#' @export
str_pad_month <- function(themonth) {
  return(str_pad(themonth,2,"left","0"))
}

#' String collapse
#'
#' Collapse a string with a provided collapse string. Different from \code{\link[base]{paste}}
#' in that the collapse string character is first.
#' @param collapsestring The string to connect all items.
#' @family string functions
#' @export
paste0collapse <- function(collapsestring, ...) {
  return(paste0(list(...), collapse = collapsestring))
}

#' Get domain
#' Get domain without http or www elements
#' @param x Vector containing the strings to get domains from
#' @return Vector of the domains
#' @family string functions
#' @export
domain <- function(x) {
  #modified from https://stackoverflow.com/a/19021142/2710064
  sapply(strsplit(gsub("http://|https://|www\\.", "", tolower(x)), "/"),"[[",1)
}
