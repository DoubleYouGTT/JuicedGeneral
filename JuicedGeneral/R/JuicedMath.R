#This file implements math functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# MATH
#####################################

#' In between
#'
#' Check if a value is in between minimum and maximum value
#' @param value The value to check
#' @param minval The minimum value of the range
#' @param maxval The maximum value of the range
#' @return \code{TRUE} if the value is in between the range (minimum and maximum
#' values included), \code{FALSE} if this is not the case.
#' @family
#' @export
inbetween <- function(value,minval,maxval) {
  if (value >= minval & value <= maxval) {
    return(TRUE)
  }
  return(FALSE)
}

#' Radians to degrees
#' @param rad Radians.
#' @return Degrees.
#' @family math functions
#' @export
rad2deg <- function(rad) {(rad * 180) / (pi)}

#' Degrees to radians
#' @param deg Degrees.
#' @return Radians.
#' @family math functions
#' @export
deg2rad <- function(deg) {(deg * pi) / (180)}

#' Running average
#'
#' Calculate running average.
#' @param vec The vector to calculate the running average for.
#' @param width Width of the window.
#' @return Vector of the same length as \code{vec} with the running average.
#' @family math functions
#' @family vector functions
#' @export
runavg <- function(vec, width) {
  return(as.vector(stats::filter(vec,rep(1/width,width),sides=1)))
}

#' Running average weigthed
#'
#' Calculate running average with weighing factors.
#' @param vec The vector to calculate the running average for.
#' @param weighvals Vector containing the weighing values (length of vector indicates the amount of previous points to use for calculation).
#' @return Vector of the same length as \code{vec} with the running average.
#' @family math functions
#' @family vector functions
#' @export
runavgweighed <- function(vec, weighvals) {
  weighvals=rev(weighvals)
  return(as.vector(stats::filter(vec,weighvals,sides=1)))
}

#' Running standard deviation
#'
#' Calculate running standard deviation. In constract with \code{\link[stats]{sd}}, uses denominator \emph{n}.
#' @param vec The vector to calculate the running standard deviation for.
#' @param width Width of the window.
#' @return Vector of the same length as \code{vec} with the running standard deviation.
#' @family math functions
#' @family vector functions
#' @export
runsd <- function(vec, width) {
  dorollapply <- function(v, w, FUN)
    sapply(seq_along(v),
           function(i) if (i < w) NA else FUN(v[i:(i-w+1)]))

  return(dorollapply(vec,width,sd))
}

#' Linear extrapolation
#' @param x The values on the x axis.
#' @param y Corresponding values on the y axis.
#' @param xi The extrapolated x axis value to calculate a new y value for.
#' @return The calculated extrapolated y value.
#' @family predictors
#' @export
linearextrapolate <-function(x,y,xi){
  yi = NA
  if (length(x)>4) {
    x= as.numeric(x)
    y= as.numeric(y)
    rechte_lijn = lm(y ~ x + 1)
    b = as.numeric(coef(rechte_lijn))[1]
    a = as.numeric(coef(rechte_lijn))[2]
    yi = a*(xi) + b
  }
  return(yi)
}

#' Linear interpolation with normalization
#'
#' Perform linear interpolation and normalization in between a minimum and maximum value.
#' The minimum value and below represent 0, the maximum value and above represent 1, unless \code{inverted=TRUE}, which reverses
#' this logic.
#' @param inputvector The values to calculate on.
#' @param minval The minimum value.
#' @param maxval The maximum value.
#' @param inverted Set to \code{TRUE} to invert the result, so that the maximum value and above represent 0 and the minimum value and below represent 1
#' (default=\code{FALSE}, which uses the minimum value and below to represent 0 and the maximum value and above to represent 1).
#' @family predictors
#' @export
linearcalc <- function(inputvector,minval,maxval,inverted=FALSE) {
  if (inverted) {
    retvec = ifelse(inputvector<minval,1,ifelse(inputvector>maxval,0,1-((inputvector-minval) / (maxval-minval))))
  } else {
    retvec = ifelse(inputvector<minval,0,ifelse(inputvector>maxval,1,(inputvector-minval) / (maxval-minval)))
  }
  return(retvec)
}

