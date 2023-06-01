#This file implements vector functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# VECTOR FUNCTIONS
#####################################

#' Extend vector
#'
#' Ensure a certain length of the vector is maintained, by cutting it
#' short or extending it to the required length with a specific single value.
#' @param vec The vector to set the length for.
#' @param len The length the vector needs to be.
#' @param extval The value to use in case the provided \code{vec} is shorter
#' than the required \code{len}. Default=\code{NA}.
#' @return The vector having the required lenght, extended if needed.
#' @family vector functions
#' @export
makelength <- function(vec,len,extval=NA) {
  aa = rep(vec, length.out=len)
  if (length(vec)<len) {
    aa[(length(vec)+1):len] <- extval
  }
  return(aa)
}

#' Extend vector
#'
#' Ensure a certain length of the vector is maintained, by cutting it
#' short or extending it to the required length with a specific single value.
#' @param vec The vector to set the length for.
#' @param len The length the vector needs to be.
#' @param extval The value to use in case the provided \code{vec} is shorter
#' than the required \code{len}. Default=\code{NA}.
#' @return The vector having the required lenght, extended if needed.
#' @family vector functions
#' @export
extendvector <- function(vec,len,extval=NA) {
  aa = rep(vec, length.out=len)
  if (length(vec)<len) {
    aa[(length(vec)+1):len] <- extval
  }
  return(aa)
}

#' Rolling apply
#'
#' Apply a function rolling over vector data.
#' @param vec The vector to apply the function on.
#' @param width Width (or length) of the window.
#' @param FUN The function to roll over the vector. Calls the function for
#' all length pieces.
#' @family vector functions
#' @export
dorollapply <- function(vec, width, FUN)
  sapply(seq_along(vec),
         function(i) if (i < width) NA else FUN(vec[i:(i-width+1)]))


#' Replace elements in a vector
#'
#' Replace elements in a vector by values from another (equal length) vector based on a vector condition.
#' @param input The input vector from which elements need to be replaced.
#' @param condition A condition or TRUE/FALSE vector that specifies which
#' elements need to be replaced (need to be of equal length as input).
#' @param values A vector (need to be of equal length as input) for which
#' values need to be taken where the condition is TRUE.
#' @return New vector in which the elements of input are replaced with elements
#' from values for which condition is TRUE.
#' @family vector functions
#' @export
replaceelements <- function(input,condition,values) {
  inverted = !condition
  output = input*inverted + values*condition
  return(output)
}

#' Replace NA in a vector
#'
#' Replace all \code{NA} values in a vector by a single value.
#' @param input The input vector from which \code{NA} elements need to be replaced.
#' @param value The value to use instead of \code{NA} Default=\code{0}.
#' @return New vector in which \code{NA} is replaced by the value.
#' @family vector functions
#' @export
replaceNAelements <- function(input,value=0) {
  input[is.na(input)] = value
  return(input)
}

#' Get the same vector elements
#'
#' For multiple vectors, get the vector that contains the same values that are within all provided vectors.
#' @param a First vector
#' @param b Second vector
#' @param ... any further vectors
#' @return Vector containing elements that were in all provided vectors.
#' @example
#' intersect_all(c(2,3,5,6),c(3,6,5,8),c(2,5,7,3))
#' #returns c(3,5)
intersect_all <- function(a,b,...) {
  #from https://stackoverflow.com/a/37475390/2710064
  Reduce(intersect, list(a,b,...))
}

#' Shift vector values
#'
#' Shift vector values to the left or right.
#' @param x Vector for which to shift values.
#' @param n Number of places to be shifted.
#'
#' Positive numbers will shift to the right while negative numbers will shift to the left.
#' This direction can be inverted by the invert parameter.
#' @param invert Whether shift direction should be inverted.
#' @param default The value that should be inserted by default (at the empty spots).
#' @return The shifted vector.
#' @family vector functions
#' @export
shift <- function(x, n, invert=FALSE, default=NA){
  stopifnot(length(x)>=n)
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}

#' Create chunks
#'
#' Split a vector or list into chunks of a provided length.
#' @param x The vector or list to split
#' @param n The maximum length the vector or list is allowed to be.
#' @return Vector or list split into chunks, as unnamed lists.
#' @family vector functions
#' @export
chunk <- function(x,n) {
  split(x, ceiling(seq_along(x)/n))
}

#' Leave chunks
#'
#' For a vector, leave only those elements where a certain value occurs for
#' at least a certain amount of time in a chunk.
#' @param vals Vector to check for chunks.
#' @param chunksize Minimum size of a chunk.
#' @param replaceval Replace values of too small chunks in the vector with this value. Default=\code{NA},
#' which replaces all chunks that are too small with \code{NA}. If \code{NULL}, it will automatically check
#' the class of the vector, and uses \code{FALSE} for booleans and \code{0} for numeric or integer vectors.
#' @return A vector where the length of chunks having the same value is larger than the provided \code{chunksize}.
#' Too small chunks have been replaced with the \code{replaceval}.
#' @family vector functions
#' @export
leaveonlychunks <- function(vals,chunksize,replaceval=NA) {
  if (is.null(replaceval)) {
    if (class(vals)=="boolean") {
      replaceval=FALSE
    } else if (class(vals)=="numeric" | class(vals)=="integer") {
      replaceval=0
    }
  }
  rleresult=rle(vals)
  toosmallchunks=which(rleresult$lengths<chunksize & rleresult$values)         #get the indices of chunks that are too small
  rleresult$values[toosmallchunks]=replaceval
  retbool=inverse.rle(rleresult)
  return(retbool)
}

#' Find constant value
#'
#' Within a numeric vector, find the locations of a window for which the values can be considered
#' constant according to the provided standard deviation level settings.
#' @param thevector Numeric vector to search in.
#' @param minpoints Length of the window that requires to be constant. Default=\code{20}.
#' @param stdlevel The maximum standard deviation of the window. Any standard
#' deviation above this level will be considered not constant. Default=\code{0.05}.
#' @param desiredvalue If provided, it will search for the vector windows for the closest
#' desired average value with a standard deviation below the provided \code{stdlevel}.
#' Default=\code{NULL}, which omits the search for a desired value.
#' @param desiredvaluecloseness Value to determine if the provided \code{desiredvalue}
#' is close enough to the value found. It will check if the window average is between
#' \code{desiredvalue-desiredvaluecloseness} and \code{desiredvalue+desiredvaluecloseness} to
#' be a match. Default=\code{0.05}.
#' @return A list with the following items:
#' \describe{
#' \item{locations}{If \code{desiredvalue} is provided, the vector locations which result in the
#' closest average \code{desiredvalue} with at least the standard deviation below \code{stdlevel}.
#' Otherwise, the vector locations that provide the lowest standard deviation in the window,
#' at least below the provided \code{stdlevel}.}
#' \item{average}{The calculated average for the vector locations.}
#' \item{standarddeviation}{The calculated standard deviation for the vector locations.}
#' }
#' If no window below \code{stdlevel} is found, or if the desired average value is not found, or the closest
#' desired average value is not below \code{stdlevel}, \code{NULL} returns.
#' @family vector functions
#' @export
findconstant <- function(thevector,
                         minpoints=20,
                         stdlevel=0.05,
                         desiredvalue=NULL,
                         desiredvaluecloseness=0.05) {
  if (length(thevector)<2 | length(thevector)<=minpoints) {
    return(NULL)
  }
  if (all(is.na(thevector))) {
    return(NULL)
  }
  #calculate running average and running standard deviation
  runningavg=runavg(thevector,minpoints)
  runningsd=runsd(thevector,minpoints)

  #determine matching
  theloc=NULL
  searchvalues=runningavg[!is.na(runningsd) & runningsd<=stdlevel]      #extract those averages for which the std matches the level
  if (length(searchvalues)) {
    if (!is.null(desiredvalue)) {                                       #desired value provided
      difwdes=abs(searchvalues-desiredvalue)                            #calculate difference with calculated averages
      themin=min(difwdes,na.rm = TRUE)
      if (themin<=desiredvaluecloseness) {                              #match found
        theloc=min(which(difwdes==themin))                              #find first location in matching vector (as there could be more than one)
        theloc=min(which(runningavg==searchvalues[theloc]))             #find location in main vector
      }
    } else {
      theloc=which.min(runningsd)                                       #location with minimum std
    }
    if (!is.null(theloc)) {
      retlist=NULL
      retlist$average=runningavg[theloc]
      retlist$standarddeviation=runningsd[theloc]
      retlist$locations=c((theloc-minpoints+1):theloc)                  #the locations for which the constant was calculated
      return(retlist)
    }
  }
  return(NULL)
}

#' Find constant value chunks
#'
#' Within a numeric vector, find all locations of a window for which the values can be considered
#' constant according to the provided standard deviation level settings.
#'
#' In contrast to the \code{\link{findconstant}} function, which either returns a constant window
#' chunk for a provided desired value or the 'most constant' chunk (based on the lowest standard
#' deviation), this function returns all possible chunks in a vector that can be considered constant.
#' @param thevector Numeric vector to search in.
#' @param minpoints Length of the window that requires to be constant. Default=\code{20}.
#' @param stdlevel The maximum standard deviation of the window. Any standard
#' deviation above this level will be considered not constant. Default=\code{0.05}.
#' @return A data.table with one row for each chunk, with the following column names:
#' \describe{
#' \item{chunksize}{The amount of points in \code{thevector} that refers to this chunk.}
#' \item{chunkstartloc}{The very first location of \code{thevector} that refers to the start of
#' this window chunk.}
#' \item{chunkendloc}{The very last location of \code{thevector} that refers to the end of
#' this window chunk.}
#' \item{chunkaverage}{The average value of this chunk.}
#' \item{chunkstandarddeviation}{The standard deviation of this chunk (denominator \emph{n}).}
#' \item{minstdstartloc}{The very first location of \code{thevector} that provide the lowest
#' standard deviation in the chunk calculated for a window of size \code{minpoints}.}
#' \item{minstdendloc}{The very last location of \code{thevector} that provide the lowest
#' standard deviation in the chunk calculated for a window of size \code{minpoints}.}
#' \item{minstdaverage}{The calculated average value for the vector locations that
#' provide the lowest standard deviation in the chunk.}
#' \item{minstdstandarddeviation}{The standard deviation for the vector locations
#' that provide the lowest standard deviation in the chunk (denominator \emph{n})
#' calculated for a window of size \code{minpoints}.}
#' }
#' If no window below \code{stdlevel} is found, \code{NULL} returns.
#' @family vector functions
#' @export
findconstantchunks <- function(thevector,
                               minpoints=20,
                               stdlevel=0.05) {
  if (length(thevector)<2 | length(thevector)<=minpoints) {
    return(NULL)
  }
  if (all(is.na(thevector))) {
    return(NULL)
  }
  #calculate running average and running standard deviation
  runningavg=runavg(thevector,minpoints)
  runningsum=as.vector(stats::filter(thevector^2,rep(1/minpoints,minpoints),sides=1))
  runningsd=sqrt(runningsum-runningavg^2)

  matchingvec=(runningsd<=stdlevel)
  matchingvec=ifelse(is.na(matchingvec),FALSE,matchingvec)
  thediff=prepend(abs(diff(matchingvec)),0)
  matchgroup=cumsum(thediff)
  matchgroup=ifelse(matchingvec,matchgroup,0)
  counter=as.data.table(plyr::count(matchgroup))
  counter=counter[x>0,]

  if (nrow(counter)) {
    rettable=data.table()
    for (i in 1:nrow(counter)) {
      internallist=NULL
      internallist$chunksize=counter[i,freq]+minpoints
      thelocs=which(matchgroup==counter[i,x])                                 #locations with are considered this group
      internallist$chunkstartloc=min(thelocs)-minpoints+1
      internallist$chunkendloc=max(thelocs)
      chunkvalues=thevector[internallist$chunkstart:internallist$chunkend]
      internallist$chunkaverage=mean(chunkvalues)
      internallist$chunkstandarddeviation=sqrt(sum((chunkvalues - internallist$chunkaverage)^2) / length(chunkvalues))
      thelocs=which.min(ifelse(matchgroup==counter[i,x], runningsd, NA))      #location with minimum std
      internallist$minstdstartloc=thelocs-minpoints+1
      internallist$minstdendloc=thelocs
      internallist$minstdaverage=runningavg[thelocs]
      internallist$minstdstandarddeviation=runningsd[thelocs]
      rettable=rbind(rettable,internallist)
    }
    return(rettable)
  }
  return(NULL)
}
