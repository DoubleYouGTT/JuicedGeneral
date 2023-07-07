#This file implements list functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# LIST FUNCTIONS
#####################################

#' Strip string of named elements
#'
#' Remove all specific named elements from a list. Executes recursively so
#' that it also works within lists in lists.
#' @param thelist List to remove names from.
#' @param thenames Vector of strings containing the names of the elements to remove from the list.
#' @return List with all the named elements removed.
#' @family list functions
#' @export
stripnames <- function(thelist, thenames) {
  #function from https://stackoverflow.com/a/37855200/2710064
  thisdepth <- listdepth(thelist)
  if (thisdepth == 0) {
    return(thelist)
  } else if (length(nameIndex <- which(names(thelist) %in% thenames))) {
    thelist <- thelist[-nameIndex]
  }
  return(lapply(thelist, stripnames, thenames))
}

#' Leave list with only names
#'
#' Removes all named elements except the provided names from a list. If none of the
#' provided names are found within the list, the full list is kept (therefore keeping
#' the list structure).
#' Executes recursively so that it also works within lists in lists.
#' @param thelist List to only leave the provided names in.
#' @param thenames Vector of strings containing the names of the elements to leave in the list.
#' @return List with only the named elements remaining.
#' @family list functions
#' @export
leavenames <- function(thelist, thenames) {
  #function from https://stackoverflow.com/a/37855200/2710064
  thisdepth <- listdepth(thelist)
  if (thisdepth == 0) {
    return(thelist)
  } else if (length(nameIndex <- which(names(thelist) %in% thenames))) {
    thelist <- thelist[nameIndex]
  }
  return(lapply(thelist, leavenames, thenames))
}

#' Find list depth
#'
#' Find the depth of a list, which is an integer value of the
#' furthest recursion into the lists in lists.
#' @param thelist List to find the depth for.
#' @param thisdepth Used for recursion to find the depth but can be modified
#' if you provide a list within list already. Default=\code{0}.
#' @return Integer that identifies the depth of a list.
#' @family list functions
#' @export
listdepth <- function(thelist, thisdepth=0) {
  #function from http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
  if(is.list(thelist) && length(thelist) == 0) {
    return(0)
  }
  if (!is.list(thelist)) {
    return(thisdepth)
  } else{
    return(max(unlist(lapply(thelist,listdepth,thisdepth=thisdepth+1))))
  }
}

#' Force all values to be of list type
#'
#' Recursively go over the list and transform all values to be of list type.
#' @param thelist The list to traverse.
#' @return List in which all values have been put into a list.
#' @family list functions
#' @export
forcelisttype <- function(thelist) {
  return(lapply(thelist, function(x) lapply(x, function(x2) if(is.list(x2)) list(x2) else x2)))
}
