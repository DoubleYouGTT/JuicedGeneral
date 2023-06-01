#This file implements loading functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# LOADING FUNCTIONS
#####################################

`%!in%` = Negate(`%in%`)

#' Load a library
#'
#' Load a specific named library. If it is not yet installed, it will install it.
#' @param x Character string of the library to load.
#' @family loading functions
#' @export
loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#' Load dependencies
#'
#' For a development package, only load the dependencies.
#' @param dir The directory of the development package.
#' @family loading functions
#' @export
loaddependentpackages <- function(dir) {
  deppackages=devtools::dev_package_deps(dir, dependencies = TRUE)$package
  if (length(deppackages)) {
    for (i in 1:length(deppackages)) {
      loadlibrary(deppackages[i])
    }
  }
}

#' Load source files
#'
#' Load all source files in a specific directory. Can be used to source a package, allowing direct package
#' editing while running it. Of course, after code modification the package needs to be built and installed
#' again.
#' @param dir The directory to load the source files from.
#' @param openeditor Set to \code{TRUE} to open the source files in the editor. In RStudio, this allows debugging
#' of the functions through automatic error breakpoints or using the F2 function lookup shortcut followed by direct
#' editing. Default=\code{FALSE}, which only sources the files without opening an editor.
#' @param loaddependencies Set to \code{TRUE} to load all libraries that the source package depends on. This allows
#' for the loading of the source plus all dependent libraries, without actually calling \code{library} of the actual
#' package. This allows for setting breakpoints and debugging the opened source code without RStudio complaining that
#' an updated package is needed. Default=\code{FALSE}, which does not call dependent libraries.
#' @family loading functions
#' @export
loadsource <- function(dir,
                       openeditor=FALSE,
                       loaddependencies=FALSE) {
  if (loaddependencies) {
    loaddependentpackages(dir)
  }
  rfiles=list.files(dir, full.names=TRUE, pattern=".R", include.dirs=FALSE)            #list R files
  if (length(rfiles)) {
    locs=is.na(str_locate(rfiles,"makepackagehere")[,1])
    rfiles=rfiles[locs]                                               #avoid makepackagehere
    packname=NULL
    if (basename(dir)=="R") {                                         #if this directory is "R"
      packdir=dirname(dir)                                            #effectively do ..
      if (file.exists(paste0(packdir,"/DESCRIPTION"))) {              #and there is a DESCRIPTION file
        packname=basename(packdir)                                    #we have a package source directory
      }
    }
    if (!is.null(packname)) {                                         #if we're sourcing a package
      locs=getfilenames(rfiles)!=paste0(packname,".R")                #do not source the code having the same name as the package
      rfiles=rfiles[locs]
    }
    for (i in 1:length(rfiles)) {                                     #loop over the files
      source(file=rfiles[i])                                          #source the file
      if (openeditor) {
        file.edit(rfiles[i])                                          #open in editor if needed
      }
    }
  }
}

#' Load a development library
#'
#' Load a specific named development library.
#' @param x Character string of the library to load.
#' @param musthaveversion The version that the package must have (default="0.0.0")
#' @family loading functions
#' @export
loaddevelopmentlibrary <- function(x, musthaveversion="0.0.0") {
  if (x %in% rownames(installed.packages())) {                        #check if dev package exists
    curversion=packageVersion(x)
    if (curversion>=musthaveversion) {                                #check if right version is installed
      if (!library(x,character.only = TRUE)) {                        #load it
        stop(paste0("Development package '",x,"' could not be loaded."))
      }
    } else {
      stop(paste0("Development package '",x,"' is not up to date: ",packageVersion(x)," is installed but ",musthaveversion,"is needed."))
    }
  } else {
    stop(paste0("Development package '",x,"' not found."))
  }
}
