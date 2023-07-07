# Make a package automatically.

# author: Joost Kuckartz, jkuckartz1984@hotmail.com
# modified: July 2023
# version: 1.0

#########
# USAGE #
#########
# Before using this file, it's recommended you update existing packages.
#
# Step 1. Place this file plus "makepackagevariables.R" in the folder containing
# the R files to make a package out of it, or place it in the directory
# containing the folder named "R".
#
# Sample 1:
# C:/
# C:/code/               <-- place in this folder
# C:/code/R/
# C:/code/R/script1.R
# C:/code/R/script2.R
# 
# Sample 2:
# C:/
# C:/code/               <-- place in this folder
# C:/code/script1.R
# C:/code/script2.R
#
# Step 2: Source this file

#############
# VARIABLES #
#############

rm(list=ls())

# To edit the package variables, edit the 'makepackagevariables.R' file.
# Keeping one file for each package is the most useful.
miktexinstalled = FALSE        #set to true if you have MiKTeX installed to make a pdf manual

##########
# SCRIPT #
##########

#' Load a library. If it is not yet installed, it will install it.
#' @param x The library to load
loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dependencies=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#' Detach all package versions based on specific name. From https://stackoverflow.com/a/6979989/2710064
#' @param pkg The package or its string name to unload
#' @param character.only Set to \code{TRUE} if the provided name is a string. Default=\code{FALSE}.
#' @examples \dontrun{
#' detach_package(vegan)
#' detach_package("vegan", TRUE)
#' }
detach_package <- function(pkg, character.only = FALSE) {
  if(!character.only) {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search()) {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

loadlibrary("devtools")
loadlibrary("roxygen2")
loadlibrary("stringr")
loadlibrary("data.table")

rm(list = c("loadlibrary"))                                  #no need for the function anymore so no conflict warning message at the end

#prepare move files settings
copydelete=TRUE
curdir=getSrcDirectory(function(x) {x})                      #only works if this file is run or sourced!
setwd(curdir)
if (basename(getwd())=="R") {                                #if this directory is "R"
  if (file.exists("../DESCRIPTION")) {                       #and there is a DESCRIPTION file
    setwd("..")                                              #we already have a package directory structure
	  copydelete=FALSE
  }
}
curdir=getwd()

#load package variables
source("makepackagevariables.R")

#move source files to right directory
newdir=paste0(curdir,"/R")
dir.create(newdir, showWarnings = FALSE)
if (copydelete) {
  rfiles=list.files(pattern=".R", include.dirs=FALSE)       #list R files
  locs=is.na(str_locate(rfiles,"makepackagehere")[,1])
  rfiles=rfiles[locs]                                       #avoid makepackagehere file
  locs=is.na(str_locate(rfiles,"makepackagevariables")[,1])
  rfiles=rfiles[locs]                                       #avoid makepackagevariables file
  locs=is.na(str_locate(rfiles,"installthispackage")[,1])
  rfiles=rfiles[locs]                                       #avoid installthispackage file
  message(paste0("Moving ",length(rfiles)," source files..."))
  file.rename(paste0(curdir,"/",rfiles),paste0(newdir,"/",rfiles))    #move files to "./R" directory
}

#clean up existing package files for recreation (if present)
isnewpackage=TRUE
chkfile=paste0(curdir,"/DESCRIPTION")
if (file.exists(chkfile)) {
  isnewpackage=FALSE
  file.remove(chkfile)                                    #remove DESCRIPTION file to update it
}
chkfile=paste0(curdir,"/NAMESPACE")
if (file.exists(chkfile)) {
  isnewpackage=FALSE
  file.remove(chkfile)                                    #remove NAMESPACE file to update it
}
chkfile=paste0(curdir,"/",packagename,".Rproj")
if (file.exists(chkfile)) {
  file.remove(chkfile)                                    #remove Rproj file
}

#prepare description information
if (length(packageauthors)==1) {
  authorstring=format(packageauthors[[1]], style="R")
} else if (length(packageauthors)> 1) {
  authorstring="c("                                         #start with list item
  for (i in 1:length(packageauthors)) {
    if (i>1) {
      authorstring=paste0(authorstring,",")                 #add comma's to add the next person to list
    }
    rformatauthor=paste0(format(packageauthors[[i]], style="R"),collapse="")  #add the person as R executable text
    authorstring=paste0(authorstring,rformatauthor)
  }
  authorstring=paste0(authorstring,")")                     #close the list
}

existingpackage=as.data.table(installed.packages())         #extract build version from package info (if exists)
existingpackage=existingpackage[Package==packagename,]
packageversion_build = 1
if (nrow(existingpackage)) {
  packageversion = existingpackage[,Version]
  packageversion = str_split(packageversion,"[.]")[[1]]
  curversionmajor=as.numeric(packageversion[1])
  if (is.na(curversionmajor)) {
    curversionmajor=0
  }
  curversionminor=as.numeric(packageversion[2])
  if (is.na(curversionminor)) {
    curversionminor=0
  }
  if (curversionmajor==packageversion_major & curversionminor==packageversion_minor) {
    packageversion_build=as.numeric(packageversion[3])
    if (is.na(packageversion_build)) {
      packageversion_build=0
    }
    packageversion_build = packageversion_build + 1
  }
}

packagedesc=list(                                           #create package description items
  Package = packagename,
  Title = packagetitle,
  Version = paste0(packageversion_major,".",packageversion_minor,".",packageversion_build),
  Date = strftime(Sys.Date(),"%Y-%m-%d"),
  `Authors@R` = authorstring,
  Description = packagedesctext,
  Depends = paste0("R (>= ", as.character(getRversion()), ")"),
  License = packagelicense,
  Encoding = "UTF-8", 
  LazyData = "true"
)

#prepare package directory and settings and create it
if (isnewpackage) {                                         #if this is a new package
  message(paste0("Creating package ",packagename,"..."))
} else {
  message(paste0("Updating package ",packagename,"..."))
}

#trick to avoid question for nesting
challenge_nested_project <- function(path, name) return()
rlang::env_unlock(env = asNamespace('usethis'))
rlang::env_binding_unlock(env = asNamespace('usethis'))
assign('challenge_nested_project', challenge_nested_project, envir = asNamespace('usethis'))
rlang::env_binding_lock(env = asNamespace('usethis'))
rlang::env_lock(asNamespace('usethis'))

#create package 
usethis::create_package(curdir,packagedesc,open=FALSE)

#install and document package
message(paste0("Installing package ",packagename,"..."))
document(".")                                                #create documentation for all R files in the folder
detach_package(packagename,TRUE)                             #ensure the package is not loaded just in case.
install.packages(".", repos = NULL, dependencies = TRUE, type = "source") #install this package and its dependencies

#make pdf documentation file
if (miktexinstalled) {
  message(paste0("Creating documentation pdf for package ",packagename,"..."))
  if (file.exists(paste0(packagename,".pdf"))) {
    file.remove(paste0(packagename,".pdf"))
  }
  path <- find.package(packagename)
  system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
}
message("All done! Best to close and reopen the R application.")