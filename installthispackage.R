# Install the development package(s) found in this directory.

# author: Joost Kuckartz, jkuckartz1984@hotmail.com
# modified: July 2023
# version: 1.0

#########
# USAGE #
#########
# Before using this file, it's recommended you update existing packages.
#
# Step 1. Place this file in the parent folder of an R package (a subfolder 'R'
# and possibly 'man' exists), or place it in the folder named 'R' containing all
# code of the package.
#
# Sample 1:
# C:/
# C:/code/              <-- place in this folder
# C:/code/R/
# C:/code/R/script1.R
# C:/code/R/script2.R
# 
# Sample 1:
# C:/
# C:/code/
# C:/code/R/            <-- place in this folder
# C:/code/R/script1.R
# C:/code/R/script2.R
#
# Step 2: Source this file

rm(list=ls())                           #clear memory

###################
# INSTALL PACKAGE #
###################

#' Load a library. If it is not yet installed, it will install it.
#' @param x The library to load
loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("devtools")

thisfiledir=getSrcDirectory(function(x) {x})                 #only works if this file is run or sourced!
setwd(thisfiledir)
if (basename(getwd())=="R") {                                #if this directory is "R"
  if (file.exists("../DESCRIPTION")) {                       #and there is a DESCRIPTION file
    setwd("..")                                              #we're in the package source code
  }
}

install(getwd(), dependencies = TRUE, upgrade = "always")  #this directory is the package