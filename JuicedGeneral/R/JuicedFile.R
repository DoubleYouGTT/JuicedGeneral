#This file implements file functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# FILE FUNCTIONS
#####################################

#' Get file name
#'
#' Get the file name from a full path.
#' @param fullpaths The full paths to get file names from.
#' @param excludeextension Set to \code{TRUE} to exclude the extension from the returned filenames..
#' Default=\code{FALSE}, which includes the extension.
#' @return List of file names.
#' @family file functions
#' @export
getfilenames <- function(fullpaths,excludeextension=FALSE){
  thefilenames=basename(fullpaths)
  if (excludeextension) {                                           #if extension is to be excluded, take it away
    thefilenames=tools::file_path_sans_ext(thefilenames)

    #original below, which does not deal well with dots in filenames
    #help_function <- function(fullpath)(unlist(str_split(fullpath,fixed("."))))[1]
    #thefilenames = unlist(lapply(thefilenames, help_function))
  }
  return(thefilenames)
}

#' Get file extension
#'
#' Get the file extension from a full path or filename.
#' @param fullpaths The full paths or file name to get extensions from.
#' @return List of extensions.
#' @family file functions
#' @export
getfileextensions <- function(fullpaths) {
  return(tools::file_ext(fullpaths))

  #original: works too
  #help_function <- function(fullpath)rev(unlist(str_split(fullpath,fixed("."))))[1]
  #extensions = unlist(lapply(fullpaths, help_function))
  return(extensions)
}

#' Get directory file list
#'
#' Get a list of files in a specific directory.
#' @param dirtocheck The directory to obtain the files from.
#' @param fileextension One of more strings specifying the extensions (without dot, such as \code{"zip"}) of the files to return.
#' Default=\code{NULL}, which returns all files.
#' @param includesubdirs Set to \code{TRUE} to also include the files in all subdirectories.
#' Default=\code{FALSE}, which only checks the cirrent directory.
#' @param printmessage Set to \code{TRUE} to display the amount of files found in the directory in a message.
#' Default=\code{FALSE}, which does not print a message.
#' @return Data.table with the files, containing the columns FULLPATH (full non-relative path), DIRONLY (directory of the file),
#' FILENAME (file name without extension), and EXTENSION (file extension).
#' @family file functions
#' @export
getfiletable <- function(dirtocheck,
                         fileextension=NULL,
                         includesubdirs=FALSE,
                         printmessage=FALSE) {
  thefiles = NULL
  if (length(fileextension)) {
    for (ext in fileextension) {
      ext=paste0("*.",ext)
      thefiles = c(thefiles, list.files(dirtocheck,pattern = ext, full.names = TRUE, recursive = includesubdirs) )
    }
  } else {
    thefiles = c(thefiles, list.files(dirtocheck,pattern = NULL, full.names = TRUE, recursive = includesubdirs) )
  }
  thefiles=normalizePath(thefiles,winslash = "/", mustWork = FALSE)
  if (length(thefiles)) {
    if (printmessage) message(paste0("Found ",length(thefiles)," matching files in the provided directory."))
    thefiletable = as.data.table(file.info(thefiles), keep.rownames="FULLPATH")
    thefiletable[,DIRONLY:=paste0(dirname(thefiles),"/")]
    thefiletable[,FILENAME:=getfilenames(thefiles,excludeextension = TRUE)]
    thefiletable[,EXTENSION:=getfileextensions(thefiles)]
    thefiletable=renamecolumns(thefiletable,c("size","isdir","exe","mode","mtime","ctime","atime"),c("BYTES","ISDIR","ISEXE","FILEMODE","MODIFICATIONTIME","CHANGETIME","ACCESSTIME"))
    thefiletable=setcolorder(thefiletable,c("FULLPATH","DIRONLY","FILENAME","EXTENSION","BYTES","FILEMODE","MODIFICATIONTIME","CHANGETIME","ACCESSTIME","ISDIR","ISEXE"))
  } else {
    thefiletable=data.table()
  }
  return(thefiletable)
}
