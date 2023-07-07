#This file implements data.table functions
#modified: July 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com

#####################################
# DATA TABLES
#####################################

#' Load a SAS data table
#'
#' Load a SAS data table from file and return it as a data.table.
#'
#' Depending on the size of the SAS table, this function can take a while to finish. It does not have a progress indicator.
#' @param dir Directory in which the file can be found. Must end with '/'.
#' @param fname File name of the file (without extension).
#' @return Data.table on success, NULL on failure.
#' @family loading functions
#' @family data.table functions
#' @export
loadsastable <- function(dir,fname) {
  file = paste0(dir,fname,".sas7bdat")     #concat full file path
  if(file.exists(file) == FALSE) {
    message("File does not exist!")
    return(NULL)
  }

  message("Please wait while loading the SAS file '",fname,"'...")
  begintime = proc.time()
  data = setDT(haven::read_sas(file))         #load file
  endtime=proc.time()-begintime
  message(paste0("Loading finished after ",endtime[[3]]," seconds."))

  data = as.data.table(data)
  return(data)
}

#' Load a large table
#'
#' Load a large SAS or RDS data table from file, with an automatic RDS existance check,
#' and return it as a data.table. If a SAS table is loaded, it will automatically save
#' it as RDS in the same directory for future faster loading.
#'
#' Depending on the size of the SAS table, this function can take a while to finish.
#' It does not have a progress indicator.
#' @param dir Directory in which the file can be found. Must end with '/'.
#' @param fname File name of the file (without extension).
#' @param forcesas Set to TRUE if the original SAS table needs to be loaded (it otherwise uses the quicker rds file).
#' @return Data.table on success, NULL on failure.
#' @family loading functions
#' @family data.table functions
#' @export
loadlargetable <- function(dir,fname,forcesas=FALSE) {
  sasfile=paste0(dir,fname,".sas7bdat")     #concat full file path
  rdsfile=paste0(dir,fname,".rds")

  loadfile=rdsfile                          #keeps track which file to load
  if(!file.exists(rdsfile)) {               #rds file does not exist
    message("RDS file does not exist, trying to load SAS file.")
    forcesas=TRUE
  }
  if(forcesas) {
    loadfile=sasfile
    if(!file.exists(loadfile)) {            #sas file does not exist
      message("SAS file does not exist, cancelling.")
      return(NULL)
    }
  }
  message("Please wait while loading the data from '",fname,"'...")
  begintime = proc.time()
  if(forcesas) {
    data = setDT(haven::read_sas(loadfile)) #load file
    data = as.data.table(data)
    saveRDS(object=data, file = rdsfile)
  } else {
    data = readRDS(file=loadfile)
  }
  endtime=proc.time()-begintime
  message(paste0("Loading finished after ",endtime[[3]]," seconds."))

  return(data)
}

#' Load a tabular file
#'
#' Loads a tabular file and returns it as a data.table. Automatically loads the most efficient type first if available.
#' It can read the following file types:
#' RDS, CSV, XLSX, SAS
#' @param dir Directory in which the file can be found. If it does not end with '/', this directory separator will be added.
#' @param fname File name of the file (without extension).
#' @param quickstore Set to \code{FALSE} to NOT store the data to .RDS after loading.
#' Default=\code{TRUE}, which does store an RDS file that makes future loading faster.
#' @param forceallsheets Set to \code{TRUE} if all sheets of an .XLSX file need to be loaded, without asking for user input.
#' @return Data.table on success, \code{NULL} on failure. It will return a list of data.tables
#' in case an .xlsx with multiple sheets is selected to read.
#' @family loading functions
#' @family data.table functions
#' @export
loadtable <- function(dir,fname,filetype="RDS",quickstore=TRUE,forceallsheets=FALSE,...) {
  if (str_sub(dir, start=-1) !="/") {
    dir=paste0(dir,"/")
  }
  csvfile1=paste0(dir,fname,".csv")
  csvfile2=paste0(dir,fname,".CSV")
  csvfile3=paste0(dir,toupper(fname),".CSV")
  xlsxfile=paste0(dir,fname,".xlsx")
  sasfile=paste0(dir,fname,".sas7bdat")
  rdsfile=paste0(dir,fname,".rds")

  if (filetype=="RDS") {
    if(!file.exists(rdsfile)) {                                   #rds file does not exist
      message("RDS file does not exist, trying to load CSV file.")
      filetype="CSV"
    }
  }
  if (filetype=="CSV") {
    existinfo1=file.exists(csvfile1)
    existinfo2=file.exists(csvfile2)
    existinfo3=file.exists(csvfile3)
    if(! (existinfo1 & existinfo2 & existinfo3) ) {               #csv file does not exist
      message("CSV file does not exist, trying to load XLSX file.")
      filetype="XLSX"
    }
  }
  if (filetype=="XLSX") {
    if(!file.exists(xlsxfile)) {                                  #xlsx file does not exist
      message("XLSX file does not exist, trying to load SAS file.")
      filetype="SAS"
    } else {                                                      #ensure sheet selection (or all sheets)
      sheets=excel_sheets(xlsxfile)
      if (!forceallsheets && length(sheets)>1) {
        inputform=dlg_message("Multiple sheets found. Do you want to load all sheets?","yesno")$res
        if (inputform=="no")
          sheets=dlgList(sheets,multiple=TRUE,title="Which sheet(s) you want to load?")$res
      }
    }
  }
  if (filetype=="SAS") {
    if(!file.exists(sasfile)) {                                   #sas file does not exist
      message("SAS file does not exist, cancelling.")
      return(NULL)
    }
  }

  message("Please wait while loading the data from '",fname,"'...")
  begintime = proc.time()
  if (filetype=="SAS") {
    data = as.data.table(haven::read_sas(sasfile)) #load file
  } else if (filetype=="CSV") {
    if (existinfo1) data = fread(file=csvfile1)
    if (existinfo2) data = fread(file=csvfile2)
    if (existinfo3) data = fread(file=csvfile3)
  } else if (filetype=="XLSX") {
    if (length(sheets)>1) {
      data=list()
      for (i in 1:length(sheets)) {
        message("Loading ",i," of ",length(sheets)," sheets...")
        listname=sheets[i]
        data[[listname]]=as.data.table(read_xlsx(xlsxfile,sheet=sheets[i]))
      }
    } else {
      data=as.data.table(read_xlsx(xlsxfile,sheet=sheets))
    }
  } else {
    data = readRDS(file=rdsfile)
  }
  if (quickstore & filetype!="RDS") {
    saveRDS(object=data, file = rdsfile)
  }
  endtime=proc.time()-begintime
  message(paste0("Loading finished after ",endtime[[3]]," seconds."))

  return(data)
}

#' Load a RDS, CSV, XLSX or SAS file
#'
#' Loads the file and returns it as a data.table. The loading priority follows RDS, then CSV, then XLSX, then SAS.
#'
#' Depending on the size of the file, this function can take a while to finish. It does not have a progress indicator.
#' @param dir Directory in which the file can be found. If it does not end with '/', this directory separator will be added.
#' @param fname File name of the file (without extension).
#' @param filetype The file type of the file to force load ("RDS", "CSV", "XLSX", or "SAS").
#' Default = "RDS". Setting this to a either "CSV", "XLSX" or "SAS" forces a reload.
#' @param largetable Set to \code{TRUE} if this is a large table, which stores the table to RDS after loading.
#' Default=\code{FALSE}, which does not store the result in a RDS file.
#' @return Data.table on success, NULL on failure.
#' @family loading functions
#' @family data.table functions
#' @export
loadspecifictable <- function(dir,fname,filetype="RDS",largetable=FALSE) {
  return(loadtable(dir,fname,filetype,largetable))
}

#' Add a column
#'
#' Add a column to a data.table with a given name or use the variable as name (modification by reference).
#' @param data The data.table to which to add a column.
#' @param colvalues The values of the column to add (preferrably having
#' the same length as the amount of rows in the data.table).
#' @param colname The name of the new column. Default=\code{NULL}, which will
#' read the original colvalues variable name and uses that as column name.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
addcolumn <- function(data,colvalues,colname=NULL) {
  if (is.null(colname)) {
    colname=deparse(substitute(colvalues))
  }
  data[,eval(colname):=colvalues]
  invisible(data)
}

#' Set columns
#'
#' Set the columns that a data.table must have. If it does not exist, it will
#' add the column with a default value.
#' @param data The data.table which needs to have the columns.
#' @param cnames The column names that need to exist.
#' @param cval The default value for newly added columns. Default=\code{NA}.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
setcolumns <- function(data,cnames,cval=NA) {
  add <-cnames[!cnames %in% colnames(data)]

  if(length(add)!=0) data[,(add):=cval]
  invisible(data)
}

#' Rename columns
#'
#' Rename specific named columns from a data.table (if they exist).
#' @param datatable The data.table to rename the columns from.
#' @param origcols Original column names (character vector).
#' @param newcols New column names (character vector). Must be of same length as \code{origcols}.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
renamecolumns <- function(datatable,origcols,newcols) {
  invisible(setcolnames(datatable=datatable, newnames=newcols, orignames=origcols))
}

#' Move specific columns to front
#'
#' Move specific named columns to the front of a data.table, while keeping the other
#' column names in the provided order.
#' @param datatable The data.table to move columns.
#' @param columnnames The column names (character vector) to move to the front, in the order that
#' they should be.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
setcoltofront <- function(datatable,columnnames) {
  setcolorder(datatable, c(columnnames, setdiff(names(datatable), columnnames)))
}

#' Rename columns
#'
#' Rename specific named columns from a data.table (if they exist).
#' @param datatable The data.table to rename the columns from.
#' @param newnames New column names (character vector). Must be of same length as \code{orignames}.
#' @param orignames Original column names (character vector). Default=\code{NULL},
#' which tries to rename all columns.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
setcolnames <- function(datatable, newnames, orignames=NULL) {
  if (is.null(orignames)) {
    orignames=colnames(datatable)
  }
  if (length(orignames)!=length(newnames)) {
    stop("Vector with names are not equal in length!")
  }
  for (i in 1:length(orignames)) {
    try(setnames(x=datatable,old=orignames[i],new=newnames[i]), silent = TRUE)
  }
  invisible(datatable)
}


#' Remove columns
#'
#' Remove specific named columns from a data.table (if they exist).
#' @param data The data.table to remove the columns from.
#' @param remcols Column names (character vector) to remove.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
removecolumns <- function(data,remcols) {
  if (length(remcols)) {
    for (i in 1:length(remcols)) {
      if (remcols[i] %in% colnames(data)) {
        data[,c(remcols[i]):=NULL]
      }
    }
  }
  invisible(data)
}

#' Remove rows by reference
#'
#' Remove specific rows from a data.table (by providing indices). Note that removing
#' a row by reference when the table only consists of one row does not work.
#' @param data The data.table to remove the rows from.
#' @param remrows Row indices (for example as returned by which=TRUE).
#' @return Returns the modified data.table. It is modified by reference if not all
#' rows are removed, but returns a 'copy' if all rows are removed.
#' @family data.table functions
#' @export
removerows <- function(data, remrows) {
  if (length(remrows)) {
    #from https://stackoverflow.com/a/40672437/2710064
    keep.idxs <- setdiff(data[, .I], remrows);
    if (length(keep.idxs)==0) {
      return(data[0,])
    }
    cols = names(data);
    DT.subset <- data.table(data[[1]][keep.idxs]);
    setnames(DT.subset, cols[1]);
    for (col in cols[2:length(cols)]) {
      DT.subset[, (col) := data[[col]][keep.idxs]];
      data[, (col) := NULL];  # delete
    }
  }
  return(DT.subset);
}

#' Remove columns containing NA
#'
#' Remove columns from a data.table where all data in that column is NA.
#' @param data The data.table to remove the columns from.
#' @return The data.table with the columns removed.
#' @family data.table functions
#' @export
removeallNAcolumns <- function(data) {
  data=data[,which(unlist(lapply(data, function(x)!all(is.na(x))))),with=FALSE]
  return(data)
}

#' Remove rows containing only NA
#'
#' Remove columns from a data.table where all data in a row is NA. Different from na.omit in that the complete row needs to be NA.
#' @param data The data.table to remove the rows from.
#' @return The data.table with the rows removed.
#' @family data.table functions
#' @export
removeallNArows <- function(data) {
  nacount=rowSums(is.na(data))
  keeprows=nacount!=ncol(data)
  data=data[keeprows,]
  return(data)
}

#' Remove NA values
#'
#' Update (by reference) all NA's in a data.table with zero or \code{FALSE} or
#' an empty string (based on the column class).
#' @param datatable The data.table to update.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
removeNAfromtable <- function(datatable) {
  for (j in seq_len(ncol(datatable))) {
    if (class(datatable[[j]])=="character") {
      set(datatable,which(is.na(datatable[[j]])),j,"")
    } else if (class(datatable[[j]])=="logical") {
      set(datatable,which(is.na(datatable[[j]])),j,FALSE)
    } else {
      set(datatable,which(is.na(datatable[[j]])),j,0)
    }
  }
  invisible(datatable)
}

#' Update values
#'
#' Update (by reference) specific values in a data.table.
#' @param datatable The data.table to update.
#' @param from A vector of the items to replace.
#' @param to A vector of replacement values.
#' @param columns A vector of column names, or vector of integer column indices, where replacement
#' needs to happen. Default=\code{NULL}, which replaces the values in all columns.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
updatetablevalues <- function(datatable,from,to,columns=NULL) {
  if (!is.null(columns)) {
    if (all(class(columns)=="character")) {
      columns=which(colnames(datatable) %in% columns)
    }
    columns=columns[columns<=length(colnames(datatable))]
  } else {
    columns=seq_len(ncol(datatable))
  }

  for (j in columns) {
    set(datatable,NULL,j,plyr::mapvalues(datatable[[j]],from,to,warn_missing = FALSE))
  }
  invisible(datatable)
}

#' Merge two data tables with overwrite
#'
#' Merge two data tables in which the second data.table overwrites the values in the first one.
#' Missing values (\code{NA}) in the second data.table will not remove values from the first data.table.
#' Uses the same variables and functionality as the default data.table \code{\link[data.table]{merge}}
#' function, but does post-processing on the merged table to overwrite values.
mergeoverwrite <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,...) {
  #modified from https://stackoverflow.com/questions/16042380/merge-data-frames-and-overwrite-values
  if (!is.null(by)) {
    by.x = by.y = by
  }
  commonNames <- names(x)[which(colnames(x) %in% colnames(y))]
  commonNames <- commonNames[!commonNames %in% c(by.x,by.y)]
  dfmerge<- merge(x,y,suffixes=c(".x", ".y"), by.x=by.x, by.y=by.y, ...)
  for (i in commonNames) {
    left <- paste(i, ".x", sep="")
    right <- paste(i, ".y", sep="")
    dfmerge[!is.na(get(right)),(left):=get(right)]
    dfmerge[,(right):=NULL]
    colnames(dfmerge)[colnames(dfmerge) == left] <- i
  }
  return(dfmerge)
}

#' Merge two data tables with preference
#'
#' Merge two data tables in which the second data.table appends values in the first one, meaning
#' that \code{NA} values in the first data.table are updated with the values of the second data.table.
#' Missing values (\code{NA}) in the second data.table will not remove values from the first data.table.
#' Uses the same variables and functionality as the default data.table \code{\link[data.table]{merge}}
#' function, but does post-processing on the merged table to overwrite values.
mergeappend <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,...) {
  #modified from https://stackoverflow.com/questions/16042380/merge-data-frames-and-overwrite-values
  if (!is.null(by)) {
    by.x = by.y = by
  }
  commonNames <- names(x)[which(colnames(x) %in% colnames(y))]
  commonNames <- commonNames[!commonNames %in% c(by.x,by.y)]
  dfmerge<- merge(x,y,suffixes=c(".x", ".y"), by.x=by.x, by.y=by.y, ...)
  for (i in commonNames) {
    left <- paste(i, ".x", sep="")
    right <- paste(i, ".y", sep="")
    dfmerge[is.na(get(left)),(left):=get(right)]
    dfmerge[,(right):=NULL]
    colnames(dfmerge)[colnames(dfmerge) == left] <- i
  }
  return(dfmerge)
}

#' Prefix all column names
#'
#' Add a prefix text to all column names of a data.table
#' @param datatable The data.table to update columns for
#' @param prefix String to add as prefix to the column names
#' @param omitcolumns Columns that do not need a prefix (default=NULL, which prefixes every column)
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
prefixcolnames <- function(thedata,prefix,omitcolumns=NULL) {
  if (!is.null(thedata)) {
    if (any(class(thedata)=="data.table")) {
      origcolnames=colnames(thedata)
      if (!is.null(omitcolumns)) {
        origcolnames=origcolnames[! origcolnames %in% omitcolumns]
      }
      newcolnames=paste0(prefix,origcolnames)
      try(setnames(thedata,origcolnames,newcolnames), silent = TRUE)     #rename
    }
  }
  invisible(thedata)
}

#' Add ISO information
#'
#' Add ISO date information to a data.table
#' @param datatable The data.table to add ISO weeknumber, year, and month to.
#' @param datecolumn String identifying the column name in which a date is present. The column itself should be
#' of the POSIXct class. Default=\code{"DATE"}.
#' @param timezone String determining the timezone (from \code{\link[base]{strptime}}). Default=\code{""},
#' which uses the local time zone.
#' @return Returns the modified data.table invisibly (modified by reference)
#' with the added columns ISOWEEKNR, ISOYEAR and ISOMONTH.
#' @family data.table functions
#' @family date and time functions
#' @export
addisoinfo <- function(datatable,datecolumn="DATE", timezone="") {
  thedates=datatable[,get(datecolumn)]
  datatable[,ISOWEEKNR:=as.integer(strftime(thedates,format="%V",tz=timezone))]
  datatable[,ISOYEAR:=as.integer(strftime(thedates,format="%Y",tz=timezone))]
  datatable[,ISOMONTH:=as.integer(strftime(thedates,format="%m",tz=timezone))]
  invisible(datatable)
}

#' Remove ISO information
#'
#' Remove ISO date information from a data.table, as was added through the \code{\link{addisoinfo}} function.
#' @param datatable The data.table to remove ISO weeknumber, year, and month from
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @family date and time functions
#' @export
removeisoinfo <- function(datatable) {
  invisible(removecolumns(datatable,c("ISOWEEKNR","ISOYEAR","ISOMONTH")))
}

#' Get all column classes
#' @param thedata The data.table to get column classes for
#' @return Data.table with one row containing the column classes of each column.
#' @family data.table functions
#' @export
getcolumnclasses <- function(thedata) {
  return(thedata[, lapply(.SD, class)])
}

#' Run function on columns
#'
#' Run a specific function on the provided columns, to update them.
#' @param thedata The data.table to modify.
#' @param thefun The function to run on the columns.
#' @param thecolnames Column names to run the function on. Default=\code{NULL},
#' which runs the function on all columns.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
runcolfun <- function(thedata,FUN,thecolnames=NULL) {
  if (is.null(thecolnames)) {                                               #check column names
    thecolnames=colnames(thedata)
  } else {
    thecolnames = thecolnames[thecolnames %in% colnames(thedata)]           #only keep column names that exist in the data
  }
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      thedata[,(colname):=FUN(thedata[,get(colname)])]                      #run function
    }
  }
  invisible(thedata)
}

#' Set column class
#'
#' Generic function to set columns of a data.table to a specific class.
#' @param thedata The data.table to modify.
#' @param theclassto Single string or function representing the class to transform columns to.
#' @param theclassfrom Single string or function representing the class the column originally needs to be.
#' Default=\code{NULL}, which transforms all column types to the provided class.
#' @param thecolnames Vector string of column names to set the class for. Default=\code{NULL},
#' which sets the class for all columns.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
setcolclass <- function(thedata,theclassto,theclassfrom=NULL,thecolnames=NULL) {
  if (length(theclassto)>1 | length(theclassfrom)>1)                        #can't transform multiple
    return()

  #fix settings for theclassto
  if (class(theclassto)=="character") {
    stringclass=theclassto
    setclassfun=eval(parse(text=theclassto))
  } else {
    stringclass=deparse(substitute(theclassto))                             #get the provided class as string
    setclassfun=theclassto
  }
  if (substr(stringclass,0,3)!="as.") {                                     #does it contain "as."?
    setclassfun=eval(parse(text=paste0("as.",stringclass)))                 #prepend "as." for the function transformation
  }

  #fix settings for theclassfrom
  checkclass=NULL
  if (!is.null(theclassfrom)) {
    if (class(theclassfrom)=="character") {
      checkclass=theclassfrom
    } else {
      checkclass=deparse(substitute(theclass))                              #get the provided class as string
    }
  }

  #fix column names
  if (is.null(thecolnames)) {                                               #check column names
    thecolnames=colnames(thedata)
  } else {
    thecolnames = thecolnames[thecolnames %in% colnames(thedata)]           #only keep column names that exist in the data
  }
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      if (!is.null(checkclass)) {
        if (class(thedata[,get(colname)])==checkclass) {                    #if class is as the string
          thedata[,(colname):=setclassfun(thedata[,get(colname)])]          #fix it by function
        }
      } else {
        thedata[,(colname):=setclassfun(thedata[,get(colname)])]            #fix it by function
      }
    }
  }
  invisible(thedata)
}

#' Map column classes
#'
#' Ensure columns of a data.table have the right class and if not, transform those columns.
#' @param thedata The data.table to modify.
#' @param thecolumns Vector string of column names of the data.table that
#' need to have a specific class.
#' @param theclasses Vector string of column classes that they need to be.
#' Need to be of same length as \code{thecolumns}. If the strings \code{"date"}
#' or \code{"datetime"} are provided but the columns are not yet in POSIXct,
#' the functions \code{\link{makedatecolumns}} and \code{\link{makedatetimecolumns}}
#' are called respectively.
#' @param ... Additional parameters of the \code{\link{makedatecolumns}}
#' and \code{\link{makedatetimecolumns}} functions, if needed.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
mapcolumnclasses <- function(thedata,thecolumns,theclasses,...) {
  if (length(thecolumns)!=length(theclasses))
    return()

  wantedclasses=data.table(t(theclasses))
  setcolnames(wantedclasses,thecolumns)                                         #create a datatable with column names and wanted classes
  curentclasses=getcolumnclasses(thedata)                                       #create a datatable with existing classes
  combinedcolclasses=rbind(wantedclasses,curentclasses,fill=TRUE)               #combine it so row 2/3 consists of existing, row 1 of wanted
  #row 2 NA means that the provided column is not in the data.table
  #row 1 NA means that the data.table column is not requested to set for a specific class
  #remove all columns that have a NA in it because we can't do anything with it
  combinedcolclasses=combinedcolclasses[,which(unlist(lapply(combinedcolclasses, function(x)!any(is.na(x))))),with=FALSE]
  differentclasses=colnames(combinedcolclasses)[which(combinedcolclasses[1,]!=combinedcolclasses[2,])]    #which columns are different
  if (length(differentclasses)) {                                               #only need to continue if there actually is a change
    combinedcolclasses=combinedcolclasses[,..differentclasses]                  #these columns need to change from class in row 2/3 to class in row 1
    for (colname in colnames(combinedcolclasses)) {
      fromclass=combinedcolclasses[1,..colname]
      mapclass=combinedcolclasses[1,..colname]
      if (mapclass=="date") {
        if (fromclass!="POSIX" && fromclass!="POSIXct") {
          makedatecolumns(thedata,colname,...)
        }
      } else if (mapclass=="datetime") {
        if (fromclass!="POSIX" && fromclass!="POSIXct") {
          makedatetimecolumns(thedata,colname,...)
        }
      } else {
        setcolclass(thedata,as.character(mapclass),thecolnames = colname)
      }
    }
  }
  invisible(thedata)
}

#' Unlist columns
#'
#' Unlist all columns that are of the list type.
#' @param thedata The data.table to modify.
#' @param thecolnames Column names to unlist. Default=\code{NULL},
#' which unlists all columns that it detects to be of class list.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
unlistcolumns <- function(thedata,thecolnames=NULL) {
  if (is.null(thecolnames)) {
    thecolnames=colnames(thedata)
  } else {
    thecolnames = thecolnames[thecolnames %in% colnames(thedata)]           #only keep column names that exist in the data
  }
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      if (class(thedata[,get(colname)])=="list") {                          #if class is list
        coldata=unlist(thedata[,get(colname)])
        if (length(coldata)==nrow(thedata))                                 #if we don't do this we otherwise get a R session termination!
          thedata[,(colname):=coldata]                                      #fix it
      }
    }
  }
  invisible(thedata)
}

#' Make columns numeric
#'
#' Make specific columns from a data.table numeric, if they are of character class.
#' @param thedata The data.table to modify.
#' @param thecolnames Column names to modify to numeric. If commas are present,
#' they are replaced by dots (to represent decimals). Default=\code{NULL},
#' which transforms all columns that it detects to be of class character.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
makenumericcolumns <- function(thedata,thecolnames=NULL) {
  if (is.null(thecolnames)) {
    thecolnames=colnames(thedata)
  } else {
    thecolnames = thecolnames[thecolnames %in% colnames(thedata)]           #only keep column names that exist in the data
  }
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      if (class(thedata[,get(colname)])=="character") {                     #if class is character
        thedata[,(colname):=as.numeric(str_replace_all(thedata[,get(colname)],",","."))]  #fix it
      }
    }
  }
  invisible(thedata)
}

#' Make columns GPS coordinates
#'
#' Make specific columns from a data.table numeric GPS coordinates, if they are of character class.
#' @param thedata The data.table to modify.
#' @param thecolnames Column names to modify to numeric GPS coordinates. If commas are present,
#' they are replaced by dots (to represent decimals). Default=\code{NULL},
#' which transforms all columns that it detects to be of class character.
#' @param degreestodecimal Set to \code{TRUE} to assume the existing coordinates are
#' in degrees, and need to be converted to decimals. Default=\code{FALSE}, which assumes the
#' existing coordinates to be decimals already.
#' @param divisionfactor Provide a value to divide the coordinates by. In certain situations,
#' the provided coordinates have been multiplied by this factor to be stored more efficiently.
#' Default=\code{1}, which assumes the coordinates have been stored without multiplication.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data fixes
#' @export
makecoordinatecolumns <- function(thedata,thecolnames=NULL,degreestodecimal=FALSE,divisionfactor=1) {
  if (is.null(thecolnames)) {
    thecolnames=colnames(thedata)
  } else {
    thecolnames = thecolnames[thecolnames %in% colnames(thedata)]           #only keep column names that exist in the data
  }
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      if (class(thedata[,get(colname)])=="character") {                     #if class is character
        newdata=str_replace_all(thedata[,get(colname)],",",".")
        invertcoord=str_detect(newdata,"[SWsw]")
        newdata=as.numeric(str_replace_all(newdata,"[NESWnesw]",""))
        thedata[,(colname):=newdata]  #fix it
      }
      if (class(thedata[,get(colname)])=="numeric") {
        newdata=thedata[,get(colname)]/divisionfactor
        if (degreestodecimal) {
          newdata=convertcoords_degrees2decimal(newdata)
        }
        thedata[,(colname):=newdata]  #fix it
      }
    }
  }
  invisible(thedata)
}

#' Make columns integer
#'
#' Make specific columns from a data.table integer, if they are of character class.
#' @param thedata The data.table to modify.
#' @param thecolnames Column names to modify to integer. Default=\code{NULL},
#' which transforms all columns that it detects to be of class character.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @export
makeintegercolumns <- function(thedata,thecolnames=NULL) {
  invisible(setcolclass(thedata,"integer","character",thecolnames))
}

#' Make columns date
#'
#' Make specific columns from a data.table into date format, either POSIXct with timezone Europe/Berlin, or a string format, when
#' the current column class is of \code{character}.
#' @param thedata The data.table to modify.
#' @param thecolnames Column names to modify to date.
#' @param stringformat A string format (following \code{\link[base]{strptime}}) to convert the date to.
#' Default=\code{NULL}, which sets the column to POSIXct.
#' @param timezone String determining the timezone (from \code{as.POSIXct}). Default=\code{""},
#' which uses the local time zone.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @family date and time functions
#' @export
makedatecolumns <- function(thedata, thecolnames, stringformat=NULL, timezone="") {
  thecolnames = thecolnames[thecolnames %in% colnames(thedata)]             #only keep column names that exist in the data
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      datumneedsfix=TRUE
      if (class(thedata[,get(colname)])=="character") {                     #if class is character
        if (!is.null(stringformat)) {
          if (stringformat=="%Y-%m-%d") {                                   #specific situation that doesn't need fixing
            locs=as.data.table(str_locate_all(thedata[1,get(colname)],"-"))
            if (nrow(locs)!=0) {
              if (locs[1,start]==5 & locs[2,start]==8) {
                datumneedsfix=FALSE
              }
            }
          }
        }
      }
      if (datumneedsfix) {
        thedata[,(colname):=makeconsistentdateformat(thedata[,get(colname)],stringformat,timezone)]
      }
    }
  }
  invisible(thedata)
}

#' Make columns datetime
#'
#' Make specific columns from a data.table into datetime format, either POSIXct with timezone Europe/Berlin, or a string format.
#' @param thedata The data.table to modify.
#' @param thecolnames Column names to modify to date.
#' @param stringformat A string format (following \code{\link[base]{strptime}}) to convert the date to.
#' Default=\code{NULL}, which sets the column to POSIXct.
#' @param timezone String determining the timezone (from \code{as.POSIXct}). Default=\code{""},
#' which uses the local time zone.
#' @return Returns the modified data.table invisibly (modified by reference).
#' @family data.table functions
#' @family date and time functions
#' @export
makedatetimecolumns <- function(thedata, thecolnames, stringformat=NULL,timezone="") {
  thecolnames = thecolnames[thecolnames %in% colnames(thedata)]             #only keep column names that exist in the data
  if (length(thecolnames)) {                                                #if column names are there
    for (colname in thecolnames) {                                          #loop over all of them
      if (all(class(thedata[,get(colname)])!="POSIXct") | !is.null(stringformat)) {            #if class is not POSIX or format is provided
        thedata[,(colname):=makeconsistentdatetimeformat(thedata[,get(colname)],stringformat,timezone)]
      }
    }
  }
  invisible(thedata)
}

#' Join data.tables
#'
#' This function executes all possible join options for data.tables. It has specific settings for the base and the additional data.table.
#'
#' @seealso The functions \link{leftjoin}, \link{rightjoin}, and \link{fulljoin}.
#' @param basetable The base data.table.
#' @param additionaltable The additional data.table.
#' @param basetablejoincolumns The column name of the base data.table to join on.
#' @param additionaltablejoincolumns The column name of the additional data.table to join on. Default=\code{NULL},
#' which assumes the same column name as \code{basetablejoincolumns}.
#' @param additionaltableextractcolumns The columns of the additional data.table to add to the base data.table.
#' Default=\code{NULL}, which assumes that all columns need to be joined.
#' @param removejoincols If \code{TRUE}, it will remove the provided \code{basetablejoincolumns} from the resulting table.
#' If \code{FALSE}, it will keep the provided \code{basetablejoincolumns} in the result. If \code{NA} (default), it will check if
#' \code{basetablejoincolumns} and \code{additionaltablejoincolumns} are the same. The column names that are different will result
#' in the respective \code{basetablejoincolumns} to be removed.
#'
#' For example, let's take \code{basetablejoincolumns=c("ID","SECID")} and \code{additionaltablejoincolumns=c("ID","ID2")}.
#' With \code{removejoincols=TRUE}, the column \code{"ID2"} will return in the result, but columns \code{"ID"} and \code{"SECID"} will not.
#' With \code{removejoincols=FALSE}, the columns \code{"ID.x"}, \code{"ID.y"}, \code{"ID2"} and \code{"SECID"} will return.
#' Note that column \code{ID} exists in both the base as well as additional table, which results in the \code{"ID.x"} and \code{"ID.y"} columns.
#' With \code{removejoincols=NA} the column \code{"ID"} and \code{"ID2"} will return in the result, but column \code{"SECID"} will not.
#' @param keepbaserows If \code{TRUE} (default), it keeps al rows of the base data.table and adds \code{NA} data for columns
#' in the additional data.table where there is no match. If \code{FALSE}, rows in the base data.table for which
#' there are no matching rows in the additional data.table are not returned.
#' @param keepadditionalrows If \code{TRUE}, it keeps al rows of the additional data.table and adds \code{NA} data for columns
#' in the base data.table where there is no match. If \code{FALSE} (default), rows in the additional data.table for which
#' there are no matching rows in the base data.table are not returned.
#' @return The joined data.table or, when join column(s) or extracted column(s) don't exist, the original base data.table.
#' @family data.table functions
#' @export
dojoin <- function(basetable, additionaltable, basetablejoincolumns,
                   additionaltablejoincolumns=NULL,
                   additionaltableextractcolumns=NULL,
                   removejoincols=NA,
                   keepbaserows=TRUE,
                   keepadditionalrows=FALSE) {
  if (is.null(additionaltablejoincolumns)) {                #if not provided, we join on the same column name
    additionaltablejoincolumns=basetablejoincolumns
  }
  if (is.null(additionaltableextractcolumns)) {             #if not provided, we join all columns
    additionaltableextractcolumns=names(additionaltable)
  }
  if (any(basetablejoincolumns %in% names(basetable) == FALSE)) {                   #either or both the join columns don't exist
    message("Join failed: not all join columns are in the base table.")
    return(basetable)
  }
  if (any(additionaltablejoincolumns %in% names(additionaltable) == FALSE)) {       #either or both the join columns don't exist
    message("Join failed: not all join columns are in the additional table.")
    return(basetable)
  }
  if (any(additionaltableextractcolumns %in% names(additionaltable) == FALSE)) {    #the provided columns don't exist in additionaltable
    message("Join failed: not all extraction columns are in the additional table.")
    return(basetable)
  }
  if (any(additionaltablejoincolumns %in% additionaltableextractcolumns == FALSE)) {              #the join column is not in the extract column list (needs to be)
    additionaltableextractcolumns=append(additionaltableextractcolumns,additionaltablejoincolumns)
  }
  mergedtable=merge(basetable,additionaltable[,match(additionaltableextractcolumns,names(additionaltable)),with=FALSE],by.x=basetablejoincolumns,by.y=additionaltablejoincolumns,all.x=keepbaserows,all.y=keepadditionalrows)

  #ensure the right columns are removed
  removecols=NULL
  if (is.na(removejoincols)) {
    removecols=basetablejoincolumns[basetablejoincolumns!=additionaltablejoincolumns]
    removejoincols=length(removecols)>0
  } else {
    if (removejoincols==TRUE) {
      removecols=basetablejoincolumns
    }
  }
  if (removejoincols) {
    mergedtable[, (removecols) := NULL]
  }

  return(mergedtable)
}

#' Left join data.tables
#'
#' Execute a left join on two data.tables, meaning that all data of the left data.table (\code{basetable}) will be kept, and only
#' the data from the right data.table (\code{additionaltable}) is linked to where it can be found in the left data.table.
#' @param basetable The base data.table
#' @param additionaltable The additional data.table (if unmatched in \code{basetable}, \code{NA} values are added for this data.table).
#' @param basetablejoincolumns The column name string of the base data.table to join on.
#' @param additionaltablejoincolumns The column name string of the additional data.table to join on.
#' Default=\code{NULL}, which assumes the same column name as \code{basetablejoincolumns}.
#' @param additionaltableextractcolumns The column name strings of the additional data.table to add to the base data.table.
#' Default=\code{NULL}, which assumes that all columns need to be joined.
#' @param removejoincols If \code{TRUE}, it will remove the provided \code{basetablejoincolumns} from the resulting table.
#' If \code{FALSE}, it will keep the provided \code{basetablejoincolumns} in the result. If \code{NA} (default), it will check if
#' \code{basetablejoincolumns} and \code{additionaltablejoincolumns} are the same. The column names that are different will result
#' in the respective \code{basetablejoincolumns} to be removed.
#'
#' For example, let's take \code{basetablejoincolumns=c("ID","SECID")} and \code{additionaltablejoincolumns=c("ID","ID2")}.
#' With \code{removejoincols=TRUE}, the column \code{"ID2"} will return in the result, but columns \code{"ID"} and \code{"SECID"} will not.
#' With \code{removejoincols=FALSE}, the columns \code{"ID.x"}, \code{"ID.y"}, \code{"ID2"} and \code{"SECID"} will return.
#' Note that column \code{ID} exists in both the base as well as additional table, which results in the \code{"ID.x"} and \code{"ID.y"} columns.
#' With \code{removejoincols=NA} the column \code{"ID"} and \code{"ID2"} will return in the result, but column \code{"SECID"} will not.

#' @return The joined data.table or, when join column(s) or extracted column(s) don't exist, the original base data.table.
#' @family data.table functions
#' @export
leftjoin <- function(basetable, additionaltable, basetablejoincolumns,
                     additionaltablejoincolumns=NULL,
                     additionaltableextractcolumns=NULL,
                     removejoincols=NA) {
  return(dojoin(basetable,additionaltable,basetablejoincolumns,additionaltablejoincolumns,additionaltableextractcolumns,removejoincols,keepbaserows=TRUE,keepadditionalrows=FALSE))

  setkeyv(basetable,basetablejoincolumns)
  setkeyv(additionaltable,additionaltablejoincolumns)
  return(additionaltable[basetable])
}

#' Right join data.tables
#'
#' Execute a right join on two data.tables, meaning that all data of the right data.table (\code{additionaltable}) will be kept, and only
#' the data from the left data.table (\code{basetable}) is linked to where it can be found in the right data.table.
#' @param basetable The base data.table (if unmatched in \code{additionaltable}, \code{NA} values are added for this data.table).
#' @param additionaltable The additional data.table.
#' @param basetablejoincolumns The column name string of the base data.table to join on.
#' @param additionaltablejoincolumns The column name string of the additional data.table to join on.
#' Default=\code{NULL}, which assumes the same column name as \code{basetablejoincolumns}.
#' @param additionaltableextractcolumns The column name strings of the additional data.table to add to the base data.table.
#' Default=\code{NULL}, which assumes that all columns need to be joined.
#' @param removejoincols If \code{TRUE}, it will remove the provided \code{basetablejoincolumns} from the resulting table.
#' If \code{FALSE}, it will keep the provided \code{basetablejoincolumns} in the result. If \code{NA} (default), it will check if
#' \code{basetablejoincolumns} and \code{additionaltablejoincolumns} are the same. The column names that are different will result
#' in the respective \code{basetablejoincolumns} to be removed.
#'
#' For example, let's take \code{basetablejoincolumns=c("ID","SECID")} and \code{additionaltablejoincolumns=c("ID","ID2")}.
#' With \code{removejoincols=TRUE}, the column \code{"ID2"} will return in the result, but columns \code{"ID"} and \code{"SECID"} will not.
#' With \code{removejoincols=FALSE}, the columns \code{"ID.x"}, \code{"ID.y"}, \code{"ID2"} and \code{"SECID"} will return.
#' Note that column \code{ID} exists in both the base as well as additional table, which results in the \code{"ID.x"} and \code{"ID.y"} columns.
#' With \code{removejoincols=NA} the column \code{"ID"} and \code{"ID2"} will return in the result, but column \code{"SECID"} will not.

#' @return The joined data.table or, when join column(s) or extracted column(s) don't exist, the original base data.table.
#' @family data.table functions
#' @export
rightjoin <- function(basetable, additionaltable, basetablejoincolumns,
                      additionaltablejoincolumns=NULL,
                      additionaltableextractcolumns=NULL,
                      removejoincols=NA) {
  return(dojoin(basetable,additionaltable,basetablejoincolumns,additionaltablejoincolumns,additionaltableextractcolumns,removejoincols,keepbaserows=FALSE,keepadditionalrows=TRUE))

  setkeyv(basetable,basetablejoincolumns)
  setkeyv(additionaltable,additionaltablejoincolumns)
  return(basetable[additionaltable])
}

#' Full join data.tables
#'
#' Execute a full join on two data.tables, meaning that all data of both data.tables will be kept.
#' @param basetable The base data.table (if unmatched in \code{additionaltable}, \code{NA} values are added for this data.table).
#' @param additionaltable The additional data.table (if unmatched in \code{basetable}, \code{NA} values are added for this data.table).
#' @param basetablejoincolumns The column name string of the base data.table to join on.
#' @param additionaltablejoincolumns The column name string of the additional data.table to join on.
#' Default=\code{NULL}, which assumes the same column name as \code{basetablejoincolumns}.
#' @param additionaltableextractcolumns The column name strings of the additional data.table to add to the base data.table.
#' Default=\code{NULL}, which assumes that all columns need to be joined.
#' @param removejoincols If \code{TRUE}, it will remove the provided \code{basetablejoincolumns} from the resulting table.
#' If \code{FALSE}, it will keep the provided \code{basetablejoincolumns} in the result. If \code{NA} (default), it will check if
#' \code{basetablejoincolumns} and \code{additionaltablejoincolumns} are the same. The column names that are different will result
#' in the respective \code{basetablejoincolumns} to be removed.
#'
#' For example, let's take \code{basetablejoincolumns=c("ID","SECID")} and \code{additionaltablejoincolumns=c("ID","ID2")}.
#' With \code{removejoincols=TRUE}, the column \code{"ID2"} will return in the result, but columns \code{"ID"} and \code{"SECID"} will not.
#' With \code{removejoincols=FALSE}, the columns \code{"ID.x"}, \code{"ID.y"}, \code{"ID2"} and \code{"SECID"} will return.
#' Note that column \code{ID} exists in both the base as well as additional table, which results in the \code{"ID.x"} and \code{"ID.y"} columns.
#' With \code{removejoincols=NA} the column \code{"ID"} and \code{"ID2"} will return in the result, but column \code{"SECID"} will not.

#' @return The joined data.table or, when join column(s) or extracted column(s) don't exist, the original base data.table.
#' @family data.table functions
#' @export
fulljoin <- function(basetable, additionaltable, basetablejoincolumns,
                     additionaltablejoincolumns=NULL,
                     additionaltableextractcolumns=NULL,
                     removejoincols=NA) {
  if (nrow(basetable)==0) {
    return(additionaltable)
  }
  if (nrow(additionaltable)==0) {
    return(basetable)
  }
  return(dojoin(basetable,additionaltable,basetablejoincolumns,additionaltablejoincolumns,additionaltableextractcolumns,removejoincols,keepbaserows=TRUE,keepadditionalrows=TRUE))

  return(merge(basetable,additionaltabley,by.x=basetablejoincolumns,by.y=additionaltablejoincolumns,all=TRUE))
}

#' Inner join data.tables
#'
#' Execute an inner join on two data.tables, meaning that only data present in both data.tables will be kept.
#' @param basetable The base data.table.
#' @param additionaltable The additional data.table.
#' @param basetablejoincolumns The column name string of the base data.table to join on.
#' @param additionaltablejoincolumns The column name string of the additional data.table to join on.
#' Default=\code{NULL}, which assumes the same column name as \code{basetablejoincolumns}.
#' @param additionaltableextractcolumns The column name strings of the additional data.table to add to the base data.table.
#' Default=\code{NULL}, which assumes that all columns need to be joined.
#' @param removejoincols If \code{TRUE}, it will remove the provided \code{basetablejoincolumns} from the resulting table.
#' If \code{FALSE}, it will keep the provided \code{basetablejoincolumns} in the result. If \code{NA} (default), it will check if
#' \code{basetablejoincolumns} and \code{additionaltablejoincolumns} are the same. The column names that are different will result
#' in the respective \code{basetablejoincolumns} to be removed.
#'
#' For example, let's take \code{basetablejoincolumns=c("ID","SECID")} and \code{additionaltablejoincolumns=c("ID","ID2")}.
#' With \code{removejoincols=TRUE}, the column \code{"ID2"} will return in the result, but columns \code{"ID"} and \code{"SECID"} will not.
#' With \code{removejoincols=FALSE}, the columns \code{"ID.x"}, \code{"ID.y"}, \code{"ID2"} and \code{"SECID"} will return.
#' Note that column \code{ID} exists in both the base as well as additional table, which results in the \code{"ID.x"} and \code{"ID.y"} columns.
#' With \code{removejoincols=NA} the column \code{"ID"} and \code{"ID2"} will return in the result, but column \code{"SECID"} will not.

#' @return The joined data.table or, when join column(s) or extracted column(s) don't exist, the original base data.table.
#' @family data.table functions
#' @export
innerjoin <- function(basetable, additionaltable, basetablejoincolumns,
                      additionaltablejoincolumns=NULL,
                      additionaltableextractcolumns=NULL,
                      removejoincols=NA) {
  return(dojoin(basetable,additionaltable,basetablejoincolumns,additionaltablejoincolumns,additionaltableextractcolumns,removejoincols,keepbaserows=FALSE,keepadditionalrows=FALSE))
}

#' Extract unique categories
#'
#' Extract the unique categories/elements found in the dataset for a specific column.
#' @param data The data.table in which all approaches are listed.
#' @param datacolumn String containing the column from the data.table to extract from.
#' @return Vector with all unique categories/elements.
#' @examples
#' extractcategories(data,"TREINNR") #returns all unique train numbers
#' @family data.table functions
#' @export
extractcategories <- function(data,datacolumn) {
  output=unique(data[,get(datacolumn)])
  return(output)
}

#' Extract unique categories
#'
#' Extract the unique categories/elements found in the dataset for a specific column.
#' @param data The data.table in which all approaches are listed.
#' @param datacolumn String containing the column from the data.table to extract from.
#' @return Vector with all unique categories/elements.
#' @examples
#' selectuniques(data,"TREINNR") #returns all unique train numbers
#' @family data.table functions
#' @export
selectuniques <- function(data,datacolumn) {
  return(extractcategories(data,datacolumn))
}


#SELECT selectcols FROM data WHERE datacolumn IN datavalues
#' Extract from a dataset
#'
#' Extract specified columns from the dataset where 1 specific column contains specific values.
#' This follows the SQL principle "SELECT selectcols FROM data WHERE datacolumn IN datavalues".
#' @param data The data.table in which all approaches are listed.
#' @param datacolumn String containing the column which must match the values.
#' @param datavalues The data values to search for.
#' @param selectcols Vector containing column name strings to export (default=\code{NULL}, which returns all columns).
#' @param uniques Set to \code{TRUE} if only unique rows are to be returned (default=\code{FALSE}, which returns all matching rows).
#' @return Data.table containing all rows for which the column matches the values.
#' @examples
#' selectwherein(data, "SIGNAALREMCURVE",c("ATTENTIE","ALARM"),c("DATUM","TREINNR"),TRUE) #returns all unique dates and train numbers where signal is attention or alarm
#' @family data.table functions
#' @export
selectwherein <- function(data,datacolumn,datavalues,selectcols=NULL,uniques=FALSE) {
  if(is.null(selectcols)) {
    output = data[get(datacolumn) %in% datavalues,]         #equivalent: SELECT * FROM data WHERE datacolumn IN datavalues
  } else {
    output = data[get(datacolumn) %in% datavalues,match(selectcols,names(data)),with=FALSE]         #equivalent: SELECT selectcols FROM data WHERE datacolumn IN datavalues
  }
  if (uniques) {
    output = unique(output)                                 #get all unique rows (data table format)
    if (!is.null(selectcols)&length(selectcols)==1) {       #get all unique rows (single column convert to vector)
      output = output[[1]]
    }
  }
  return(output)
}

#SELECT selectcols FROM data WHERE whereclause
#' Extract from a dataset
#'
#' Extract specified columns from the dataset where 1 specific column contains specific values.
#' This follows the SQL principle "SELECT selectcols FROM data WHERE datacolumn IN datavalues".
#' @param data The data.table in which all approaches are listed
#' @param whereclause String containing the condition certain columns must match
#' @param selectcols Vector containing column name strings to export (default=\code{NULL}, which returns all columns).
#' @param uniques Set to \code{TRUE} if only unique rows are to be returned (default=\code{FALSE}, which returns all matching rows).
#' @return Data.table containing all rows for which the column matches the values. Returns a vector if uniques=TRUE and selectcols contains only one column.
#' @examples
#' selectwhere(data, "is_ATTENTIE==1&is_ALARM==1",c("DATUM","TREINNR"),TRUE) #returns all unique dates and train numbers having both attention and alarm signal
#' @family data.table functions
#' @export
selectwhere <- function(data,whereclause,selectcols=NULL,uniques=FALSE) {
  if(is.null(selectcols)) {
    output = data[eval(parse(text=whereclause)),]         #equivalent: SELECT * FROM data WHERE whereclause
  } else {
    output = data[eval(parse(text=whereclause)),match(selectcols,names(data)),with=FALSE]         #equivalent: SELECT selectcols FROM data WHERE whereclause
  }
  if (uniques) {
    output = unique(output)                                 #get all unique rows (data table format)
    if (!is.null(selectcols)&length(selectcols)==1) {       #get all unique rows (single column convert to vector)
      output = output[[1]]
    }
  }
  return(output)
}

