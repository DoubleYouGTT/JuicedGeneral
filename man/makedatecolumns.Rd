% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuicedDatatable.R
\name{makedatecolumns}
\alias{makedatecolumns}
\title{Make columns date}
\usage{
makedatecolumns(thedata, thecolnames, stringformat = NULL, timezone = "")
}
\arguments{
\item{thedata}{The data.table to modify.}

\item{thecolnames}{Column names to modify to date.}

\item{stringformat}{A string format (following \code{\link[base]{strptime}}) to convert the date to.
Default=\code{NULL}, which sets the column to POSIXct.}

\item{timezone}{String determining the timezone (from \code{as.POSIXct}). Default=\code{""},
which uses the local time zone.}
}
\value{
Returns the modified data.table invisibly (modified by reference).
}
\description{
Make specific columns from a data.table into date format, either POSIXct with timezone Europe/Berlin, or a string format, when
the current column class is of \code{character}.
}
\seealso{
Other data.table functions: 
\code{\link{addcolumn}()},
\code{\link{addisoinfo}()},
\code{\link{copytoclipboard}()},
\code{\link{dojoin}()},
\code{\link{extractcategories}()},
\code{\link{fulljoin}()},
\code{\link{getcolumnclasses}()},
\code{\link{innerjoin}()},
\code{\link{leftjoin}()},
\code{\link{loadlargetable}()},
\code{\link{loadsastable}()},
\code{\link{loadspecifictable}()},
\code{\link{loadtable}()},
\code{\link{makedatetimecolumns}()},
\code{\link{makeintegercolumns}()},
\code{\link{makenumericcolumns}()},
\code{\link{mapcolumnclasses}()},
\code{\link{prefixcolnames}()},
\code{\link{removeNAfromtable}()},
\code{\link{removeallNAcolumns}()},
\code{\link{removeallNArows}()},
\code{\link{removecolumns}()},
\code{\link{removeisoinfo}()},
\code{\link{removerows}()},
\code{\link{renamecolumns}()},
\code{\link{rightjoin}()},
\code{\link{runcolfun}()},
\code{\link{selectuniques}()},
\code{\link{selectwherein}()},
\code{\link{selectwhere}()},
\code{\link{setcolclass}()},
\code{\link{setcolnames}()},
\code{\link{setcoltofront}()},
\code{\link{setcolumns}()},
\code{\link{unlistcolumns}()},
\code{\link{updatetablevalues}()}

Other date and time functions: 
\code{\link{addisoinfo}()},
\code{\link{findperiodcontained}()},
\code{\link{findperiodoverlapping}()},
\code{\link{makeconsistentdateformat}()},
\code{\link{makeconsistentdatetimeformat}()},
\code{\link{makedatetimecolumns}()},
\code{\link{removeisoinfo}()},
\code{\link{seqdays}()}
}
\concept{data.table functions}
\concept{date and time functions}
