% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuicedDatatable.R
\name{removecolumns}
\alias{removecolumns}
\title{Remove columns}
\usage{
removecolumns(data, remcols)
}
\arguments{
\item{data}{The data.table to remove the columns from.}

\item{remcols}{Column names (character vector) to remove.}
}
\value{
Returns the modified data.table invisibly (modified by reference).
}
\description{
Remove specific named columns from a data.table (if they exist).
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
\code{\link{makedatecolumns}()},
\code{\link{makedatetimecolumns}()},
\code{\link{makeintegercolumns}()},
\code{\link{makenumericcolumns}()},
\code{\link{mapcolumnclasses}()},
\code{\link{prefixcolnames}()},
\code{\link{removeNAfromtable}()},
\code{\link{removeallNAcolumns}()},
\code{\link{removeallNArows}()},
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
}
\concept{data.table functions}
