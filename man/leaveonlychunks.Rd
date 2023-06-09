% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuicedVector.R
\name{leaveonlychunks}
\alias{leaveonlychunks}
\title{Leave chunks}
\usage{
leaveonlychunks(vals, chunksize, replaceval = NA)
}
\arguments{
\item{vals}{Vector to check for chunks.}

\item{chunksize}{Minimum size of a chunk.}

\item{replaceval}{Replace values of too small chunks in the vector with this value. Default=\code{NA},
which replaces all chunks that are too small with \code{NA}. If \code{NULL}, it will automatically check
the class of the vector, and uses \code{FALSE} for booleans and \code{0} for numeric or integer vectors.}
}
\value{
A vector where the length of chunks having the same value is larger than the provided \code{chunksize}.
Too small chunks have been replaced with the \code{replaceval}.
}
\description{
For a vector, leave only those elements where a certain value occurs for
at least a certain amount of time in a chunk.
}
\seealso{
Other vector functions: 
\code{\link{chunk}()},
\code{\link{dorollapply}()},
\code{\link{extendvector}()},
\code{\link{findconstantchunks}()},
\code{\link{findconstant}()},
\code{\link{localmaxima}()},
\code{\link{localminimamaxima}()},
\code{\link{localminima}()},
\code{\link{makelength}()},
\code{\link{replaceNAelements}()},
\code{\link{replaceelements}()},
\code{\link{runavgweighed}()},
\code{\link{runavg}()},
\code{\link{runsd}()},
\code{\link{shift}()}
}
\concept{vector functions}
