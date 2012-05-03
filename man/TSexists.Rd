\name{TSexists}

\alias{TSexists}
\alias{TSexists,default-method}

\title{Check if Series Exist on a Database}
\description{Return a logical value indicating if series exist on a 
database, including vintages.}
\usage{
    TSexists(serIDs, con=getOption("TSconnection"),
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
    \S4method{TSexists}{default}(
	serIDs, con=getOption("TSconnection"),
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series on the database.}
    \item{vintage}{character string indicating vintage of the series on the 
        database (if supported by the database).}
    \item{panel}{character string indicating panel of the series on the 
        database (if supported by the database).}
    \item{...}{arguments passed to other methods.}
    }
\value{Depends. See details.}
\details{
\code{TSexists} returns TRUE or FALSE, depending on whether the series exist
at the connection. (All series specified must exist for TRUE.)
If \code{vintage} is a vector then a corresponding vector result is returned.

}
\seealso{
    \code{\link{TSdescription}},
    \code{\link{TSget}},
    \code{\link{TSput}},
    \code{\link{TSdates}}
    }

\keyword{ts}


