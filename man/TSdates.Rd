\name{TSdates}
\alias{TSdates}
\alias{TSdates,character,missing-method}
\alias{TSdates,character,ANY-method}

\alias{start.TSdates}
\alias{tfstart.TSdates}
\alias{end.TSdates}
\alias{tfend.TSdates}

\title{Check Data Availability}
\description{Check the dates for which date is available.}
\usage{
    TSdates(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)

    \S4method{TSdates}{character,missing}(serIDs, con=getOption("TSconnection"),
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
    \S4method{TSdates}{character,ANY}(serIDs, con=getOption("TSconnection"),
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)

    \method{start}{TSdates}(x, ...)
    \method{tfstart}{TSdates}(x)
    \method{end}{TSdates}(x, ...)
    \method{tfend}{TSdates}(x)
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series on the database.}
    \item{x}{an object returned by \code{TSdates}.}
    \item{vintage}{character string indicating vintage of the series on the 
        database (if supported by the database).}
    \item{panel}{character string indicating panel of the series on the 
        database (if supported by the database).}
    \item{...}{arguments passed to other methods.}
}
\value{depends.}
\details{
\code{TSdates} returns information about the start and 
end of each series in serIDs. \code{con} is a database connection as returned by
\code{dbConnect}.  \code{TSdates} also provides simple way to query a regularly 
used database. The connection can be set in \code{options}
using \code{options(TSconnection=con)} and then only the series identifiers 
need to be specified in calls to \code{TSdates}.

\code{start, tfstart, end,} and \code{tfend} extract start and end dates from
the object returned by TSdates.
}
\seealso{
\code{\link{TSdbi-package}},
\code{\link{TSdescription}},
\code{\link[DBI]{dbConnect}},
\code{\link{TSget}},
\code{\link{TSput}},
\code{\link[tframe]{tfstart}},
\code{\link[tframe]{tfend}}
}
\keyword{ts}

