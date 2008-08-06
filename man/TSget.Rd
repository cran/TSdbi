\name{TSget}
\alias{TSget}
\alias{TSget}
\alias{TSget,character,missing-method}
\alias{TSget,character,ANY-method}

\title{Get Time Series Data Using a Database Connection}
\description{Get time series matrix structure from a database}
\usage{
    TSget(serIDs, con=options()$TSconnection,  ...)
    \S4method{TSget}{character,missing}(serIDs, con=options()$TSconnection,  ...)
    \S4method{TSget}{character,ANY}(serIDs, con=options()$TSconnection,  ...)
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series to extract.}
    \item{...}{Arguments passed to TSgetSQL or other methods.}
}
\value{A time series matrix.}
\details{
These functions extracts data from 
a database using a connection. This method is generic.
The argument \code{serIDs} should give identifiers for the series to extract. 

\code{TSget} and other functions also provide a way to query a regularly 
used database by setting the connection in \code{options}
using \code{options(TSconnection=con)}, so then only the series identifiers 
need to be specified in calls to \code{TSget}.

If the database supports vintages or panels then it is also possible to set
defaults for these with, for example, \code{options(TSvintage="current")} and
\code{options(TSpanel="Canada")}. The default specification has to be supported
by the database for this to work.

The user can specify a default time series representation with
\code{options(TSrepresentation="something")}, where "something" might 
typically be "zoo". 
If \code{TSrepresentation} is not set with \code{options()} and not specified 
in the call then the ts representation is used for data from tables 
"A", "Q", "M","S" and \code{zoo} otherwise. See \code{\link{TSput}} for a
list of the various tables. ONLY THIS DEFAULT IS WORKING AT PRESENT.

\code{names}, \code{TSrepresentation}, \code{TSdescription} and 
\code{documentation} can also be specified as arguments.
(Passed in \dots). See \code{\link{TSputSQL}} for more details.
}

\seealso{ 
\code{\link{TSdbi-package}},
\code{\link{TSconnect}},
\code{\link{TSput}},
\code{\link{TSdates}}
}
\keyword{ts}

