\name{TSget}
\alias{TSget}
\alias{TSget}
\alias{TSget,character,missing-method}
\alias{TSget,character,ANY-method}

\title{Get Time Series Data Using a Database Connection}
\description{Get time series matrix structure from a database}
\usage{
    TSget(serIDs, con=getOption("TSconnection"),  ...)
    \S4method{TSget}{character,missing}(serIDs, con=getOption("TSconnection"),  ...)
    \S4method{TSget}{character,ANY}(serIDs, con=getOption("TSconnection"),  ...)
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series to extract.}
    \item{...}{Arguments passed to TSgetSQL or other methods. See details.}
}
\value{A time series matrix.}
\details{
These functions extract data from 
a database using a connection. This method is generic.
The argument \code{serIDs} should give identifiers for the series to extract. 

\code{TSget} and other functions also provide a way to query a regularly 
used database by setting the connection in \code{options}
using \code{options(TSconnection=con)}, so then only the series identifiers 
need to be specified in calls to \code{TSget}.

The user can specify a default time series representation with the argument
TSrepresentation="something" where "something" is "default" by default, but 
might be "zoo" or something else which is used to coerce the representation.
The \code{TSrepresentation} is passed in the \dots argument. 
The conversion is done with the function \code{changeTSrepresentation}.
If the representation is a character string then it is applied using \code{do.call(TSrepresentation, list(mat, dates))}
where \code{mat} is the time series (matrix) to be return and dates are
determined by \code{as.Date(time(x))} where \code{x} is the default representation of the data. If representation is not a character string 
then it should be
a function and is applied using \code{TSrepresentation(mat, dates)}

If \code{TSrepresentation} is not specified, or is specified as "default",
then for SQL packages (\pkg{TSMySQL}, \pkg{TSPostgreSQL }, \pkg{TSSQLite}, etc)
the \code{ts} representation is used for data from tables 
"A", "Q", "M","S" and \code{zoo} otherwise. See \code{\link{TSput}} for a
list of the various tables. For other packages the default is zoo in most cases,
but may vary.

It would be possible to specify \code{TSrepresentation="as.zoo"}, but this may
result in \code{as.zoo} being applied twice, in which case some information 
abut the time representation gets lost, so the best way to get a \code{zoo}
represenation is to specify \code{TSrepresentation="zoo"}.

Users can set a session default with \code{options(TSrepresentation="something")}
so that this is always passed as an argument to \code{TSget}. 

It is also possible to pass start, end, or tframe information to truncate the
returned series. This is part of the \dots argument passed 
to \code{tfwindow}. See \code{\link[tframe]{tfwindow}} for more details. By
default no truncation is applied.

If the database supports vintages or panels then it is also possible to set
defaults for these with, for example, \code{options(TSvintage="current")} and
\code{options(TSpanel="Canada")}. The default specification has to be supported
by the database for this to work.

Also, if the database supports vintages or panels it is possible to give a
vector value for one of \code{vintage} or \code{panel} as long as 
\code{serIDs} is length \code{1}. (That is, only one of \code{serIDs}, 
\code{vintage} or \code{panel} can have more than one element.)
In this case, if \code{names} is not specified, \code{vintage} or \code{panel} 
will be used for the series names in the returned time series matrix.

\code{names}, \code{TSdescription}, 
\code{TSdoc} and\code{TSlabel} can also be specified as arguments.
(Passed in \dots). See \code{\link{TSputSQL}} for more details.
}

\seealso{ 
\code{\link{TSdbi-package}},
\code{\link{TSconnect}},
\code{\link{TSput}},
\code{\link{TSdates}}
\code{\link[tframe]{tfwindow}}
\code{\link[tframePlus]{changeTSrepresentation}}
}
\keyword{ts}

