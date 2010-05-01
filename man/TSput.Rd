\name{TSput}
\alias{TSput}
\alias{TSput,ANY,missing,missing-method}
\alias{TSput,ANY,DBIConnection,missing-method}
\alias{TSput,ANY,character,ANY-method}
\alias{TSput,ANY,character,missing-method}
\alias{TSdelete}
\alias{TSdelete,character,missing-method}
\alias{TSdelete,character,ANY-method}
\alias{TSdelete,character,missing,ANY,ANY-method}
\alias{TSdelete,character,ANY,ANY,ANY-method}
\alias{TSreplace}
\alias{TSreplace,default-method}

\alias{logicalId-class}

\title{Write Data to a Data Connection}
\description{Write data to a server.}
\usage{
    TSput(x, serIDs=seriesNames(x), con=getOption("TSconnection"), ...)

    TSdelete(serIDs, con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)

    TSreplace(x, serIDs=seriesNames(x), con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)

    \S4method{TSput}{ANY,missing,missing}(
	x, serIDs=seriesNames(x), con=getOption("TSconnection"), ...)
    \S4method{TSput}{ANY,DBIConnection,missing}(
	x, serIDs=seriesNames(x), con=getOption("TSconnection"), ...)
    \S4method{TSput}{ANY,character,ANY}(
	x, serIDs=seriesNames(x), con=getOption("TSconnection"), ...)

    \S4method{TSdelete}{character,missing}(
	serIDs, con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
    \S4method{TSdelete}{character,ANY}(
	serIDs, con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
    \S4method{TSdelete}{character,missing,ANY,ANY}(
	serIDs, con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
    \S4method{TSdelete}{character,ANY,ANY,ANY}(
	serIDs, con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)

    \S4method{TSreplace}{default}(
	x, serIDs=seriesNames(x), con=getOption("TSconnection"), 
	vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
}
\arguments{
    \item{con}{A database connection.}
    \item{x}{time series data.}
    \item{serIDs}{identifiers for series on the database.}
    \item{vintage}{character string indicating vintage of the series on the 
        database (if supported by the database).}
    \item{panel}{character string indicating panel of the series on the 
        database (if supported by the database).}
    \item{...}{Arguments passed to other methods.}
}

\value{An object of class logicalId.}

\details{
    Class logicalId is a logical indicating if the operation succeeded, and
    also contains meta data indicating how to retrieve the data. (Except in 
    the case of TSdelete the data cannot be retrieved.)

    These functions write data to a database connection. 
    \code{TSreplace} removes any existing object first. \code{TSput} will
    fail if a series with the same identifier already exists.

    \code{TSput} and \code{TSreplace} provide ways to query a regularly 
    used single database. The connection can be set in \code{options}
    using \code{options(TSconnection=con)} and then only the series identifiers 
    need to be specified in calls to \code{TSput} and \code{TSreplace}.

\code{TSdescription} and \code{TSdoc}can also be set.
(Passed in \dots). See \code{\link{TSputSQL}} for more details.

If an appropriate table cannot be determined from the series it will be
necessary to pass the \code{Table} argument (in \dots). 
The DBI/SQL interface uses the following tables:
\preformatted{
A     for annual data
Q     for quarterly data
M     for monthly data
S     for semiannual data
W     for weekly data
D     for daily data
B     for business data
U     for minutely data
I     for irregular data with a date
T     for irregular data with a date and time
Meta  for meta data
}
    }

\seealso{ 
    \code{\link{TSdbi-package}},
    \code{\link{TSdates}},
    \code{\link{TSget}},
    \code{\link{TSputSQL}},
    \code{\link[DBI]{dbConnect}}
}
\keyword{ts}



