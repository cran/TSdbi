\name{standardSQL}
\alias{TSget.SQL}
\alias{TSput.SQL}
\alias{TSdates.SQL}
\alias{TSdoc.SQL}
\alias{TSdescription.SQL}
\alias{TSdelete.SQL}


\title{Standard SQL Database Queries}
\description{Standard SQL database queries}
\usage{
    TSget.SQL(serIDs, con, TSrepresentation=options()$TSrepresentation,
       names=NULL, TSdescription=FALSE, TSdoc=FALSE,
       vintage=options()$TSvintage, panel=options()$TSpanel, ...)

    TSdates.SQL(serIDs, con, 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...)
    TSdelete.SQL(serIDs, con=options()$TSconnection, 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...) 
    TSdoc.SQL(x=NULL, con=options()$TSconnection, 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...)
    TSdescription.SQL(x=NULL, con=options()$TSconnection, 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...)

    TSput.SQL(x, serIDs=seriesNames(x), con,  Table=NULL,
       TSdescription.=TSdescription(x), TSdoc.=TSdoc(x), 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...)
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series to extract.}
    \item{TSrepresentation}{time series representation to use for the result.}
    \item{x}{time series data, or serIDs.}
    \item{names}{Optional character vector to use for series names.}
    \item{TSdescription}{TRUE or FALSE indicating whether description 
       should be extracted}
    \item{TSdescription.}{Description to put on the database.}
    \item{TSdoc}{TRUE or FALSE indicating whether documentation 
       should be extracted.}
    \item{TSdoc.}{Documentation to put on the database.}
    \item{Table}{Database table indication (necessary if it cannot be
         determined automatically).}
    \item{vintage}{Vintage to be supplied (if supported by db).}
    \item{panel}{Panel to be supplied (if supported by db).}
    \item{...}{Arguments passed to TSget.SQL or other methods.}
}
\value{A time series matrix.}
\details{
These functions provide standard SQL queries that can be used by other packages
such as \pkg{TSMySQL} and \pkg{TSSQLite} that provide connections. These
functions would usually not be called directly by the user.

The functions are not exported from the namespace, so they should be refered to
with the package name (e.g. \code{TSdbi:::TSget.SQL})
} 

\seealso{ 
\code{\link{TSdbi-package}},
\code{\link[DBI]{dbConnect}},
\code{\link{TSget}},
\code{\link{TSput}},
\code{\link{TSdelete}},
\code{\link{TSmeta}},
\code{\link{TSdates}}
}
\keyword{internal}

