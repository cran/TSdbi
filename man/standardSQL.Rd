\name{standardSQL}
\alias{TSgetSQL}
\alias{TSputSQL}
\alias{TSdatesSQL}
\alias{TSdescriptionSQL}
\alias{TSdocSQL}
\alias{TSlabelSQL}
\alias{TSdeleteSQL}


\title{Standard SQL Database Queries}
\description{Standard SQL database queries}
\usage{
    TSgetSQL(serIDs, con, TSrepresentation=getOption("TSrepresentation"),
       tf = NULL, start = tfstart(tf), end = tfend(tf), 
       names=NULL, TSdescription=FALSE, TSdoc=FALSE,  TSlabel=FALSE,
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"))

    TSdatesSQL(serIDs, con, 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"))
    TSdeleteSQL(serIDs, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel")) 
    TSdescriptionSQL(x=NULL, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"),
       lang=getOption("TSlang"))
    TSdocSQL(x=NULL, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"),
       lang=getOption("TSlang"))
    TSlabelSQL(x=NULL, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"),
       lang=getOption("TSlang"))

    TSputSQL(x, serIDs=seriesNames(x), con,  Table=NULL,
       TSdescription.=TSdescription(x), TSdoc.=TSdoc(x),  TSlabel.=TSlabel(x), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"))
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series to extract.}
    \item{TSrepresentation}{time series representation to use for the result.}
    \item{x}{time series data, or serIDs.}
    \item{names}{Optional character vector to use for series names.}
    \item{tf}{Optional tframe to use for truncating data. (See \code{tfwindow}.)}
    \item{start}{Optional start date to use for truncating data. (See \code{tfwindow}.)}
    \item{end}{Optional end date to use for truncating data. (See \code{tfwindow}.)}
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
}
\value{A time series matrix.}
\details{
These functions provide standard SQL queries that can be used by other packages
such as \pkg{TSMySQL} and \pkg{TSSQLite} that provide connections. These
functions would usually not be called directly by the user.

The functions are not exported from the namespace, so they should be refered to
with the package name (e.g. \code{TSdbi:::TSgetSQL})
} 

\seealso{ 
\code{\link{TSdbi-package}},
\code{\link[DBI]{dbConnect}},
\code{\link{TSget}},
\code{\link{TSput}},
\code{\link{TSdelete}},
\code{\link{TSmeta}},
\code{\link{TSdates}}
\code{\link[tframe]{tfwindow}}
}
\keyword{internal}

