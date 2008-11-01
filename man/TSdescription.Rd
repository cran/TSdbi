\name{TSdescription}
\alias{TSdescription}
\alias{TSdescription,character,missing-method}
\alias{TSdescription,character,ANY-method}
\alias{TSdescription,ANY,missing-method}
\alias{TSdescription,missing,ANY-method}
\alias{TSdescription,missing,missing-method}
\alias{TSdescription<-}

\alias{TSdoc}
\alias{TSdoc,character,missing-method}
\alias{TSdoc,character,ANY-method}
\alias{TSdoc,ANY,missing-method}
\alias{TSdoc,missing,ANY-method}
\alias{TSdoc,missing,missing-method}
\alias{TSdoc<-}

\alias{TSlabel}
\alias{TSlabel,character,missing-method}
\alias{TSlabel,character,ANY-method}
\alias{TSlabel,ANY,missing-method}
\alias{TSlabel,missing,ANY-method}
\alias{TSlabel,missing,missing-method}
\alias{TSlabel<-}

\alias{TSmeta-class}
\alias{TSmeta}
\alias{TSmeta,character,missing-method}
\alias{TSmeta,character,ANY-method}
\alias{TSmeta,ANY,missing-method}
\alias{TSmeta<-}

\alias{TSexists}
\alias{TSexists,default-method}

\alias{TSrefperiod}
\alias{TSrefperiod,default-method}
\alias{TSrefperiod<-}


\title{Specific Methods for Documenting Data}
\description{See the generic function description.}
\usage{
    TSexists(serIDs, con=getOption("TSconnection"), ...)
    \S4method{TSexists}{default}(serIDs, con=getOption("TSconnection"), ...)

    TSmeta(x, con, ...) 
    \S4method{TSmeta}{character,missing}(x, con=getOption("TSconnection"), ...)
    \S4method{TSmeta}{character,ANY}(x, con=getOption("TSconnection"), ...)
    \S4method{TSmeta}{ANY,missing}(x, con, ...)

    TSmeta(x) <- value

    TSdescription(x, con, ...) 
    \S4method{TSdescription}{character,missing}(x, con=getOption("TSconnection"), ...)
    \S4method{TSdescription}{character,ANY}(x, con=getOption("TSconnection"), ...)
    \S4method{TSdescription}{ANY,missing}(x, con, ...)
    \S4method{TSdescription}{missing,ANY}(x, con, serIDs, ...)
    \S4method{TSdescription}{missing,missing}(x, serIDs, ...)
    TSdescription(x) <- value

    TSdoc(x, con, ...) 
    \S4method{TSdoc}{character,missing}(x, con=getOption("TSconnection"), ...)
    \S4method{TSdoc}{character,ANY}(x, con=getOption("TSconnection"), ...)
    \S4method{TSdoc}{ANY,missing}(x, con, ...)
    \S4method{TSdoc}{missing,ANY}(x, con, serIDs, ...)
    \S4method{TSdoc}{missing,missing}(x, serIDs, ...)
    TSdoc(x) <- value

    TSlabel(x, con, ...) 
    \S4method{TSlabel}{character,missing}(x, con=getOption("TSconnection"), ...)
    \S4method{TSlabel}{character,ANY}(x, con=getOption("TSconnection"), ...)
    \S4method{TSlabel}{ANY,missing}(x, con, ...)
    \S4method{TSlabel}{missing,ANY}(x, con, serIDs, ...)
    \S4method{TSlabel}{missing,missing}(x, serIDs, ...)
    TSlabel(x) <- value

    TSrefperiod(x)
    \S4method{TSrefperiod}{default}(x)
    TSrefperiod(x) <- value
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series on the database.}
    \item{x}{a time series data object or an identifier for a series on the database.}
    \item{value}{a character string (or vector of character strings).}
    \item{...}{arguments passed to other methods.}
    }
\value{Depends. See details.}
\details{
These functions return various information about the data series. Methods
with \code{con} and \code{serIDs} (sometimes identifier are passed as
argument \code{x}) get data from the database. Others extract
information from the object.

\code{TSexists} returns TRUE or FALSE, depending on whether the series exist
at the connection. (All series specified must exist for TRUE.)

    Assignments assign an attribute to the object \code{x} with value. If 
    \code{x} is a multivariate time series (matrix) then \code{value} should
    be a vector of length equal the number of series. The reference period for
    a time series indicates a special refence point (e.g. "Wednesday" for weekly
    data collected on Wednesday).
    
    The extraction methods extract the attribute.
    }
\seealso{
    \code{\link{TSget}}
    \code{\link{TSput}}
    \code{\link{TSdates}}
    }

\keyword{ts}


