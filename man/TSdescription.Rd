\name{TSdescription}
\alias{TSdescription}
\alias{TSdescription,character,missing-method}
\alias{TSdescription,character,ANY-method}
\alias{TSdescription,ANY,missing-method}
\alias{TSdescription<-}

\alias{TSdoc}
\alias{TSdoc,character,missing-method}
\alias{TSdoc,character,ANY-method}
\alias{TSdoc,ANY,missing-method}
\alias{TSdoc<-}

\alias{TSmeta-class}
\alias{TSmeta}
\alias{TSmeta,character,missing-method}
\alias{TSmeta,character,ANY-method}
\alias{TSmeta,ANY,missing-method}
\alias{TSmeta<-}
\alias{show,TSmeta-method}
\alias{print,TSmeta-method}

\alias{TSexists}
\alias{TSexists,default-method}

\alias{TSrefPeriod}
\alias{TSrefPeriod,default-method}
\alias{TSrefPeriod<-}


\title{Specific Methods for Documenting Data}
\description{See the generic function description.}
\usage{
    \S4method{show}{TSmeta}(object)
    \S4method{print}{TSmeta}(x, ...)

    TSexists(serIDs, con=options()$TSconnection, ...)
    \S4method{TSexists}{default}(serIDs, con=options()$TSconnection, ...)

    TSmeta(x, con, ...) 
    \S4method{TSmeta}{character,missing}(x, con=options()$TSconnection, ...)
    \S4method{TSmeta}{character,ANY}(x, con=options()$TSconnection, ...)
    \S4method{TSmeta}{ANY,missing}(x, con, ...)

    TSmeta(x) <- value

    TSdescription(x, con, ...) 
    \S4method{TSdescription}{character,missing}(x, con=options()$TSconnection, ...)
    \S4method{TSdescription}{character,ANY}(x, con=options()$TSconnection, ...)
    \S4method{TSdescription}{ANY,missing}(x, con, ...)
    TSdescription(x) <- value

    TSdoc(x, con, ...) 
    \S4method{TSdoc}{character,missing}(x, con=options()$TSconnection, ...)
    \S4method{TSdoc}{character,ANY}(x, con=options()$TSconnection, ...)
    \S4method{TSdoc}{ANY,missing}(x, con, ...)
    TSdoc(x) <- value

    TSrefPeriod(x)
    \S4method{TSrefPeriod}{default}(x)
    TSrefPeriod(x) <- value
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifiers for series on the database.}
    \item{x}{a time series data object or an identifier for a series on the database.}
    \item{object}{an object to display.}
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


