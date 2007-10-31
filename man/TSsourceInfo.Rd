\name{TSsourceInfo}
\alias{TSseriesIDs}
\alias{TScon}
\alias{TSextractionDate}

\title{Get source information from a data object}
\description{Get source information from an object}
\usage{
    TSseriesIDs(x)
    TScon(x)
    TSextractionDate(x)
}
\arguments{
    \item{x}{An object which contains source series information (as returned
        by TSget).}
}
\value{
Strings indicating the  information.
}
\seealso{
\code{\link{TSget}}, 
\code{\link{TSconnect}}, 
\code{\link{TSdates}}
}
\keyword{ts}


