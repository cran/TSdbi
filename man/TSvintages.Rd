\name{TSvintages}
\alias{TSvintages}
\alias{TSvintages,missing-method}
\alias{TSvintages,ANY-method}

\title{Indicate all Vintages at a Connection}
\description{Indicate all vintages on the database(s) associated with a 
TSconnection.}
\usage{
    TSvintages(con=getOption("TSconnection")) 
    \S4method{TSvintages}{missing}(con=getOption("TSconnection"))
    \S4method{TSvintages}{ANY}(con=getOption("TSconnection"))
}
\arguments{
    \item{con}{A TSconnection object}
}
\value{A vector of strings indicating vintage identifiers}
\details{\code{TSvintages} returns the vintage identifiers if available.
Otherwise  NULL is returned. The result, or subsets of it, can be used as the
\code{vintage} argument in calls to \code{TSget}. 
}

\seealso{ 
\code{\link{TSconnect}}
}
\keyword{ts}

