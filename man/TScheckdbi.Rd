\name{TScheckdbi}
\alias{TScheckdbi}
\alias{TScheckdbi,missing-method}
\alias{TScheckdbi,ANY-method}

\title{Check Connection}
\description{Check if time series database connection is ok.}
\usage{
    TScheckdbi(con)
    \S4method{TScheckdbi}{missing}(con=getOption("TSconnection"))
    \S4method{TScheckdbi}{ANY}(con=getOption("TSconnection"))
}
\arguments{
    \item{con}{A database connection.}
}
\value{TRUE or FALSE.}
\details{
The function \code{TScheckdbi} checks if a connection to a server is a
connection to a valid time series database.
}
\seealso{
\code{\link[DBI]{dbConnect}},
\code{\link{TSdates}},
\code{\link{TSget}},
\code{\link{TSput}}
}
\keyword{ts}


