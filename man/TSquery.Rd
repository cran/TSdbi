\name{TSquery}
\alias{TSquery}

\title{Construct Time Series Data From a Database}
\description{Construct a time series from a database}
\usage{
    TSquery(select, dateField, table, where=NULL, frequency="monthly",
             con=options()$connection)
}
\arguments{
    \item{select}{character string used to construct query.}
    \item{dateField}{character string used to construct query.}
    \item{table}{character string used to construct query.}
    \item{where}{character string used to construct query.}
    \item{frequency}{character string used to specify frequency of the result
    (only "monthly" or "annual" are currently supported.}
    \item{con}{A database connection.}
}
\value{A time series or time series matrix.}
\details{
This functions is unlike other functins in this package (and may eventually be
moved to a separate package).  
It construct a time series from a database (using a connection) and a query
generated with the function arguments. An example query might be something like 
\code{
 z<- TSquery(select="SUM(amount )", dateField="issue_date", table="term", 
    where="term > 10",  frequency="annual", con=con)
}
}

\seealso{ 
\code{\link{TSget}},
}
\keyword{ts}

