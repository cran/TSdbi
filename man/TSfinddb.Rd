\name{TSfinddb}
\alias{TSfinddb}

\title{Find a Time Series Database Connection}
\description{Find a connection to a specified time series database.}
\usage{
    TSfinddb(dbname=NULL, driverOrder=c("MySQL", "SQLite", "padi")) 
}
\arguments{
    \item{dbname}{Character string indicating the name of a database.}
    \item{driverOrder}{A vector of character string indicating TSdbi drivers
        in the order they should be tried.}
}
\value{A connection}
\details{\code{TSfinddb} tries to establish a connection to the indicated 
database using the drivers in the order specified. This attempt also requires
the corresponding TSdbi driver package (e.g., "TSMySQL", "TSSQLite", or
"TSpadi"). If the package cannot be loaded then the driver is skipped.
The first valid connection is returned. If no valid connection is found 
then NULL is returned.
}

\seealso{ 
\code{\link{TSdbi-package}},
\code{\link[DBI]{dbConnect}},
\code{\link{TSput}},
\code{\link{TSget}},
\code{\link{TSdates}}
}
\keyword{ts}

