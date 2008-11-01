\name{TSconnect}
\alias{TSconnect}
\alias{TSconnect,character,character-method}
\alias{TSdb-class}
\alias{TSid-class}
\alias{show,logicalId-method}
\alias{show,TSdb-method}
\alias{print,TSdb-method}

\title{Connect to a Time Series Database}
\description{Return a connection to a time series database}
\usage{
    TSconnect(drv, dbname, ...)
    \S4method{TSconnect}{character,character}(drv, dbname, ...)
    \S4method{show}{logicalId}(object)
    \S4method{show}{TSdb}(object)
    \S4method{print}{TSdb}(x, ...)
}
\arguments{
    \item{drv}{A database connection driver or character string.}
    \item{dbname}{The name of the database to which the connection should be
         established}
    \item{x}{A database connection as returned by TSconnect.}
    \item{object}{an object to display.}
    \item{...}{Additional arguments passed to other print methods.}
}
\value{A database connection.}
\details{
This function establishes a connection using a driver from one of the driver
packages (e.g. \pkg{TSMySQL} or \pkg{TSSQLite}). If \code{drv} is a character
string (e.g. \code{"MySQL"}) then the method attempts to get a driver using the
character string.

\code{"TSconnect"} uses \code{dbConnect} but checks the database
has expected tables and also establishes
information about additional features that may be available (vintiges and/or
panels).

Options can be set to simplify access to a commonly used database 
(see \code{\link{TSput}}).
}

\seealso{ 
\code{\link{TSdbi-package}},
\code{\link[DBI]{dbConnect}},
\code{\link{TSget}},
\code{\link{TSput}},
\code{\link{TSdates}}
}
\keyword{ts}

