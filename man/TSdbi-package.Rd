\name{TSdbi-package}
\alias{TSdbi-package}
\alias{TSdbi.Intro}
\docType{package}

\title{Time Series Data Base Interface}

\description{TSdbi provides an common interface to 
time series databases. Several of these are databases available over the Internet.
Others are packages that use the DBI package interface to
SQL databases. For these a table structure is specified. 
TSdbi can also be used as an interface to Fame databases through TSfame.
}

\details{
\tabular{ll}{
Package: \tab TSdbi\cr
Depends: \tab R (>= 2.5.0), methods, tframe (>= 2008.5-1)\cr
Imports: \tab methods, DBI\cr
Suggests:\tab zoo, tseries\cr
License: \tab GPL Version 2.\cr
URL: \tab http://tsdbi.r-forge.r-project.org\cr
}

The main functions are:
\preformatted{
TSconnect       Connect to a database.
TSget       	Extract a series from a database.
TSput       	Write a series to the database.
TSdates      	Check the availability of a series.
TSdescription	Extract the long description of a series.
TSdoc	 	Extract the documentation for a series.
}

Use of this package requires one of the interface packages (e. g. 
\pkg{TSSQLite}, \pkg{TSMySQL}, \pkg{TSPostgreSQL}) 
An overview of how to use the package is  available in the vignettes of the
package \pkg{TSdata}.
Using \pkg{TSdbi} is very similar for the different database interfaces, 
but building vignettes requires working code so the vignettes  are included 
in the various interface packages.  For the same reason, most examples and 
demos must be included in the interface packages. Consult the documentation for
the methods in a particular interface package for most examples.

Options can be set to simplify access to a commonly used database 
(see \code{\link{TSput}}).
}

\author{
Paul Gilbert <pgilbert.ttv9z@ncf.ca>
Maintainer: Paul Gilbert <pgilbert.ttv9z@ncf.ca>  
}
\keyword{ package }
\keyword{ ts }
\seealso{
   \code{\link{TSconnect}}, 
   \code{\link{TSget}}, 
   \code{\link{TSput}}, 
   \code{\link{TSdates}}, 
   \code{\link[DBI]{dbConnect}}, 
   \code{\link[TSSQLite]{TSdbiMethods}}, 
   \code{\link[TSMySQL]{TSdbiMethods}}, 
   \code{\link[TSPostgreSQL]{TSdbiMethods}}, 
}
