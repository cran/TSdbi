\name{TSdbi-package}
\alias{TSdbi-package}
\alias{TSdbi.Intro}
\docType{package}

\title{Time Series Data Base Interface}

\description{TSdbi provides an common interface to 
time series databases. The package can use the DBI package and its interface to
SQL databases, in which case a table structure is specified by TSdbi. 
It can also be used as an interface to Fame databases (through the padi
protoocol for now, but directly should be possible with some work).
}

\details{
\tabular{ll}{
Package: \tab TSdbi\cr
Depends: \tab R (>= 2.5.0), GPArotation, setRNG (>= 2004.4-1), tframe (>=
2007.5-2),\cr
         \tab dse1 (>= 2007.5-2), dse2 (>= 2007.5-2)\cr
Suggests: \tab CDNmoney\cr
License: \tab GPL Version 2.\cr
URL: \tab http://www.bank-banque-canada.ca/pgilbert\cr
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
\pkg{TSSQLite}, \pkg{TSMySQL}, \pkg{TSpadi}) 
An overview of how to use the package is (will be) available in vignettes.
Using \pkg{TSdbi} is very similar for the different database interfaces, 
but building vignettes requires working code so the vignettes  are included 
in the various interface packages.  For the same reason, most examples and 
demos must be included in the interface packages. Consult the documentation for
the methods in a particular interface package for most examples.

Options can be set to simplify access to a commonly used database 
(see \code{\link{TSput}}).
}

\author{
Paul Gilbert <pgilbert@bank-banque-canada.ca> 
Maintainer: Paul Gilbert <pgilbert@bank-banque-canada.ca>  
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
   \code{\link[TSpadi]{TSdbiMethods}}, 
}
