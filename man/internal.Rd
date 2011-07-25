\name{setWhere}
\alias{setWhere}
\alias{realVintage}
\alias{realPanel}
\title{set Where}
\description{set Where for database queries}
\usage{
  setWhere(con, serIDs, realVintage, realPanel)
  realVintage(con, vintage, serIDs)
  realPanel(con, panel)
}
\arguments{
    \item{con}{A database connection.}
    \item{serIDs}{identifier for series to extract.}
    \item{vintage}{Vintage to be specified (possibly alias).}
    \item{panel}{Panel to be specified (possibly alias).}
    \item{realVintage}{Vintage to be specified (not alias).}
    \item{realPanel}{Panel to be specified (not alias).}
}
\value{A character string to use as where in db query.}
\keyword{internal}

