\name{setWhere}
\alias{setWhere}
\title{set Where}
\description{set Where for database queries}
\usage{
  setWhere(con, x, vintage, panel)
}
\arguments{
    \item{con}{A database connection.}
    \item{x}{identifier for series to extract.}
    \item{vintage}{Vintage to be specified.}
    \item{panel}{Panel to be specified).}
}
\value{A character string to use as where in db query.}
\keyword{internal}

