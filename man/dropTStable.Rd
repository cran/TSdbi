\name{dropTStable}
\alias{dropTStable}

\title{Drop (Remove) a Database Table}
\description{Drop (remove) a database table if it exists.}
\usage{
    dropTStable(con, Table, yesIknowWhatIamDoing=FALSE)
}
\arguments{
    \item{con}{A database connection.}
    \item{Table}{A character string indicating a table to drop.}
    \item{yesIknowWhatIamDoing}{logical to help prevent accidents.}
}
\value{TRUE}
\details{
WARNING: use this function only if you know what you are doing. It may destroy
the whole database.
The function \code{dropTStable} removes the indicated table if it exists on 
the \code{con}. This function will not generally be needed by users. It is 
used for initializing and testing the database set up, and in many cases 
does simply \code{if(dbExistsTable(con, Table)) dbRemoveTable(con, Table)}. 
It needs to be generic in order to work around the problem that different 
db engines treat capitalized table names differently. For example, MySQL uses
table name Meta while Posgresql converts to meta. A default \code{con} is not used 
on purpose to help avoid accidental use. The arument \code{yesIknowWhatIamDoing}
defaults to \code{FALSE} and must be set to \code{TRUE} or the function will return a n error. Beware that dropping tables will likely destory the integrity of the of
the database, and would usually only be done when you are intializing a TS database.
Database permission may also be set to prevent users from dropping tables. If that
is the case, then this function will return a failure error.
}
\seealso{
\code{\link[DBI]{dbConnect}},
\code{\link{TSdates}},
\code{\link{TSget}},
\code{\link{TSput}}
}
\keyword{ts}


