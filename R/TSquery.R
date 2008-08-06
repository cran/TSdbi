
TSquery <- function(select, dateField, table, where=NULL, frequency="monthly",
             con=options()$connection) {

    if (is.null(con))  stop("argument 'con' cannot be NULL")
    if (frequency=="monthly")      dates <- paste("EXTRACT(YEAR from ",dateField,"), EXTRACT(MONTH from ",dateField,")")
    else if (frequency=="annual")  dates <- paste("EXTRACT(YEAR from ",dateField,")")
    else stop ("frequency not supported.")

    q <-paste("SELECT ",dates,", ", select , " FROM ",table)
    if(!is.null(where)) q <- paste(q, " WHERE ", where)
    q <- paste(q, " GROUP BY ",dates, " ORDER BY ", dates, " ;")
    #res <- fetch(dbSendQuery(con, q), n=-1)  #1000)
    reso <- res <- dbGetQuery(con, q)
    if(any(dim(res) == 0)) stop("empty query result.")
    res <- as.matrix(res)
    
    # this is not very R like, but may be a good model for a C procedure 
    #  for mysql (see manual on UDF p 1208.
    if (frequency=="monthly") {
       # first column in res has year,second month, third data
       res <- res[!is.na(res[,1]),, drop=FALSE] # kludge for odd case with NA date (zero value)
       res <- res[!is.na(res[,2]),, drop=FALSE] # kludge for odd case with NA date (zero value)
       y <- res[1,1]
       m <- res[1,2]
       sampleT  <- 1+ (12 *res[nrow(res),1]+res[nrow(res),2]) - (12 *res[1, 1] +res[1,2])
       r <- matrix(numeric(3*sampleT), sampleT,3) # does zero fill
       j <- 1
       for (i in seq(sampleT)) {
          if ((res[j,1] == y) & (res[j,2] == m)) {
	     r[i,] <- res[j,]
	     j <- j+1
	     }
	   m <- m + 1
	   if (m==13) {
	      y <- y + 1
	      m <- 1
	      }
	  }
       }
    else if (frequency=="annual") {
      # first column in res has year,second data
       res <- res[!is.na(res[,1]),, drop=FALSE] # kludge for odd case with NA date (zero value)
       y <- res[1,1]
       sampleT  <- 1+ res[nrow(res),1] - y
       r <- matrix(numeric(2*sampleT), sampleT,2) # does zero fill
       j <- 1
       for (i in seq(sampleT)) {
          if (res[j,1] == y) {
	     r[i,] <- res[j,]
	     j <- j+1
	     }
	   y <- y + 1
	  }
       }
    if (frequency=="monthly")     ts(r[,-(1:2)], start=r[1,1:2], frequency=12)
    else if (frequency=="annual") ts(r[,  -1  ], start=r[1,1],   frequency=1)
    }

