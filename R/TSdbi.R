.onLoad <- function(library, section) {require("methods")}

#setClassUnion("OptionalPOSIXct",   c("POSIXct",   "NULL"))
# bug work around
setClassUnion("OptionalPOSIXct",   c("POSIXct",   "logical"))

setClass("TSmeta", 
  representation(serIDs="character", dbname="character",
  con="character", ExtractionDate="OptionalPOSIXct",
  TSdescription="character", TSdoc="character" )) 

#TSmeta itself is an S4 class, but setting attr is S3ish,
#  but this should always return an S4 TSmeta object
setGeneric("TSmeta", 
   def= function(x, con, ...) standardGeneric("TSmeta"))

# this really should be for con="TSconnection" rather than ANY
setMethod("TSmeta",   signature(x="character", con="ANY"),
   definition= function(x, con=options()$TSconnection, ...){
       new("TSmeta", serIDs=x, dbname=con@dbname, 
               con=class(con), ExtractionDate=NA, #NULL, # bug in 2.7.0 =Sys.time(),
	       TSdoc=TSdoc(x, con=con, ...),
               TSdescription=TSdescription(x, con=con, ...))
	} )

setMethod("TSmeta",   signature(x="character", con="missing"),
   definition= function(x, con=options()$TSconnection, ...) 
       TSmeta(x, con=options()$TSconnection, ...) )

# extract meta data from an object, not from the db
setMethod("TSmeta",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...) {
      m <- attr(x, "TSmeta")
      if(is.null(m)) new("TSmeta",serIDs=seriesNames(x), 
	         dbname="", con="", ExtractionDate=NA, #NULL,
		 TSdescription="", TSdoc="") else m
      })

setGeneric("TSmeta<-", 
   def= function(x, value) standardGeneric("TSmeta<-"),
   useAsDefault= function(x, value){
      if (!is(value,"TSmeta")) stop("trying to set attribute TSmeta incorrectly.") 
      attr(x, "TSmeta") <- value
      x
      })

setClass("TSdbOptions", representation(
    dbname="character", vintage="logical", panel="logical"), "VIRTUAL" )

setMethod("show", "TSdbOptions", function(object) {
    cat("database ", object@dbname) 
    if (object@vintage) cat( " Has vintages." )
    if (object@panel) cat( " Has panels." )
    cat("\n") 
    invisible(object)
    })

setMethod("print", "TSdbOptions", function(x, ...) {
    cat("database ", x@dbname) 
    if (x@vintage) cat( " Has vintages." )
    if (x@panel) cat( " Has panels." )
    cat("\n") 
    invisible(x)
    })

setGeneric("TSconnect", def= function(drv, dbname, ...) standardGeneric("TSconnect"))

setMethod("TSconnect",   signature(drv="character", dbname="character"),
   definition=function(drv, dbname, ...) TSconnect(dbDriver(drv), dbname=dbname))

setGeneric("TSdescription<-", 
   def= function(x, value) standardGeneric("TSdescription<-"),
   useAsDefault=  function (x, value){
    m <- TSmeta(x)
    m@TSdescription <- value
    TSmeta(x) <- m
    x})

setGeneric("TSdescription", 
   def= function(x, con, ...) standardGeneric("TSdescription"))

setMethod("TSdescription",   signature(x="character", con="missing"),
   definition= function(x, con=options()$TSconnection, ...) 
       TSdescription(x, con=options()$TSconnection, ...) )

setMethod("TSdescription",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...) TSmeta(x)@TSdescription)

#  next is for case where there is no method for con  
setMethod("TSdescription",   signature(x="character", con="ANY"),
   definition= function(x, con=options()$TSconnection, ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdescription.")
       else stop("con class ", class(con), " is not supported.")} )

# a little internal utility to construct WHERE
setWhere <- function(con, x, vintage, panel) {
     if(con@panel   && is.null(panel))   stop("default panel is not set.") 
     if(con@vintage) {
       if(is.null(vintage))	vintage <- "current" 
       if("current" == vintage) vintage <- dbGetQuery(con,
   	   "SELECT vintage  FROM vintageAlias WHERE alias='current';" )$vintage
       }
     where <-  paste(" WHERE id = '", x, "'", sep="")
     if(!is.null(vintage)) where <- paste(where, " AND vintage='", vintage, "'", sep="")
     if(!is.null(panel))   where <- paste(where, " AND panel='",   panel,   "'", sep="")
     where
     }

TSdescription.SQL <-  function(x=NULL, con=options()$TSconnection, 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...) {	    
            r <- dbGetQuery(con, paste("SELECT description  FROM Meta ", 
                       setWhere(con, x, vintage, panel), ";", sep=""))$description
	    if(is.null(r)) "" else r
	    }

setGeneric("TSdoc<-", 
   def= function(x, value) standardGeneric("TSdoc<-"),
   useAsDefault=  function (x, value){
    m <- TSmeta(x)
    m@TSdoc <- value
    TSmeta(x) <- m
    x})


setGeneric("TSdoc", 
   def= function(x, con, ...) standardGeneric("TSdoc"))

setMethod("TSdoc",   signature(x="character", con="missing"),
   definition= function(x, con=options()$TSconnection, ...) 
       TSdoc(x, con=options()$TSconnection, ...) )

setMethod("TSdoc",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...) TSmeta(x)@TSdoc)

#  next is for case where there is no method for con  
setMethod("TSdoc",   signature(x="character", con="ANY"),
   definition= function(x, con=options()$TSconnection, ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdoc.")
       else stop("con class ", class(con), " is not supported.")} )

TSdoc.SQL <-  function(x=NULL, con=options()$TSconnection, 
       vintage=options()$TSvintage, panel=options()$TSpanel, ...) {
            if(1 < length(x)) stop("One series only for TSdoc")
            r <- dbGetQuery(con, paste("SELECT documentation  FROM Meta ", 
                       setWhere(con, x, vintage, panel), ";", sep=""))$documentation
	    if(is.null(r)) "" else r
	    }

setMethod("show", "TSmeta", function(object) {
    cat("serIDs: ", object@serIDs, " from dbname: ", object@dbname, "\n") 
    cat("description: ", object@TSdescription, "\n") 
    cat("documentaion: ", object@TSdoc, "\n") 
    invisible(object)
    })

setMethod("print", "TSmeta", function(x, ...) {
    cat("serIDs: ", x@serIDs, " from dbname ", x@dbname, "\n") 
    cat("description: ", x@TSdescription, "\n") 
    cat("documentaion: ", x@TSdoc, "\n") 
    invisible(x)
    })


setGeneric("TSrefPeriod", 
   def= function(x) standardGeneric("TSrefPeriod"),
   useAsDefault= function(x) attr(x, "TSrefPeriod"))

setGeneric("TSrefPeriod<-", 
   def= function(x, value) standardGeneric("TSrefPeriod<-"),
   useAsDefault= function (x, value){attr(x, "TSrefPeriod") <- value ; x })


TSseriesIDs      <- function(x)TSmeta(x)@serIDs
TScon            <- function(x)TSmeta(x)@con
TSextractionDate <- function(x)TSmeta(x)@ExtractionDate

# host = "" is interpreted as localhost by the driver (see ?mysqlNewConnection)
#see  vignette("zoo") for other examples


setGeneric("TSput",    
   def= function(x, serIDs=seriesNames(x), con=options()$TSconnection, ...)
             standardGeneric("TSput")) 

setMethod("TSput",   signature(x="ANY", serIDs="character", con="missing"),
   definition= function(x, serIDs, con=options()$TSconnection, ...) 
       TSput(x, serIDs=serIDs, con=options()$TSconnection, ...))

setMethod("TSput",   signature(x="ANY", serIDs="missing", con="missing"),
   definition= function(x, serIDs, con=options()$TSconnection, ...) 
       TSput(x, serIDs=seriesNames(x), con=options()$TSconnection, ...))

setMethod("TSput",   signature(x="ANY", serIDs="DBIConnection", con="missing"),
   definition= function(x, serIDs, con, ...) 
       TSput(x, serIDs=seriesNames(x), con=serIDs, ...))

#  next is for case where there is no method for con  
setMethod("TSput",   signature(x="ANY", serIDs="character", con="ANY"),
   definition= function(x, serIDs=seriesNames(x), con=options()$TSconnection, ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSput.")
       else stop("con class ", class(con), " is not supported.")} )

   

TSput.SQL <- function(x, serIDs=seriesNames(x), con, Table=NULL,
       TSdescription.=TSdescription(x), TSdoc.=TSdoc(x),  
       vintage=options()$TSvintage, panel=options()$TSpanel, ...) {

  # so far I think this is generic to all SQL, but not extensively tested.
  # should rollback meta when data put fails
  #  (reversing order of M and P would mean data can change and 
  #    then meta fail, which seems worse.)
  if(con@panel   && is.null(panel))   stop("default panel is not set.") 
  if(con@vintage) {
    if(is.null(vintage))     vintage <- "current" 
    if("current" == vintage) vintage <- dbGetQuery(con,
     	"SELECT vintage  FROM vintageAlias WHERE alias='current';" )$vintage
    }
  # M does the write to Meta, P writes values to data table.

  # Column order could be specified after Meta, but this assumes  order
  #([vintage,] [ panel,] id, tbl, refPeriod, description, documentation )
  M <- function(v, p, table, id, rP, desc, doc) {
    if(is.null(rP))   rP   <- "NA"
    if(is.null(desc)) desc <- "NA"
    if(is.null(doc))  doc  <- "NA"
    q <- "INSERT INTO Meta VALUES ('"
    if(!is.null(v))  q <- paste(q,v, "', '", sep="")
    if(!is.null(p))  q <- paste(q,p, "', '", sep="")
    q <- paste(q, id, "', '", table, "', '", 
                 rP, "', '", desc, "', '", doc, "') ;", sep="") 
    dbGetQuery(con, q) 
    # next if is RSQLite BUG work around
    if(is(con, "SQLiteConnection")) return(TRUE)
    0 == dbGetException(con)$errorNum
    }

  P <- function(v, p, table, columns, id, ...) {
    x <- list(...)
    vl <- id
    if(!is.null(p))  {
      vl <- paste(p,vl, sep=",")
      columns <- paste("panel,",columns, sep="")
      }
    if(!is.null(v))  {
      vl <- paste(v,vl, sep=",")
      columns <- paste("vintage,",columns, sep="")
      }
    for (i in 1:length(x)) vl <- paste(vl, x[[i]], sep="', '")
    # SQL wants NULL (not 'NULL') for NA
    vl <-   gsub("'NA'", "NULL", paste(vl,"'", sep=""))
    dbGetQuery(con, paste("DELETE FROM  ",table,
                     setWhere(con,id[1],vintage,panel), ";", sep="")) 
    for (i in seq(length(vl))){
      q <- paste("INSERT INTO ",table, " (", columns,
                     ") VALUES ('", vl[i], ") ", sep="")  
      dbGetQuery(con, q) 
      }
    # next if is RSQLite BUG work around
    if(is(con, "SQLiteConnection")) return(TRUE)
    0 == dbGetException(con)$errorNum
    }

  # as.matrix(x) clobbers seriesNames(x)
  ids <-  serIDs 
  #ids <- gsub(" ", "", serIDs ) # remove spaces in id
  #if(! all( ids == serIDs)) warning("spaces removed from series names.")
  rP <- TSrefPeriod(x)
  N <- periods(x)
  # SHOULD PROBABLY HAVE METHODS FOR TS, ZOO, ITS, ... HERE
  if(is.ts(x)) {
    fr <-frequency(x) 
    y <- floor(time(x))
    p <- 1 + round(frequency(x) * (time(x) %% 1 ))
    x <- as.matrix(x)
    if(1==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "A",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(vintage, panel, "A", "id, year, v", ids[i], y, x[,i])
        }
    else if(2==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "S",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(vintage, panel, "S", "id, year, period, v", ids[i], y, p, x[,i])
        }
    else if(4==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "Q",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(vintage, panel, "Q", "id, year, period, v", ids[i], y, p, x[,i])
        }
    else if(12==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "M",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(vintage, panel, "M", "id, year, period, v", ids[i], y, p, x[,i])
        }
    else stop("ts frequency not supported.")  
    }
  else if (inherits(x, "zoo")) {
    require("zoo")
    #  might do better than this
    if (is.null(Table)) stop("Table must be specified for zoo series.")
    d <- time(x) 
    p <-  as.POSIXlt(d)
    x <- as.matrix(x)
    #  STILL NEED A S Q M  FOR ZOO
    if("W" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "W",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        # period should be week of the year 1-52/3
	ok <- P(vintage, panel, "W", "id, date, period, v", ids[i], d, p$mday, x[,i])
        }
    else if("B" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "B",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        # period should be business day of the year 1- ~260
	#  but need to map holidays
        ok <- P(vintage, panel, "B", "id, date, period, v", ids[i], d, p$yday, x[,i])
        }
    else if("D" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "D",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        # period should be day of the year 1- 365/6
        ok <- P(vintage, panel, "D", "id, date, period, v", ids[i], d, p$yday, x[,i])
        }
    else if("U" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "U",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        tz <- attr(p, "tzone")
	ok <- P(vintage, panel, "U", "id, date, period, v", ids[i], d, tz, p$mday,x[,i])
        }
    else if("T" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "T",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(vintage, panel, "T", "id, date, v", ids[i], d, x[,i])
        }
    else if("I" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "I",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(vintage, panel, "I", "id, date, v", ids[i], d, x[,i])
        }
    else stop("Table specification not supported.")  
     }
  else stop("Time series type not recognized.")  
  ok & okM
  }


setGeneric("TSreplace", 
   def= function(x, serIDs=seriesNames(x), con=options()$TSconnection, ...)
           standardGeneric("TSreplace"),
   useAsDefault= function(x,serIDs=seriesNames(x), con=options()$TSconnection,  ...) {
      if(missing(con) & (!missing(serIDs)) && is(serIDs, "DBIConnection")) 
	     return(TSreplace(x, serIDs=seriesNames(x), con=serIDs, ...))
      TSdelete(serIDs=serIDs, con=con, ...)
      TSput(x, serIDs=serIDs, con=con, ...) }
   )

setGeneric("TSdelete", 
   def= function(serIDs, con=options()$TSconnection, ...)
           standardGeneric("TSdelete") )

setMethod("TSdelete",   signature(serIDs="character", con="missing"),
   definition= function(serIDs, con=options()$TSconnection, ...) 
       TSdelete(serIDs, con=options()$TSconnection, ...) )

#  next is for case where there is no method for con  
setMethod("TSdelete",   signature(serIDs="character", con="ANY"),
   definition= function(serIDs, con=options()$TSconnection, ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdelete.")
       else stop("con class ", class(con), " is not supported.")} )

   
TSdelete.SQL <- function(serIDs, con=options()$TSconnection,  
   vintage=options()$TSvintage, panel=options()$TSpanel, ...) {
       for (i in seq(length(serIDs))) {
           where <-  setWhere(con, serIDs[i], vintage, panel)
           q <- dbGetQuery(con, paste("SELECT tbl  FROM Meta ",where, ";"))
            if(0 != length(q)) {
	      dbGetQuery(con, paste("DELETE FROM ", q$tbl, where, ";")) 
              dbGetQuery(con, paste("DELETE FROM Meta ",   where, ";")) 
              }
	    }
	}


setGeneric("TSget", 
   def= function(serIDs, con=options()$TSconnection, ...)
           standardGeneric("TSget") )

setMethod("TSget",   signature(serIDs="character", con="missing"),
   definition= function(serIDs, con=options()$TSconnection, ...) 
       TSget(serIDs, con=options()$TSconnection, ...) )

#  next is for case where there is no method for con  
setMethod("TSget",   signature(serIDs="character", con="ANY"),
   definition= function(serIDs, con=options()$TSconnection, ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSget.")
       else stop("con class ", class(con), " is not supported.")} )

# next is not really a method, but it is called by methods for various SQL dbs.
TSget.SQL <- function(serIDs, con, TSrepresentation=options()$TSrepresentation,
       names=NULL, TSdescription=FALSE, TSdoc=FALSE,
       vintage=options()$TSvintage, panel=options()$TSpanel, ...) {
  # so far I think this is generic to all SQL.
  if(is.null(TSrepresentation)) TSrepresentation <- "default"
    
  Q <- function(q) {# local function
      res <- dbGetQuery(con, q)
      if(any(dim(res) == 0)) stop("empty query result.")
      as.matrix(res)
      }

  mat <- desc <- doc <- rp <- NULL
  for (i in seq(length(serIDs))) {
    where <-  setWhere(con, serIDs[i], vintage, panel)
    q <- dbGetQuery(con, paste("SELECT tbl, refPeriod  FROM Meta ",where, ";"))
    tbl <- q$tbl
    rp <- c(rp, q$refPeriod)

    if(is.null(tbl)) stop("Meta lookup for series ",
             serIDs[i], " table result NULL. Probably series does not exist.")

    if (TSrepresentation == "default") {
       TSrepresentation <-  if (tbl %in% c("A", "Q", "M","S")) "ts" else "zoo"
       }
    if (TSrepresentation == "zoo" && ! require("zoo")) 
       stop("zoo package is required.")

    if (tbl=="A") 
      {res <- Q(paste("SELECT year, v FROM A ",where, " order by year;"))
       r <- do.call(TSrepresentation,
                   list(res[,2], start=c(res[1,1], 1), frequency=1)) 
      }
    else if (tbl=="Q")  
      {res <- Q(paste("SELECT year, period, v FROM Q ",where, " order by year, period;"))
       r <- do.call(TSrepresentation,
                   list(res[,3], start=c(res[1,1:2]), frequency=4) )	 
      }
    else if (tbl=="M")
      {res <- Q(paste("SELECT year, period, v FROM M ",where, " order by year, period;"))
       r <- do.call(TSrepresentation,
                   list(res[,3], start=c(res[1,1:2]), frequency=12) )	 
      }
    else if (tbl=="W") 
      {res <- Q(paste("SELECT date, period, v FROM W ",where, " order by date;"))
       r <- do.call(TSrepresentation,
                   list(as.numeric(res[,3]), as.Date(res[,1])))
       # period is as.int(res[,2]) 	 
      }
    else if (tbl=="B") 
      {res <- Q(paste("SELECT date, period, v FROM B ",where, " order by date;"))
       r <- do.call(TSrepresentation,
                   list(as.numeric(res[,3]), as.Date(res[,1])))
       # period is as.int(res[,2]) 	 
      }
    else if (tbl=="D")  
      {res <- Q(paste("SELECT date, period, v FROM D ",where, " order by date;"))
       r <- do.call(TSrepresentation,
                   list(as.numeric(res[,3]), as.Date(res[,1])))
       # period is as.int(res[,2]) 	 
      }
    else if (tbl=="S")    
      {res <- Q(paste("SELECT year, period, v FROM S ",where, " order by year, period;"))
       r <- do.call(TSrepresentation,
                   list(res[,3], start=c(res[1,1:2]), frequency=2) )	 
      }
    else if (tbl=="U")  
      {res <- Q(paste("SELECT date, tz, period, v FROM U ",where, " order by date;"))
       r <- do.call(TSrepresentation,
                   list(as.numeric(res[,4]), as.Date(res[,1])))
       # tz ? period is as.int(res[,3]) 	 
      }
    else if (tbl=="I")  
      {res <- Q(paste("SELECT date, v FROM I ",where, " order by date;"))
       r <- do.call(TSrepresentation, list(as.numeric(res[,2]), as.Date(res[,1])))
      }
    else if (tbl=="T")  
      {res <- Q(paste("SELECT date, v FROM T ",where, " order by date;"))
       r <- do.call(TSrepresentation, 
                          list(as.numeric(res[,2]), as.POSIXct(res[,1])))
      }
    else stop("Specified table not found.", 
              " (Internal TSdbi or database error likely.)",
	      " looking for series ", serIDs[i],
              " could not find tbl ", tbl)  
    
    if(TSdescription) desc <- c(desc, TSdescription(serIDs[i],con) ) # where?
    if(TSdoc)   doc <- c(doc, TSdoc(serIDs[i],con) ) # where?
    mat <- tbind(mat, r)
    }

  #if(TSdescription) TSdescription(mat) <- desc
  #if(TSdoc)   TSdoc(mat)   <- doc
  if( (!all(is.na(rp))) && !all(rp == "	" ) ) TSrefPeriod(r) <- rp      
  
  seriesNames(mat) <- if(!is.null(names)) names else serIDs 
  # there seems to be a problem with con=con
  #TSsourceInfo(mat) <- new("TSsourceInfo", serIDs=serIDs, con=class(con), 
  #                        ExtractionDate= Sys.time()) 
  TSmeta(mat) <- new("TSmeta", serIDs=serIDs, dbname=con@dbname, 
      con=class(con), ExtractionDate=NA, # NULL,  # bug in 2.7.0 =Sys.time(), 
      TSdescription=if(TSdescription) desc else "", TSdoc=if(TSdoc) doc else "")
  mat
  }

setGeneric("TSdates", 
   def= function(serIDs, con=options()$TSconnection, ...)
           standardGeneric("TSdates") )

setGeneric("TSdates", 
   def= function(serIDs, con=options()$TSconnection, ...)
           standardGeneric("TSdates") )

setMethod("TSdates",   signature(serIDs="character", con="missing"),
   definition= function(serIDs, con=options()$TSconnection, ...) 
       TSdates(serIDs, con=options()$TSconnection, ...) )

#  next is for case where there is no method for con  
setMethod("TSdates",   signature(serIDs="character", con="ANY"),
   definition= function(serIDs, con=options()$TSconnection, ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdates.")
       else stop("con class ", class(con), " is not supported.")} )

TSdates.SQL <- function(serIDs, con,  
       vintage=options()$TSvintage, panel=options()$TSpanel, ...) {
  # so far I think this is generic to all SQL, but untested.
  r  <- av <- tb <- rP <- NULL
  st <- en <- list()
  for (i in seq(length(serIDs))) {
    q <- dbGetQuery(con, paste("SELECT id, tbl, refPeriod  FROM Meta ", 
                    setWhere(con, serIDs[i], vintage,panel), ";", sep=""))
    if(0==length(q)) {
        av <- c(av, FALSE)
	st <- append(st, list(NA))
	en <- append(en, list(NA))
	tb <- rbind(tb, NA)
	rP <- rbind(rP, NA)
	}
    else  {
      q2 <-  TSget(serIDs=serIDs[i], con)
      av <- c(av, TRUE)
      # paste(start(q2), collapse="-")
      st <- append(st, list(start(q2)))
      en <- append(en, list(end(q2)))
      tb <- rbind(tb, q$tb)
      rP <- rbind(rP, q$TSrefPeriod)
     }
    }
  r <- serIDs
  attr(r, "TSdates") <- av
  attr(r, "start") <- st
  attr(r, "end")   <- en
  attr(r, "tbl")   <- tb
  attr(r, "TSrefPeriod")   <- rP
  class(r) <- "TSdates"
  r
  }

start.TSdates   <- function(x, ...) attr(x, "start")
tfstart.TSdates <- function(x) attr(x, "start")
end.TSdates   <- function(x, ...) attr(x, "end")
tfend.TSdates <- function(x) attr(x, "end")

print.TSdates   <- function(x, ...) {
   r <- NULL
   av <-  attr(x, "TSdates")
   st <-  attr(x, "start")
   en <-  attr(x, "end")  
   fr <-  attr(x, "frequency")  # used by TSpadi and possibly elsewhere
   tb <-  attr(x, "tbl")  
   rP <-  attr(x, "TSrefPeriod")
   for (i in 1:length(x)) {
      if (!av[i]) r <- c(r, paste(x[i], " not available"))
      else {
        extra <- paste(tb[i], " ", rP[i], " ", fr[i])
        r <- c(r, paste(x[i], "from", paste(st[[i]], collapse=" "),
	                        "to", paste(en[[i]], collapse=" "), extra))
	}
      }
   print(as.matrix(r), ...)
   }
   

setGeneric("TSexists",
   def= function(serIDs, con=options()$TSconnection, ...)
              standardGeneric("TSexists"),
   useAsDefault= function(serIDs, con, ...){
	   ! any(is.na(attr(TSdates(serIDs, con), "tbl")))})

 
#####################################
# a little utility

TSfinddb <- function(dbname=NULL, driverOrder=c("MySQL", "SQLite", "padi")) {
  if(is.null(dbname)) stop("dbname must be supplied.")
  ok <- FALSE
  for (s in driverOrder) {
     pkg <- paste("TS", s,sep="")
     if(require(pkg, quietly = TRUE, character.only = TRUE)) {
        m <- dbDriver(s)
	con <- try(TSconnect(m, dbname), silent = TRUE)
        ok <- ! inherits(con, "try-error")
	}
     if (ok) break
     } 
  if( inherits(con, "try-error")) {
      warning(dbname, "not found.")
      return(NULL)
      }
  else return(con)
  }
