#.onLoad <- function(library, section) {require("methods")}

#setClassUnion("OptionalPOSIXct",   c("POSIXct",   "NULL"))
# bug work around
setClassUnion("OptionalPOSIXct",   c("POSIXct",   "logical"))


# inherit this into TSconnections so methods can abstract  from
#   specific drivers 
setClass("conType", representation( drv="character"), "VIRTUAL" )

# this just has db info
setClass("TSdb", representation( dbname="character", 
    hasVintages="logical", hasPanels="logical"), "VIRTUAL" )

#The connection type is the class of an actual connection, which includes the
# above virtual class. The conType in next TSid duplicates this, but otherwise
# I don't see how to put the info in the TSid.

# this has serIDs as well as there source db (so could be used to retrieve data)
setClass("TSid",  contains="TSdb", representation(serIDs="character", 
                     conType="character",DateStamp="OptionalPOSIXct")) 

# this has serIDs and source db (in TDid) as well as documentation
setClass("TSmeta", contains="TSid", 
  representation(TSdescription="character", TSdoc="character", TSlabel="character" )) 

# this is a logical but has TSid (so could be used to retrieve data)
setClass("logicalId", contains="logical",
            representation=representation(TSid="TSid")) 

setGeneric("TSmeta", def= function(x, con=getOption("TSconnection"), ...)
    standardGeneric("TSmeta"))

setMethod("show", "logicalId", function(object) show(object@.Data))


# this really should be for con="TSconnection" rather than ANY
setMethod("TSmeta",   signature(x="character", con="ANY"),
   definition= function(x, con=getOption("TSconnection"), ...){
       new("TSmeta", serIDs=x, dbname=con@dbname, 
               hasVintages=con@hasVintages, hasPanels=con@hasPanels,
               conType=class(con), DateStamp=NA, 
	       TSdoc=TSdoc(x, con=con, ...),
	       TSlabel=TSlabel(x, con=con, ...),
               TSdescription=TSdescription(x, con=con, ...))
	} )

setMethod("TSmeta",   signature(x="character", con="missing"),
   definition= function(x, con=getOption("TSconnection"), ...) 
       TSmeta(x, con=getOption("TSconnection"), ...) )

# extract meta data from an object, not from the db
setMethod("TSmeta",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...) {
      m <- attr(x, "TSmeta")
      if(is.null(m)) new("TSmeta",serIDs=seriesNames(x), 
	         dbname="", hasVintages=FALSE, hasPanels=FALSE,
	         conType="", DateStamp=NA, #NULL,
		 TSdescription= as(NA, "character"), 
		 TSdoc =  as(NA, "character"), 
		 TSlabel= as(NA, "character")) else m
      })

setGeneric("TSmeta<-", 
   def= function(x, value) standardGeneric("TSmeta<-"),
   useAsDefault= function(x, value){
      if (!is(value,"TSmeta")) stop("trying to set attribute TSmeta incorrectly.") 
      attr(x, "TSmeta") <- value
      x
      })

#setMethod("show", "TSdb", function(object) {
#    cat("database ", object@dbname) 
#    if (object@vintage) cat( " Has vintages." )
#    if (object@panel) cat( " Has panels." )
#    cat("\n") 
#    invisible(object)
#    })
#
#setMethod("print", "TSdb", function(x, ...) {
#    cat("database ", x@dbname) 
#    if (x@vintage) cat( " Has vintages." )
#    if (x@panel) cat( " Has panels." )
#    cat("\n") 
#    invisible(x)
#    })

setGeneric("TSconnect", def= function(drv, dbname, ...) standardGeneric("TSconnect"))

setMethod("TSconnect",   signature(drv="character", dbname="character"),
   definition=function(drv, dbname, ...)
             TSconnect(dbDriver(drv), dbname=dbname, ...))

setGeneric("TSdescription<-", 
   def= function(x, value) standardGeneric("TSdescription<-"),
   useAsDefault=  function (x, value){
    m <- TSmeta(x)
    m@TSdescription <- value
    TSmeta(x) <- m
    x})

setGeneric("TSdescription", def= function(x, con=getOption("TSconnection"), ...)
    standardGeneric("TSdescription"))

setMethod("TSdescription",   signature(x="character", con="missing"),
   definition= function(x, con=getOption("TSconnection"), ...) 
       TSdescription(x, con=getOption("TSconnection"), ...) )

setMethod("TSdescription",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...)TSmeta(x)@TSdescription)

#  next is for case where there is no method for con  
setMethod("TSdescription",   signature(x="character", con="ANY"),
   definition= function(x, con=getOption("TSconnection"), ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdescription.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

#  Next two methods are for case where the user mistakenly specifies
#     serIDS="whatever"  rather than x="whatever"
#   ( A natural mistaken as this is the syntax for other functions.)
setMethod("TSdescription",   signature(x="missing", con="ANY"),
   definition=  function(x, con, serIDs, ...) {
        if(missing(serIDs)) stop("missing argument x, must be specified.")
	else TSdescription(x=serIDs, con=con, ...)
	})

setMethod("TSdescription",   signature(x="missing", con="missing"),
   definition=  function(x, serIDs, ...) {
        if(missing(serIDs)) stop("missing argument x, must be specified.")
	else TSdescription(x=serIDs, ...)
	})

# internal utilities to construct WHERE and table name

tbNm <- function(hasVintages, tbl, rV) {
	if(hasVintages) tbl <- paste(tbl, rV, sep="_")
	# map - to _ in table names
	gsub("-", "_",tbl)
	}

realVintage <- function(con, vintage, serIDs) {
   # replace alias with canonical name if necessary
   # assuming if vintage is a vector then serIDs has already been expanded to
   # the same length. Otherwise, vintage should be null or a scalar which is
   # expanded to the length of serIDs. 
   if(!con@hasVintages) return(vintage) #usually NULL in this case 
   
   if(is.null(vintage)) vintage <- "current"
   else if( 1 == length(vintage)) vintage <- rep(vintage, length(serIDs))
   
   rV <- NULL
   for (i in seq(length(serIDs))){
     q <- paste("SELECT vintage  FROM vintageAlias WHERE alias ='",
              vintage[i],"' AND id = '",serIDs[i],"';", sep="") 
     r <- dbGetQuery(con,q )$vintage
     # if alias result is empty check null id which applies to all series.
     if (0== NROW(r)) {
        q <- paste( "SELECT vintage  FROM vintageAlias WHERE alias='", 
	            vintage[i],"' ;", sep="")
        r <- dbGetQuery(con,q )$vintage
        }
     # if alias result is still empty assume vintage is already the real one.
     if (0== NROW(r)) r <- vintage[i]
     rV <- c(rV, r)
     }
   rV
   }

realPanel <- function(con, panel) {
   # replace alias with canonical name if necessary
   if(!con@hasPanels) return(panel) #usually NULL in this case
   if(is.null(panel)) stop("panel must be specified")
   for (i in seq(length(panel))){
     q <- paste("SELECT panel  FROM panelAlias WHERE alias='",
                panel[i],"';", sep="") 

     r <- dbGetQuery(con,q )$panel
     # if alias result is empty assume panel is already the real one.
     if (0== NROW(r)) r <- panel[i]
     rP <- c(rP, r)
     }
   rP
   }

setWhere <- function(con, serIDs, realVintage, realPanel) {
   # serIDs must be a scale for this function
   # Calls for Meta will pass realVintage, but data tables will set 
   #  realVintage=NULL and append vintage to table name.
   where <-  paste(" WHERE id = '", serIDs, "'", sep="")
   if(!is.null(realVintage))
      where <- paste(where, " AND vintage='", realVintage, "'", sep="")
   if(!is.null(realPanel)) 
      where <- paste(where, " AND panel='",   realPanel,   "'", sep="")
   where
   }

TSdescriptionSQL <-  function(x=NULL, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"), 
       lang=getOption("TSlang")) {	    
            r <- dbGetQuery(con, paste("SELECT description", lang, 
	            "  FROM Meta ", setWhere(con, x, 
		        realVintage(con, vintage, x),
		        realPanel(con,panel)), ";", sep=""))[[1]]
	    # r should already be char, but odbc converts NA to logical
	    if(is.null(r))  as(NA, "character") else as(r, "character")
	    }

setGeneric("TSdoc<-", 
   def= function(x, value) standardGeneric("TSdoc<-"),
   useAsDefault=  function (x, value){
    m <- TSmeta(x)
    m@TSdoc <- value
    TSmeta(x) <- m
    x})


setGeneric("TSdoc", 
   def= function(x, con=getOption("TSconnection"), ...) standardGeneric("TSdoc"))

setMethod("TSdoc",   signature(x="character", con="missing"),
   definition= function(x, con=getOption("TSconnection"), ...){ 
       if(is.null(con)) 
	  stop("con should be specified or set with options(TSconnection=con). See ?TSdoc.") 
       TSdoc(x, con=con, ...)} )

setMethod("TSdoc",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...) TSmeta(x)@TSdoc)

#  next is for case where there is no method for con  
setMethod("TSdoc",   signature(x="character", con="ANY"),
   definition= function(x, con=getOption("TSconnection"), ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdoc.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

#  Next two methods are for case where the user mistakenly specifies
#     serIDS="whatever"  rather than x="whatever"
#   ( A natural mistaken as this is the syntax for other functions.)
setMethod("TSdoc",   signature(x="missing", con="ANY"),
   definition=  function(x, con, serIDs, ...) {
        if(missing(serIDs)) stop("missing argument x, must be specified.")
	else TSdoc(x=serIDs, con=con, ...)
	})

setMethod("TSdoc",   signature(x="missing", con="missing"),
   definition=  function(x, serIDs, ...) {
        if(missing(serIDs)) stop("missing argument x, must be specified.")
	else TSdoc(x=serIDs, ...)
	})

TSdocSQL <-  function(x=NULL, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"), 
       lang=getOption("TSlang")) {
            if(1 < length(x)) stop("One series only for TSdoc")
            r <- dbGetQuery(con, paste("SELECT documentation", lang, 
	            "  FROM Meta ", setWhere(con, x, 
		        realVintage(con, vintage, x),
		        realPanel(con,panel)), ";", sep=""))[[1]]
	    # r should already be char, but odbc converts NA to logical
	    if(is.null(r))  as(NA, "character") else as(r, "character")
	    }

setGeneric("TSlabel<-", 
   def= function(x, value) standardGeneric("TSlabel<-"),
   useAsDefault=  function (x, value){
    m <- TSmeta(x)
    m@TSlabel <- value
    TSmeta(x) <- m
    x})


setGeneric("TSlabel", 
   def= function(x, con=getOption("TSconnection"), ...) standardGeneric("TSlabel"))

setMethod("TSlabel",   signature(x="character", con="missing"),
   definition= function(x, con=getOption("TSconnection"), ...){ 
       if(is.null(con)) 
          stop("con should be specified or set with options(TSconnection=con). See ?TSlabel.") 
       TSlabel(x, con=con, ...)} )

setMethod("TSlabel",   signature(x="ANY", con="missing"),
   definition=  function(x, con, ...) TSmeta(x)@TSlabel)

#  next is for case where there is no method for con  
setMethod("TSlabel",   signature(x="character", con="ANY"),
   definition= function(x, con=getOption("TSconnection"), ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSlabel.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

#  Next two methods are for case where the user mistakenly specifies
#     serIDS="whatever"  rather than x="whatever"
#   ( A natural mistaken as this is the syntax for other functions.)
setMethod("TSlabel",   signature(x="missing", con="ANY"),
   definition=  function(x, con, serIDs, ...) {
        if(missing(serIDs)) stop("missing argument x, must be specified.")
	else TSlabel(x=serIDs, con=con, ...)
	})

setMethod("TSlabel",   signature(x="missing", con="missing"),
   definition=  function(x, serIDs, ...) {
        if(missing(serIDs)) stop("missing argument x, must be specified.")
	else TSlabel(x=serIDs, ...)
	})

TSlabelSQL <-  function(x=NULL, con=getOption("TSconnection"), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"), 
       lang=getOption("TSlang")) {
            if(1 < length(x)) stop("One series only for TSlabel")
	    #  NOT YET
            #r <- dbGetQuery(con, paste("SELECT label", lang, 
	    #        "  FROM Meta ", setWhere(con, x, 
	    #            realVintage(con, vintage, x),
	    #	         realPanel(con,panel)), ";", sep=""))[[1]]
	    ## r should already be char, but odbc converts NA to logical
	    #if(is.null(r)) as(NA, "character") else as(r, "character")
	    as(NA, "character")
	    }

#setMethod("show", "TSmeta", function(object) {
#    cat("serIDs: ", object@serIDs, " from dbname: ", object@dbname) 
#    cat("(type: ", object@conType, ")\n") 
#    cat("description: ", object@TSdescription, "\n") 
#    cat("documentaion: ", object@TSdoc, "\n") 
#    invisible(object)
#    })
#
#setMethod("print", "TSmeta", function(x, ...) {
#    cat("serIDs: ", x@serIDs, " from dbname ", x@dbname) 
#    cat("(type: ", x@conType, ")\n") 
#    cat("description: ", x@TSdescription, "\n") 
#    cat("documentaion: ", x@TSdoc, "\n") 
#    invisible(x)
#    })


setGeneric("TSrefperiod", 
   def= function(x) standardGeneric("TSrefperiod"),
   useAsDefault= function(x) attr(x, "TSrefperiod"))

setGeneric("TSrefperiod<-", 
   def= function(x, value) standardGeneric("TSrefperiod<-"),
   useAsDefault= function (x, value){attr(x, "TSrefperiod") <- value ; x })


TSseriesIDs      <- function(x)TSmeta(x)@serIDs
TScon            <- function(x)TSmeta(x)@con
TSextractionDate <- function(x)TSmeta(x)@DateStamp

# host = "" is interpreted as localhost by the driver (see ?mysqlNewConnection)
#see  vignette("zoo") for other examples


setGeneric("TSput", valueClass="logicalId",   
   def= function(x, serIDs=seriesNames(x), con=getOption("TSconnection"), ...)
             standardGeneric("TSput")) 

setMethod("TSput",   signature(x="ANY", serIDs="character", con="missing"),
   definition= function(x, serIDs, con=getOption("TSconnection"), ...){ 
       if(is.null(con)) 
          stop("con should be specified or set with options(TSconnection=con). See ?TSput.") 
       TSput(x, serIDs=serIDs, con=con, ...)})

setMethod("TSput",   signature(x="ANY", serIDs="missing", con="missing"),
   definition= function(x, serIDs, con=getOption("TSconnection"), ...){ 
       if(is.null(con)) 
          stop("con should be specified or set with options(TSconnection=con). See ?TSput.")  
       TSput(x, serIDs=seriesNames(x), con=con, ...)})

setMethod("TSput",   signature(x="ANY", serIDs="DBIConnection", con="missing"),
   definition= function(x, serIDs, con, ...)
          TSput(x, serIDs=seriesNames(x), con=serIDs, ...))

#  next is for case where there is no method for con  
setMethod("TSput",   signature(x="ANY", serIDs="character", con="ANY"),
   definition= function(x, serIDs=seriesNames(x), con=getOption("TSconnection"), ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSput.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

   

TSputSQL <- function(x, serIDs=seriesNames(x), con, Table=NULL,
       TSdescription.=TSdescription(x), TSdoc.=TSdoc(x),  TSlabel.=TSlabel(x), 
       vintage=getOption("TSvintage"), panel=getOption("TSpanel")) {

  # so far I think this is generic to all SQL, but not extensively tested.
  # should rollback meta when data put fails
  #  (reversing order of M and P would mean data can change and 
  #    then meta fail, which seems worse.)
  panel <- realPanel(con,panel)
  # vintage <- realVintage(con,vintage, x) No. To put, vintage must already be real.
  # M does the write to Meta, P writes values to data table.

  # Column order could be specified after Meta, but this assumes  order
  #([vintage,] [ panel,] id, tbl, refperiod, description, documentation )
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

  P <- function(p, tableV, columns, id, ...) {
    x <- list(...)
    vl <- id
    if(!is.null(p))  {
      vl <- paste(p,vl, sep=",")
      columns <- paste("panel,",columns, sep="")
      }
    for (i in 1:length(x)) vl <- paste(vl, x[[i]], sep="', '")
    # SQL wants NULL (not 'NULL') for NA
    vl <-   gsub("'NA'", "NULL", paste(vl,"'", sep=""))
    dbGetQuery(con, paste("DELETE FROM  ",tableV,
                     setWhere(con,id[1],NULL,panel), ";", sep="")) 
    for (i in seq(length(vl))){
      q <- paste("INSERT INTO ",tableV, " (", columns,
                     ") VALUES ('", vl[i], ") ", sep="")  
      dbGetQuery(con, q) 
      }
    # next if is RSQLite BUG work around
    if(is(con, "SQLiteConnection")) return(TRUE)
    0 == dbGetException(con)$errorNum
    }

  if (con@hasVintages) {
     hV <- TRUE
     vintage <- realVintage(con,vintage, serIDs)
     }
  else hV <- FALSE 

  # as.matrix(x) clobbers seriesNames(x)
  ids <-  serIDs 
  #ids <- gsub(" ", "", serIDs ) # remove spaces in id
  #if(! all( ids == serIDs)) warning("spaces removed from series names.")
  rP <- TSrefperiod(x)
  #N <- NROW(x) # could be periods() (or Tobs())
  # SHOULD PROBABLY HAVE METHODS FOR TS, ZOO, ITS, ... HERE
  if(is.ts(x)) {
    fr <-frequency(x) 
    y <- floor(time(x))
    p <- 1 + round(frequency(x) * (time(x) %% 1 ))
    x <- as.matrix(x)
    if(1==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "A",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(panel, tbNm(hV, "A", vintage), "id, year, v", ids[i], y, x[,i])
        }
    else if(2==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "S",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(panel, tbNm(hV, "S", vintage), "id, year, period, v", ids[i], y, p, x[,i])
        }
    else if(4==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "Q",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(panel, tbNm(hV, "Q", vintage), "id, year, period, v", ids[i], y, p, x[,i])
        }
    else if(12==fr)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "M",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(panel, tbNm(hV, "M", vintage), "id, year, period, v", ids[i], y, p, x[,i])
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
	ok <- P(panel, tbNm(hV, "W", vintage), "id, date, period, v", ids[i], d, p$mday, x[,i])
        }
    else if("B" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "B",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        # period should be business day of the year 1- ~260
	#  but need to map holidays
        ok <- P(panel, tbNm(hV, "B", vintage), "id, date, period, v", ids[i], d, p$yday, x[,i])
        }
    else if("D" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "D",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        # period should be day of the year 1- 365/6
        ok <- P(panel, tbNm(hV, "D", vintage), "id, date, period, v", ids[i], d, p$yday, x[,i])
        }
    else if("U" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "U",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        tz <- attr(p, "tzone")
	ok <- P(panel, tbNm(hV, "U", vintage), "id, date, period, v", ids[i], d, tz, p$mday,x[,i])
        }
    else if("T" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "T",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(panel, tbNm(hV, "T", vintage), "id, date, v", ids[i], d, x[,i])
        }
    else if("I" == Table)   for (i in seq(nseries(x))) {  
	okM <-  M(vintage, panel, "I",  ids[i], rP[i], TSdescription.[i], TSdoc.[i])
        ok <- P(panel, tbNm(hV, "I", vintage), "id, date, v", ids[i], d, x[,i])
        }
    else stop("Table specification not supported.")  
     }
  else stop("Time series type not recognized.")  
  new("logicalId", ok & okM, 
        TSid=new("TSid", serIDs=serIDs, dbname=con@dbname, 
	         conType=class(con), hasVintages=con@hasVintages, hasPanels=con@hasPanels,
		 DateStamp=NA))
  }


setGeneric("TSreplace",  valueClass="logicalId",
   def= function(x, serIDs=seriesNames(x), con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
              standardGeneric("TSreplace"),
   useAsDefault= function(x,serIDs=seriesNames(x), con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"),  ...) {
      if(missing(con) & (!missing(serIDs)) && is(serIDs, "DBIConnection")) 
	     return(TSreplace(x, serIDs=seriesNames(x), con=serIDs,
	             vintage=vintage, panel=panel, ...))
      if(TSexists(serIDs=serIDs, con=con, vintage=vintage, panel=panel,...))
         TSdelete(serIDs=serIDs, con=con, vintage=vintage, panel=panel, ...)
         TSput(x, serIDs=serIDs, con=con, vintage=vintage, panel=panel, ...) 
   })


setGeneric("TSdelete",  valueClass="logicalId",
   def= function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
              standardGeneric("TSdelete") )

setMethod("TSdelete",   
   signature(serIDs="character", con="missing", vintage="ANY", panel="ANY"),
   definition= function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...){ 
       if(is.null(con)) 
          stop("con should be specified or set with options(TSconnection=con). See ?TSdelete.") 
       TSdelete(serIDs, con=con, vintage=vintage, panel=panel, ...)} )

#  next is for case where there is no method for con  
setMethod("TSdelete",   
   signature(serIDs="character", con="ANY", vintage="ANY", panel="ANY"),
   definition= function(serIDs, con=getOption("TSconnection"),
        vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdelete.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

   
TSdeleteSQL <- function(serIDs, con=getOption("TSconnection"),  
   vintage=getOption("TSvintage"), panel=getOption("TSpanel")) {
     for (i in seq(length(serIDs))) {
     	rv <- realVintage(con, vintage, i)
	rp <- realPanel(con,panel)
	where  <-  setWhere(con, serIDs[i], rv,   rp)
     	whereT <-  setWhere(con, serIDs[i], NULL, rp)
     	q <- dbGetQuery(con, paste("SELECT tbl  FROM Meta ",where, ";"))
	tbl <- tbNm(con@hasVintages, q$tbl, rv)
	if(0 != length(q)) {
     	   dbGetQuery(con, paste("DELETE FROM ", tbl, whereT, ";")) 
     	   dbGetQuery(con, paste("DELETE FROM Meta ",	where, ";")) 
     	   }
     	 }
     # could do better checking here
     new("logicalId", TRUE, 
          TSid=new("TSid", serIDs=serIDs, dbname=con@dbname, 
             conType=class(con), hasVintages=con@hasVintages, hasPanels=con@hasPanels, 
	     DateStamp=NA ))
     }


setGeneric("TSget", 
   def= function(serIDs, con=getOption("TSconnection"), ...)
           standardGeneric("TSget") )

setMethod("TSget",   signature(serIDs="character", con="missing"),
   definition= function(serIDs, con=getOption("TSconnection"), ...){ 
       if(is.null(con)) 
          stop("con should be specified or set with options(TSconnection=con). See ?TSget.")
       TSget(serIDs, con=con, ...)} )

#  next is for case where there is no method for con  
setMethod("TSget",   signature(serIDs="character", con="ANY"),
   definition= function(serIDs, con=getOption("TSconnection"), ...){
       if(is.null(con)) stop("NULL con is not allowed. See ?TSget.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

# next is called by methods for various SQL dbs.
TSgetSQL <- function(serIDs, con, TSrepresentation=getOption("TSrepresentation"),
       tf=NULL, start=tfstart(tf), end=tfend(tf),
       names=NULL, TSdescription=FALSE, TSdoc=FALSE,  TSlabel=FALSE,
       vintage=getOption("TSvintage"), panel=getOption("TSpanel")) {
  # so far I think this is generic to all SQL.
  if(is.null(TSrepresentation)) TSrepresentation <- "default"

  if ( 1 < sum(c(length(serIDs), length(panel), length(vintage)) > 1))
   stop("Only one of serIDs, panel, or vintage can have length greater than 1.")

  if(is.null(names)) names <- 
         if ( length(panel)   > 1 )  panel   else
         if ( length(vintage) > 1 ) vintage  else  serIDs 

  panel <- realPanel(con,panel)
  # if vintage is a vector then serIDs needs to be expanded
  if ( 1 < length(vintage)) serIDs <- rep(serIDs, length(vintage))
  # next returns a vector of length equal serIDs
  if (con@hasVintages) {
     hV <- TRUE
     vintage <- realVintage(con,vintage, serIDs)
     }
  else hV <- FALSE 

  Q <- function(q) {# local function
      res <- dbGetQuery(con, q)
      if(any(dim(res) == 0)) stop("empty query result.")
      as.matrix(res)
      }

  mat <- desc <- doc <- label <- rp <- NULL
  # if series are in "A", "Q", "M","S" use  ts otherwise zoo.
  for (i in seq(length(serIDs))) {
    where  <-  setWhere(con, serIDs[i], vintage[i], panel)
    whereT <-  setWhere(con, serIDs[i], NULL,       panel)
    for (j in seq(length(where))) {
    qq <- paste("SELECT tbl, refperiod  FROM Meta ",where[j], ";")
    q <- dbGetQuery(con, qq)
    if(0 == NROW(q$tbl))
       stop("Meta lookup for series ", serIDs[i], 
            " failed. (Result empty for query: ",
	     qq, ") Series does not exist on database.")

    if  (i == 1) {
       tbl <- q$tbl
       useZoo <- if ((TSrepresentation == "zoo") | 
                   !(tbl %in% c("A", "Q", "M","S"))) TRUE else FALSE 
       if(useZoo && !require("zoo")) stop("zoo package is required.")
       }
    else if(q$tbl != tbl) 
       stop("Series must all have the same frequency or time representation.")
    rp <- c(rp, q$refperiod)

    if (tbl=="A") 
      {res <- Q(paste("SELECT year, v FROM ", tbNm(hV, "A", vintage[i]), 
                whereT[j], " order by year;"))
       r   <- ts(res[,2], start=c(res[1,1], 1), frequency=1) 
       if(useZoo) r <- as.zoo(r)
     }
    else if (tbl=="Q")  
      {res <- Q(paste("SELECT year, period, v FROM ", tbNm(hV, "Q", vintage[i]),
                whereT[j], " order by year, period;"))
       r   <- ts(res[,3], start=c(res[1,1:2]), frequency=4) 	 
       if(useZoo) r <- as.zoo(r)
      }
    else if (tbl=="M")
      {res <- Q(paste("SELECT year, period, v FROM ", tbNm(hV, "M", vintage[i]),
                whereT[j], " order by year, period;"))
       r   <- ts(res[,3], start=c(res[1,1:2]), frequency=12)	 
       if(useZoo) r <- as.zoo(r)
      }
    else if (tbl=="W") 
      {res <- Q(paste("SELECT date, period, v FROM ", tbNm(hV, "W", vintage[i]),
                whereT[j], " order by date;"))
       r   <- zoo(as.numeric(res[,3]), as.Date(res[,1]))
       # period is as.int(res[,2]) 	 
      }
    else if (tbl=="B") 
      {res <- Q(paste("SELECT date, period, v FROM ", tbNm(hV, "B", vintage[i]),
                whereT[j], " order by date;"))
       r   <- zoo(as.numeric(res[,3]), as.Date(res[,1]))
       # period is as.int(res[,2]) 	 
      }
    else if (tbl=="D")  
      {res <- Q(paste("SELECT date, period, v FROM ", tbNm(hV, "D", vintage[i]),
                whereT[j], " order by date;"))
       r   <- zoo(as.numeric(res[,3]), as.Date(res[,1]))
       # period is as.int(res[,2]) 	 
      }
    else if (tbl=="S")    
      {res <- Q(paste("SELECT year, period, v FROM ", tbNm(hV, "S", vintage[i]),
                whereT[j], " order by year, period;"))
       r   <- ts(res[,3], start=c(res[1,1:2]), frequency=2)	 
       if(useZoo) r <- as.zoo(r)
      }
    else if (tbl=="U")  
      {res <- Q(paste("SELECT date, tz, period, v FROM U ",tbNm(hV,"U",vintage[i]),
                whereT[j], " order by date;"))
       r   <- zoo(as.numeric(res[,4]), as.Date(res[,1]))
       # tz ? period is as.int(res[,3]) 	 
      }
    else if (tbl=="I")  
      {res <- Q(paste("SELECT date, v FROM ", tbNm(hV, "I", vintage[i]), 
                whereT[j], " order by date;"))
       r   <- zoo(as.numeric(res[,2]), as.Date(res[,1]))
      }
    else if (tbl=="T")  
      {res <- Q(paste("SELECT date, v FROM ", tbNm(hV, "T", vintage[i]), 
                whereT[j], " order by date;"))
       r   <- zoo(as.numeric(res[,2]), as.POSIXct(res[,1]))
      }
    else stop("Specified table not found.", 
              " (Internal TSdbi or database error likely.)",
	      " looking for series ", serIDs[i],
              " could not find tbl ", tbl)  
    
    if(TSdescription) desc <- c(desc, TSdescription(serIDs[i],con) ) # where?
    if(TSdoc)         doc  <- c(doc,  TSdoc(serIDs[i],con) ) # where?
    if(TSlabel)       label<- c(label,TSlabel(serIDs[i],con) ) # where?

    mat <- tbind(mat, r)
    } # where[j]
    } # serID[i]
  mat <- tfwindow(mat, tf=tf, start=start, end=end)
  if( (!all(is.na(rp))) && !all(rp == "	" ) ) TSrefperiod(mat) <- rp      
  
  if (! TSrepresentation  %in% c( "zoo", "default")){
      require("tframePlus")
      mat <- changeTSrepresentation(mat, TSrepresentation)
      }

  seriesNames(mat) <- names

  TSmeta(mat) <- new("TSmeta", serIDs=serIDs, dbname=con@dbname, 
      conType=class(con), hasVintages=con@hasVintages, hasPanels=con@hasPanels, 
      DateStamp=NA, # bug in 2.7.0 =Sys.time(), 
      TSdescription=if(TSdescription) desc else  as(NA, "character"), 
      TSdoc        =if(TSdoc)          doc else  as(NA, "character"),
      TSlabel      =if(TSlabel)      label else  as(NA, "character"))
  mat
  }

setGeneric("TSdates", def=
   function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
   standardGeneric("TSdates") )

setMethod("TSdates",
   signature(serIDs="character", con="missing", vintage="ANY", panel="ANY"),
   definition= function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...){ 
       if(is.null(con)) 
          stop("con should be specified or set with options(TSconnection=con). See ?TSdates.") 
       TSdates(serIDs, con=con, vintage=vintage, panel=panel, ...)} )

#  next is for case where there is no method for con  
setMethod("TSdates",
   signature(serIDs="character", con="ANY", vintage="ANY", panel="ANY"),
   definition= function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...) {
       if(is.null(con)) stop("NULL con is not allowed. See ?TSdates.")
       else stop("con class ", class(con), 
       " is not supported. (Check this is a TSdbi connection, not a raw DBI connection.)")} )

TSdatesSQL <- function(serIDs, con,  
       vintage=getOption("TSvintage"), panel=getOption("TSpanel")) {
  # so far I think this is generic to all SQL, but untested.
  r  <- av <- tb <- rP <- NULL
  st <- en <- list()
  vintage <- realVintage(con, vintage, serIDs)
  panel   <- realPanel(con,panel)
  for (i in seq(length(serIDs))) {
    q <- dbGetQuery(con, paste("SELECT id, tbl, refperiod  FROM Meta ", 
                    setWhere(con, serIDs[i], vintage[i], panel), ";", sep=""))
    if(is.null(q) || nrow(q) == 0) {
        av <- c(av, FALSE)
	st <- append(st, list(NA))
	en <- append(en, list(NA))
	tb <- rbind(tb, NA)
	rP <- rbind(rP, NA)
	}
    else if(nrow(q) > 1)
      warning("More than one series with the same identifier. Possible database corruption.")
    else  {
      q2 <-  TSget(serIDs=serIDs[i], con, vintage=vintage, panel=panel)
      av <- c(av, TRUE)
      # paste(start(q2), collapse="-")
      st <- append(st, list(start(q2)))
      en <- append(en, list(end(q2)))
      tb <- rbind(tb, q$tb)
      rP <- rbind(rP, q$TSrefperiod)
     }
    }
  r <- serIDs
  attr(r, "TSdates") <- av
  attr(r, "start") <- st
  attr(r, "end")   <- en
  attr(r, "tbl")   <- tb
  attr(r, "TSrefperiod")   <- rP
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
   rP <-  attr(x, "TSrefperiod")
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
   

setGeneric("TSexists", valueClass="logicalId",
 def= function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
 	    standardGeneric("TSexists"),
 useAsDefault= function(serIDs, con=getOption("TSconnection"), 
           vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...){
    new("logicalId", ! any(is.na(attr(
       TSdates(serIDs, con, vintage=vintage, panel=panel, ...), "tbl"))), 
       TSid=new("TSid", serIDs=serIDs, dbname=con@dbname, 
              conType=class(con), hasVintages=con@hasVintages, hasPanels=con@hasPanels,
	      DateStamp=NA))})


#####################################
# this method will generally not be needed by users, but is used in the test
# database setup. It needs to be generic in order to work around the problem
# that different db engines treat capitalized table names differently.
# e.g. MySQL uses table name Meta while Posgresql converts to meta.
# A default con is not used on purpose.

setGeneric("dropTStable",
   def= function(con=NULL,Table, yesIknowWhatIamDoing=FALSE)
    standardGeneric("dropTStable"))

########## a little utility#######

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
