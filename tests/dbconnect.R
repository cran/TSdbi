#  The real tests are in the database specific packages

require("TSdbi")

if(require("RMySQL") ) {
  # Before starting R you need to set user/passwd/host in ~/.my.cnf
  m <- dbDriver("MySQL")
  con <- try(dbConnect(m, dbname="test")) # pass user/passwd/host in ~/.my.cnf
  if(! inherits(con, "try-error")) {
     dbListTables(con) 
     cat("**************        disconnecting RMySQL test\n")
     dbDisconnect(con)
     } else  warning("RMySQL server error. Skipping tests.")
  dbUnloadDriver(m)
  } else  warning("RMySQL not available. Skipping tests.")

if(require("RSQLite") ) {

  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname="test") # no user/passwd/host
  dbListTables(con) 

  cat("**************        disconnecting SQLite test\n")
  dbDisconnect(con)
  #  dbUnloadDriver(m) complains about open connections.
  } else  warning("RSQLite not available. Skipping tests.")
