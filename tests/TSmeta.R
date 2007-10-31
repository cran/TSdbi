#  The real tests are in the database specific packages

require("TSdbi")

z <- ts(1:10)

TSmeta(z)

TSdescription(z) <- "test series"
TSmeta(z)
TSdescription(z) 
TSdoc(z) 

TSdoc(z) <- "test series with documentation added."
TSmeta(z)
TSdescription(z) 
TSdoc(z) 

if(TSdescription(z) != "test series") stop("TSdescription failed")
if(TSdoc(z)  != "test series with documentation added.") stop("TSdoc failed")

z <- ts(1:10)
seriesNames(z) <- "simplets"
TSmeta(z)
