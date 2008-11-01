# these tests are not really for TSdbi, but catch other potential problems 
#  that might affect the TS db specific packages.

require("methods")

setClassUnion("OptionalPOSIXct",   c("POSIXct",   "NULL"))

setClass("TSmetax", 
  representation(serIDs="character", ExtractionDate="OptionalPOSIXct" )) 

setGeneric("TSmetax", 
   def= function(x, ...) standardGeneric("TSmetax"))

setMethod("TSmetax",   signature(x="character"),
   definition= function(x, ...){
       new("TSmetax", serIDs=x, ExtractionDate=Sys.time())
       } )


z <- new("TSmetax", serIDs="whatever", ExtractionDate= NULL)
print(z)

zz <- ts(1:10)
attr(zz, "Meta") <- z
#  As of R 2.7.0 this prints NULL as <soh>NULL<soh>
print(zz) 

###########################################################

# Work around using NA in place of NULL. 
#This is not as good because only NA should be allowed, not T or F

setClassUnion("OptionalPOSIXct",   c("POSIXct",   "logical"))

setClass("TSmetax", 
  representation(serIDs="character", ExtractionDate="OptionalPOSIXct" )) 

setGeneric("TSmetax", 
   def= function(x, ...) standardGeneric("TSmetax"))

setMethod("TSmetax",   signature(x="character"),
   definition= function(x, ...){
       new("TSmetax", serIDs=x, ExtractionDate=Sys.time())
       } )

z <- new("TSmetax", serIDs="whatever", ExtractionDate= NA)
print(z)

zz <- ts(1:10)
attr(zz, "Meta") <- z
print(zz) 
