## The main purpose of makeAnnotationHubResource() is just to create a
## class and method for end users (to automate those parts of adding
## recipe code that are always the same.

.getSrcUrl <- function(ahm){ahm@SourceUrl[1]}
.getSrcUrls <- function(ahms){
    res <- unlist(lapply(ahms, .getSrcUrl))
    names(res) <- NULL
    res
}

## TODO: point to something other than this non-used field!
## .getRDataVersion <- function(ahm){as.character(ahm@RDataVersion)[1]}
## .getRDataVersions <- function(ahms){
##     res <- unlist(lapply(ahms, .getRDataVersion))
##     names(res) <- NULL
##     res
## }

## return only AHMs in list that are in the new list, but NOT the old one.
.compareAHMs <- function(new, old){
    ## get values from AHMs
    oldSrc <- .getSrcUrls(old)
    newSrc <- .getSrcUrls(new)
    ## oldVer <- .getRDataVersions(old)
    ## newVer <- .getRDataVersions(new)
    ## I want to keep them if either the url OR if the version was different
    keepNewIdx <- !(newSrc %in% oldSrc) ##|  !(newVer %in% oldVer)
    ## then filter
    new[keepNewIdx]
}


.generalNewResources <- function(importPreparer, currentMetadata,
                                 makeAnnotationHubMetadataFunction,
                                 justRunUnitTest, ...){
    ## The 1st function must return AHMs
    ahms <- makeAnnotationHubMetadataFunction(currentMetadata, ...)
    ## Next part just does a poor mans setdiff()
    ## This is to filter in the event that the users
    ## makeAnnotationHubMetadataFunction() has not already done that
    ## job...
    ahms <- .compareAHMs(ahms, currentMetadata)
    ## Then for each remaining AHM, add in the importPreparer information
    lapply(ahms, function(x){x@PreparerClass<-class(importPreparer)[1];
                             return(x)})
}


makeAnnotationHubResource <-
    function(objName,
             makeAnnotationHubMetadataFunction,
             ...)
{
    setClass(objName,
             contains="ImportPreparer",
             package="AnnotationHubData")
    
    ## Create a newResources Method for the object type passed in.
    ## The job of this method is to only get resources that are "new"
    ## It takes an arg of "old" AHMs that can be used for filtering.    
    ## So it will call the makeAnnotationHubMetadataFunction, and then
    ## toss out any currentMetadata() AHMs that are already present.
    setMethod(newResources, objName,
              function(importPreparer, currentMetadata=list(),
                       justRunUnitTest=FALSE, ...){
         .generalNewResources(importPreparer, currentMetadata,
                              makeAnnotationHubMetadataFunction,
                              justRunUnitTest, ...)})
}





























