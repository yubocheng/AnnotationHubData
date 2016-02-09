### =========================================================================
### makeAnnotationHubResource()
### -------------------------------------------------------------------------
###

## Creates a Preparer class and associated newResource() method.
.generalNewResources <- function(importPreparer, currentMetadata,
                                 makeAnnotationHubMetadataFunction, ...)
{
    ## returns metadata 
    ahms <- makeAnnotationHubMetadataFunction(currentMetadata, ...)

    ## add the importPreparer  
    lapply(ahms, function(x) {
        x@PreparerClass<-class(importPreparer)[1]
        x
    })
}

makeAnnotationHubResource <- function(objName, 
                                      makeAnnotationHubMetadataFunction,
                                      ..., where=topenv(parent.frame()))
{
    ## create class
    setClass(objName,
             contains="ImportPreparer",
             package="AnnotationHubData",
             where=where)
   
    ## FIXME: This doesn't seem to be the case - ie, no handling of 'old'.
    ## The job of this method is to only get resources that are "new"
    ## It takes an arg of "old" AHMs that can be used for filtering.    
    ## So it will call the makeAnnotationHubMetadataFunction, and then
    ## toss out any currentMetadata() AHMs that are already present.

    ## create newResources method 
    setMethod(newResources, objName, where=where,
              function(importPreparer, currentMetadata=list(), ...) 
    {
        .generalNewResources(importPreparer, currentMetadata,
                             makeAnnotationHubMetadataFunction, ...)
    })
}
