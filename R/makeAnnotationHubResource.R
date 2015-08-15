## The main purpose of makeAnnotationHubResource() is just to create a
## class and method for end users (to automate those parts of adding
## recipe code that are always the same.



.generalNewResources <- function(importPreparer, currentMetadata,
                                 makeAnnotationHubMetadataFunction,
                                 justRunUnitTest, BiocVersion, ...){
    ## The 1st function must return AHMs
    ahms <- makeAnnotationHubMetadataFunction(currentMetadata,
                                              justRunUnitTest=justRunUnitTest,
                                              BiocVersion, ...)

    ## Then for each remaining AHM, add in the importPreparer information
    lapply(ahms, function(x){x@PreparerClass<-class(importPreparer)[1];
                             return(x)})
}


makeAnnotationHubResource <-
    function(objName,
             makeAnnotationHubMetadataFunction,
             ..., where=topenv(parent.frame()))
{
    setClass(objName,
             contains="ImportPreparer",
             package="AnnotationHubData",
             where=where)
    
    ## Create a newResources Method for the object type passed in.
    ## The job of this method is to only get resources that are "new"
    ## It takes an arg of "old" AHMs that can be used for filtering.    
    ## So it will call the makeAnnotationHubMetadataFunction, and then
    ## toss out any currentMetadata() AHMs that are already present.
    setMethod(newResources, objName, where=where,
              function(importPreparer, currentMetadata=list(),
                       justRunUnitTest=FALSE, BiocVersion=biocVersion, ...){
         .generalNewResources(importPreparer, currentMetadata,
                              makeAnnotationHubMetadataFunction,
                              justRunUnitTest, BiocVersion, ...)})
}





























