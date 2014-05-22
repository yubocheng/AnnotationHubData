## The main purpose of makeAnnotationHubResource() is just to create a
## class and method for end users (to automate those parts of adding
## recipe code that are always the same.

.getSrcUrl <- function(ahm){ahm@SourceUrl}
.getSrcUrls <- function(ahms){
    res <- unlist(lapply(ahms, .getSrcUrl))
    names(res) <- NULL
    res
}

.getRDataVersion <- function(ahm){as.character(ahm@RDataVersion)}
.getRDataVersions <- function(ahms){
    res <- unlist(lapply(ahms, .getRDataVersion))
    names(res) <- NULL
    res
}

## return only AHMs in list that are in the new list, but NOT the old one.
.compareAHMs <- function(new, old){
    ## get values from AHMs
    oldSrc <- .getSrcUrls(old)
    newSrc <- .getSrcUrls(new)
    oldVer <- .getRDataVersions(old)
    newVer <- .getRDataVersions(new)
    ## I want to keep them if either the url OR if the version was different
    keepNewIdx <- !(newSrc %in% oldSrc) |  !(newVer %in% oldVer)
    ## then filter
    new[keepNewIdx]
}


.generalNewResources <- function(importPreparer, currentMetadata,
                                 makeAnnotationHubMetadataFunction, ...){
    ## The 1st function must return AHMs
    ahms <- makeAnnotationHubMetadataFunction(currentMetadata, ...)
    ## Next part just does a poor mans setdiff()
    ## This is to filter in the event that the users
    ## makeAnnotationHubMetadataFunction() has not already done that
    ## job...
    .compareAHMs(ahms, currentMetadata)
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
              function(importPreparer, currentMetadata=list(), ...){
         .generalNewResources(importPreparer, currentMetadata,
                              makeAnnotationHubMetadataFunction, ...)})
}

































## ## This can't work because there is not a function???  That is, I have
## ## no way to generate the AHMs on the fly here...
## makeAnnotationHubResourceFromAHMs<-
##     function(objName,
##              ahms)
## {    
##     setMethod(newResources, objName, 
##        function(importPreparer, currentMetadata = list(), ...){
##            ## And only return ones we don't have.
##            setdiff(ahms, currentMetadata)
##        })
## }
## ## Dan wonders if maybe we can just stop using S4 for newResources?  Or maybe I can just use the generic function I was thinking about to make it easier for them to make their AHM generating function.



################################################################################
## LONG FORM of the function call (requires less knowledge by the user)

## Of course another way to think about this would be to just allow
## users to fill in a 'form' that would basically define the
## 'metadata' for them.  then the 2nd argument could be replaced with
## generic code and the user would just fill in a bunch of arguments
## along with a function for the recipe of what to do with the data.
## So FOR THE FUTURE: this might be a good way to wrap the helper that
## I am sketching out right now (with code that will allow a user to
## basically give vectorized arguments to this helper).

## can be missing list: SourceMd5, SourceSize, TaxonomyId, RDataPath,
## RDataDateAdded

## .getArgLength <- function(argName){
##     argVal <- eval(parse(text=argName))
##     length(argVal)
## }

## genericAnnotationHubMetadataMapper <- function(AnnotationHubRoot,
##                                                BiocVersion,
##                                                Coordinate_1_based,
##                                                DataProvider,
##                                                DerivedMd5,
##                                                Description,
##                                                Genome,
##                                                Maintainer,
##                                                Notes,
##                                                RDataClass,
##                                                RDataDateAdded,
##                                                RDataLastModifiedDate,
##                                                RDataPath,
##                                                RDataSize,
##                                                RDataVersion,
##                                                Recipe,
##                                                RecipeArgs,
##                                                SourceFile,
##                                                SourceLastModifiedDate,
##                                                SourceMd5,
##                                                SourceSize,
##                                                SourceUrl,
##                                                SourceVersion,
##                                                Species,
##                                                Tags,
##                                                TaxonomyId,
##                                                Title){
    
##     ## 1st we have to look at each metadata argument
##     ## each argument must be either length(arg)==1 OR length(arg)==n
##     ## where all the args that are not ==1 are the same value of n.

##     metaArgNames <- names(formals())[-1]
##     mArgLengths <- sapply(metaArgNames, .getArgLength)
##     uArgLens <- unique(mArgLengths)
##     if ((!(1 %in% uArgLens) || length(uArgLens) > 2){
##         stop("All the metadata argument lengths need to be either length 1 OR another length.  If they are another length, then they must all be the same length")
##     }
    
##     ## Then I can do something similar to below, where all the n==1
##     ## arguments get passed to MoreArgs and the others are passed in
##     ## at the front in a generic way.
    
##     ## OR just loop through the args and use rep to make them all the
##     ## correct length...    
    
##     ## ## generic Map call to replace most AHM construction operations.
##     ## Map(AnnotationHubMetadata,
##     ##     AnnotationHubRoot=meta$annotationHubRoot,
##     ##     Description=description, Genome=meta$genome,
##     ##     SourceFile=sourceFile, SourceUrl=sourceUrl,
##     ##     SourceVersion=meta$sourceVersion, Species=meta$species,
##     ##     TaxonomyId=meta$taxonomyId, Title=meta$title,
##     ##     MoreArgs=list(
##     ##       Coordinate_1_based = TRUE,
##     ##       DataProvider = "ftp.ensembl.org",
##     ##       Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
##     ##       RDataClass = "GRanges",
##     ##       RDataDateAdded = Sys.time(),
##     ##       RDataVersion = "0.0.1",
##     ##     Recipe = c("ensemblGtfToGRangesRecipe", package="AnnotationHubData"),
##     ##       Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
    
## }

## ## I need to do something like this to get my names out...
## ## eval(parse(text= names(formals())[1] ))






## makeAnnotationHubResourceFromParams <- function(objName,
##                                                 AnnotationHubRoot,
##                                                 BiocVersion,
##                                                 Coordinate_1_based,
##                                                 DataProvider,
##                                                 DerivedMd5,
##                                                 Description,
##                                                 Genome,
##                                                 Maintainer,
##                                                 Notes,
##                                                 RDataClass,
##                                                 RDataDateAdded,
##                                                 RDataLastModifiedDate,
##                                                 RDataPath,
##                                                 RDataSize,
##                                                 RDataVersion,
##                                                 Recipe,
##                                                 RecipeArgs,
##                                                 SourceFile,
##                                                 SourceLastModifiedDate,
##                                                 SourceMd5,
##                                                 SourceSize,
##                                                 SourceUrl,
##                                                 SourceVersion,
##                                                 Species,
##                                                 Tags,
##                                                 TaxonomyId,
##                                                 Title){
##     ## get metadata as AHMs
##     AHMs <- genericAnnotationHubMetadataFunction(AnnotationHubRoot,
##                                                  BiocVersion,
##                                                  Coordinate_1_based,
##                                                  DataProvider,
##                                                  DerivedMd5,
##                                                  Description,
##                                                  Genome,
##                                                  Maintainer,
##                                                  Notes,
##                                                  RDataClass,
##                                                  RDataDateAdded,
##                                                  RDataLastModifiedDate,
##                                                  RDataPath,
##                                                  RDataSize,
##                                                  RDataVersion,
##                                                  Recipe,
##                                                  RecipeArgs,
##                                                  SourceFile,
##                                                  SourceLastModifiedDate,
##                                                  SourceMd5,
##                                                  SourceSize,
##                                                  SourceUrl,
##                                                  SourceVersion,
##                                                  Species,
##                                                  Tags,
##                                                  TaxonomyId,
##                                                  Title)    
##     ## convert metadata arguments with a generic utility function
##     makeAnnotationHubResourceFromAHMs(objName,
##                               AHMs)
## }


## ## Seeing this eventual abstraction, I see that I really want my base
## ## function to take a series of AHMs (IOW not a function).  And that I
## ## really want to make that part generic for the 1st pass (not as a
## ## thin wrapper later on).


