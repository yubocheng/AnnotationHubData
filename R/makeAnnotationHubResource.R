## Goals:

## makeAnnotationHubResource is to help people who are external to
## AnnotationHubData to be able to define recipes that can then be
## used by the package. The function takes three arguments: the 1st is
## an importPreparer object (basically a name that they will have to
## define as a class).  The 2nd is a function that describes how to
## make an AnnotationHubMetadata object from external files (on web).
## And the 3rd is a function that describes the recipe, or how you
## take that metadata and convert it into an R object that should be
## saved in the AnnotationHub.



## makeAnnotationHubResource will move the multiple resources ->
## multiple AHMs part down a level so that the user only has to define
## how to do "one" resource (by each function), and in a vectorized way???

## Unless I use a ..., these two function arguments need to have the
## property that they can't require any arguments.

## And they must each return a specific thing (AHM in the case of
## argument #2 - which will be checked for in that case).

## In this design, we still keep the preparer code separated from the
## code for the recipe (so that the two can be re-used if
## appropriate).

##############################################################################
## Return value (or what it does after making an/many AHMs):
## makeAnnotationHubResource will have to define a newResources method
## for each AHM type.  It can either return that object OR
## alternatively, it could use a template to spit their code into a
## separarate module/package.  OR, I guess the meat of this
## information could be stored in a DB and that DB could be processed
## to extract data again later.  For now I am planning on just
## defining a method in the helper function.

## So it seems that we CAN define a method inside our helper and still
## have it be usable elsewhere...
## Some tests
## library(AnnotationHubData)
## fred <- setClass("fred", contains="ImportPreparer")
## testMethDef <-function(name){
##     setMethod(newResources, name, 
##               function(importPreparer, currentMetadata = list(), ...){
##               })
## }

## testMethDef("fred")
## getMethods(f="newResources")

## BUT to take advantage of that I will have to basically define a
## single newResourcesMethod that can work for all of them.


###############################################################################
## AND: for a test case, lets re-implement the ensembl GTF preparer
## using this new interface.




makeAnnotationHubResource <-
    function(objName,
             makeAnnotationHubMetadataFunction)
{    
    ## Create a newResources Method for the object type passed in.
    ## The job of this method is to only get resources that are "new"
    ## It takes an arg of "old" AHMs that can be used for filtering.    
    ## So it will call the makeAnnotationHubMetadataFunction, and then
    ## toss out any currentMetadata() AHMs that are already present.
    setMethod(newResources, objName, 
       function(importPreparer, currentMetadata = list(), ...){
           ## The 1st function can take no args and must return AHMs
           ahms <- makeAnnotationHubMetadataFunction()
           ## And only return ones we don't have.
           setdiff(ahms, currentMetadata)
       })
}




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

genericAnnotationHubMetadataMapper <- function(AnnotationHubRoot,
                                               BiocVersion,
                                               Coordinate_1_based,
                                               DataProvider,
                                               DerivedMd5,
                                               Description,
                                               Genome,
                                               Maintainer,
                                               Notes,
                                               RDataClass,
                                               RDataDateAdded,
                                               RDataLastModifiedDate,
                                               RDataPath,
                                               RDataSize,
                                               RDataVersion,
                                               Recipe,
                                               RecipeArgs,
                                               SourceFile,
                                               SourceLastModifiedDate,
                                               SourceMd5,
                                               SourceSize,
                                               SourceUrl,
                                               SourceVersion,
                                               Species,
                                               Tags,
                                               TaxonomyId,
                                               Title){
    
    ## 1st we have to look at each metadata argument
    ## each argument must be either length(arg)==1 OR length(arg)==n
    ## where all the args that are not ==1 are the same value of n.
    
    
    ## Then I can do something similar to below, where all the n==1
    ## arguments get passed to MoreArgs and the others are passed in
    ## at the front in a generic way.
    
    ## OR just loop through the args and use rep to make them all the
    ## correct length...    
    
    ## ## generic Map call to replace most AHM construction operations.
    ## Map(AnnotationHubMetadata,
    ##     AnnotationHubRoot=meta$annotationHubRoot,
    ##     Description=description, Genome=meta$genome,
    ##     SourceFile=sourceFile, SourceUrl=sourceUrl,
    ##     SourceVersion=meta$sourceVersion, Species=meta$species,
    ##     TaxonomyId=meta$taxonomyId, Title=meta$title,
    ##     MoreArgs=list(
    ##       Coordinate_1_based = TRUE,
    ##       DataProvider = "ftp.ensembl.org",
    ##       Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
    ##       RDataClass = "GRanges",
    ##       RDataDateAdded = Sys.time(),
    ##       RDataVersion = "0.0.1",
    ##     Recipe = c("ensemblGtfToGRangesRecipe", package="AnnotationHubData"),
    ##       Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
    
}








makeAnnotationHubResourceFromParams <- function(objName,
                                                AnnotationHubRoot,
                                                BiocVersion,
                                                Coordinate_1_based,
                                                DataProvider,
                                                DerivedMd5,
                                                Description,
                                                Genome,
                                                Maintainer,
                                                Notes,
                                                RDataClass,
                                                RDataDateAdded,
                                                RDataLastModifiedDate,
                                                RDataPath,
                                                RDataSize,
                                                RDataVersion,
                                                Recipe,
                                                RecipeArgs,
                                                SourceFile,
                                                SourceLastModifiedDate,
                                                SourceMd5,
                                                SourceSize,
                                                SourceUrl,
                                                SourceVersion,
                                                Species,
                                                Tags,
                                                TaxonomyId,
                                                Title){
    ## get metadata as AHMs
    AHMs <- genericAnnotationHubMetadataFunction(AnnotationHubRoot,
                                                 BiocVersion,
                                                 Coordinate_1_based,
                                                 DataProvider,
                                                 DerivedMd5,
                                                 Description,
                                                 Genome,
                                                 Maintainer,
                                                 Notes,
                                                 RDataClass,
                                                 RDataDateAdded,
                                                 RDataLastModifiedDate,
                                                 RDataPath,
                                                 RDataSize,
                                                 RDataVersion,
                                                 Recipe,
                                                 RecipeArgs,
                                                 SourceFile,
                                                 SourceLastModifiedDate,
                                                 SourceMd5,
                                                 SourceSize,
                                                 SourceUrl,
                                                 SourceVersion,
                                                 Species,
                                                 Tags,
                                                 TaxonomyId,
                                                 Title)    
    ## convert metadata arguments with a generic utility function
    makeAnnotationHubResource(objName,
                              AHMs)
}


## Seeing this eventual abstraction, I see that I really want my base
## function to take a series of AHMs (IOW not a function).  And that I
## really want to make that part generic for the 1st pass (not as a
## thin wrapper later on).


