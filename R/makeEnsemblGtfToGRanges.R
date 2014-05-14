## This is an example of how this new helper method can make things
## simpler and also provides a test case for how we can parse ensembl
## GTF files into GRanges objects.


## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeEnsemblGTFsToAHMs <- function(){
    baseUrl <- .ensemblBaseUrl
    sourceUrl <- .ensemblGtfSourceUrls(.ensemblBaseUrl)
    
    sourceFile <- .ensemblSourcePathFromUrl(baseUrl, sourceUrl)
    meta <- .ensemblMetadataFromUrl(sourceUrl)
    rdata <- sub(".gz$", ".RData", sourceFile)
    description <- paste("Gene Annotation for", meta$species)

    Map(AnnotationHubMetadata,
        AnnotationHubRoot=meta$annotationHubRoot,
        Description=description, Genome=meta$genome,
        SourceFile=sourceFile, SourceUrl=sourceUrl,
        SourceVersion=meta$sourceVersion, Species=meta$species,
        TaxonomyId=meta$taxonomyId, Title=meta$title,
        MoreArgs=list(
          Coordinate_1_based = TRUE,
          DataProvider = "ftp.ensembl.org",
          Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
          RDataClass = "GRanges",
          RDataDateAdded = Sys.time(),
          RDataVersion = "0.0.1",
          Recipe = c("ensemblGTFToGRangesRecipe", package="AnnotationHubData"),
          Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
}



## STEP 2: Make a recipe function that takes an AnnotationHubMetadata
## object.
ensemblGTFToGRangesRecipe <- function(ahm){
    require(rtracklayer)
    gz.inputFile <- inputFiles(ahm)[1]
    con <- gzfile(gz.inputFile)
    on.exit(close(con))
    gr <- import(con, "gtf", asRangedData=FALSE)
    save(gr, file=outputFile(ahm))
    outputFile(ahm)
}



## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("EnsemblGtfImportPreparer",
                          makeEnsemblGTFsToAHMs)

























#######################################################################
## Next:
## 1) Beef up the constructor for AnnotationHub objects
## 2) Create a vignette explaining this stuff.
#######################################################################




#######################################################################
## test that this worked is whether the method is available after
## loading package (it is)
## library(AnnotationHubData);getMethod(f='newResources' ,signature='EnsemblGtfImportPreparer'); getClass('EnsemblGtfImportPreparer')
## BUT it may not 'actually' work since the class is not exported from
## the NAMESPACE in this case...
## more tests
## foo <- new('EnsemblGtfImportPreparer')
## BiocVersion <- c("2.12", "2.13", "2.14")
## cm <- AnnotationHubServer:::getExistingResources(BiocVersion)
## res <- newResources(foo, currentMetadata=cm)
## This works!

##############################################################################
## NEXT: make a long-form call and skip the AHM function helper
## function (step 2) entirely.
## RESULT: a THREE step process that eliminates the need for users to
## know about AHMs

## makeAnnotationHubResourceFromParams("EnsemblGtfImportPreparer",
##                                     AnnotationHubRoot,
##                                     BiocVersion,
##                                     Coordinate_1_based,
##                                     DataProvider,
##                                     DerivedMd5,
##                                     Description,
##                                     Genome,
##                                     Maintainer,
##                                     Notes,
##                                     RDataClass,
##                                     RDataDateAdded,
##                                     RDataLastModifiedDate,
##                                     RDataPath,
##                                     RDataSize,
##                                     RDataVersion,
##                                   Recipe,
##                                     RecipeArgs,
##                                     SourceFile,
##                                     SourceLastModifiedDate,
##                                     SourceMd5,
##                                     SourceSize,
##                                     SourceUrl,
##                                     SourceVersion,
##                                     Species,
##                                     Tags,
##                                     TaxonomyId,
##                                     Title)


## TODO:
## put metadata args above into a more sensible order
## look at match.call() and formals() for ways to get the arguments







##############################################################################
## 'NEXT' NEXT: Factor out the need for the user to know about
## AnnotationHubRecipe objects by making a wrapper for creating
## recipes? OR just explain what an AnnotationHubRecipe is...

## Right now I am leaning towards factoring it out.  Because I would
## rather just describe 4 arguments than explain this new class to
## users...  And this class contains no info that is not already
## derived from an AHM (along with an AHM).

## BUT LURKING in the code the recipes are all expecting an AHMR, so I
## will have to change all that to get rid of it?  Nope.  I can leave
## it and just not require it here.  But I will have to enable AHMs
## that can point to recipes that require 4 args instead of this
## object...

## OR if I explain it, users have to understand that there is this
## object that has methods they can call to get inputFile, recipeName
## and outputFile

## So maybe I will just explain it...  This is NOT an easy decision for me.
