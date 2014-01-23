## This is an example of how this new helper method can make things
## simpler and also provides a test case for how we can parse ensembl
## GTF files into GRanges objects.


## STEP 1: come up with a name for the new ImportPreparer class
## "EnsemblGtfImportPreparer"



## STEP 2: make function to process metadata into AHMs

## This function will return the AHMs and takes no args...
makeEnsemblAHMsFromGTFs <- function(){
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
          Recipe = c("ensemblGtfToGRangesRecipe", package="AnnotationHubData"),
          Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
}



## STEP 3:  Make a recipe that takes an AnnotationHubRecipe object.
## Yes I am keeping things so that users have to learn about
## AnnotationHubRecipe objects BECAUSE I plan to remove from users the
## need to know about the more complicated AMH objects.
ensemblGTFToGRangesRecipe <- function(recipe){
    require(rtracklayer)
    gz.inputFile <- inputFiles(recipe)[1]
    con <- gzfile(gz.inputFile)
    on.exit(close(con))
    gr <- import(con, "gtf", asRangedData=FALSE)
    save(gr, file=outputFile(recipe))
    outputFile(recipe)
}



## STEP 4:  Call the helper to set up the newResources() method
## makeAnnotationHubResource("EnsemblGtfImportPreparer",
##                           makeEnsemblAHMsFromGTFs)







##############################################################################
## NEXT: make a long-form call and skip the AHM function helper
## function (step 2) entirely.
## RESULT: a THREE step process that eliminates the need for users to
## know about AHMs

## makeAnnotationHubResourceFromParams("EnsemblGtfImportPreparer",
##                                     ensemblGTFToGRangesRecipe,
##                                     meta1,
##                                     meta2,
##                                     meta3)










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

## So maybe I will just explain it...  NOT an easy decision for me.
