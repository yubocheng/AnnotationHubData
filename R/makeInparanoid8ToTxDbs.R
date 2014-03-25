## This is an example of how this new helper method can make things
## simpler and also provides a test case for how we can parse ensembl
## GTF files into GRanges objects.


## helper to make metadata list from the data
.inparanoidMetadataFromUrl <- function(baseUrl) {
    ## get all the subDirs
    subDirs <- AnnotationForge:::.getSubDirs(baseUrl)
    subDirs <- subDirs[!(subDirs %in% c('stderr/'))]
    species <- sub("/","",subDirs)
    allDirs <- file.path(baseUrl, species)
    ## We have the tax ID and the full species names in AnnotationForge already
    meta <- read.delim(system.file('extdata','inp8_Full_species_mapping',
                                   package='AnnotationForge'),
                       sep="\t", header=TRUE, stringsAsFactors=FALSE)
    matches <- match(species, meta$inparanoidSpecies)
    fullSpecies <- meta$GenusAndSpecies[matches]
    taxonomyId <- meta$taxID[matches]
    title <- paste0('Inparanoid8_',species,'.sqlite')
    root <- setNames(rep(NA_character_, length(allDirs)), title)
    genome <- setNames(rep(NA_character_, length(allDirs)), title)
    sourceVersion <- rep('Inparanoid version 8',length(allDirs))
    description <- paste("Inparanoid 8 annotations about", fullSpecies)
    sourceUrl <- paste0(baseUrl,"/", species)
    ## get the name for the DB
    dbname <- paste0("hom.",
                     sub(" ","_",fullSpecies),
                     ".inp8",
                     ".sqlite")    
    sourceFile <- paste0("inparanoid8/Orthologs/",dbname)
    ## return as a list
    list(annotationHubRoot = root, title=title, species = fullSpecies,
         taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
         sourceFile = sourceFile, sourceVersion = sourceVersion,
         description=description)
}


## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeinparanoid8ToAHMs <- function(){
    baseUrl <- 'http://inparanoid.sbc.su.se/download/current/Orthologs'
    ## Then make the metadata for these
    meta <- .inparanoidMetadataFromUrl(baseUrl)
    ## then make AnnotationHubMetadata objects.
    Map(AnnotationHubMetadata,
        AnnotationHubRoot=meta$annotationHubRoot,
        Description=meta$description,
        Genome=meta$genome,
        SourceFile=meta$sourceFile, 
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        MoreArgs=list(
          Coordinate_1_based = NA_character_,
          DataProvider = baseUrl,
          Maintainer = "Marc Carlson <mcarlson@fhcrc.org>",
          RDataClass = "SQLite",
          RDataDateAdded = Sys.time(),
          RDataVersion = "0.0.1",
          Recipe = c("inparanoid8ToTxDbsRecipe", package="AnnotationHubData"),
          Tags = c("Inparanoid", "Gene", "Homology", "Annotation")))
}



## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
## REMEMBER: inputFiles will be file.path(AnnotationHubRoot,SourceFile)
## (from the AHM)
## and outputFile will be file.path(AnnotationHubRoot,RDataPath)
inparanoid8ToTxDbsRecipe <- function(recipe){
    require(AnnotationForge)
    db <- makeInpDb(dir=inputFiles(recipe)[1], dataDir=tempdir())
    save(db, file=outputFile(recipe))
    outputFile(recipe)
}



## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("Inparanoid8ImportPreparer",
                          makeinparanoid8ToAHMs)

























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
