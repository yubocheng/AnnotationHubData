## This recipe is no longer used. If reinstated, add this unit test
## back to test_recipes.R

#test_Inparanoid8ImportPreparer_recipe <- function() {
#    suppressWarnings({
#    ahms = updateResources(ahroot, BiocVersion,
#                           preparerClasses = "Inparanoid8ImportPreparer",
#                           insert = FALSE, metadataOnly=TRUE,
#                           justRunUnitTest=TRUE)
#    })
#    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
#}

## helper to make metadata list from the data
.inparanoidMetadataFromUrl <- function(baseUrl, justRunUnitTest) {
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
    taxonomyId <- as.integer(as.character(meta$taxID[matches]))
    ## get the name for the DB
    title <- paste0("hom.",
                     gsub(" ","_",fullSpecies),
                     ".inp8",
                     ".sqlite")    
    ## root <- setNames(rep(NA_character_, length(allDirs)), title)
    genome <- setNames(rep("inparanoid8 genomes", length(allDirs)), title)
    sourceVersion <- rep('Inparanoid version 8',length(allDirs))
    description <- paste("Inparanoid 8 annotations about", fullSpecies)
    sourceUrl <- paste0(baseUrl,"/", species)
    
    rDataPath <- paste0("inparanoid8/Orthologs/",title)
        
    df <- data.frame(title=title, species = fullSpecies,
         taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
         sourceVersion = sourceVersion,
         description=description, rDataPath=rDataPath, stringsAsFactors=FALSE)
    rownames(df) <- NULL
    
    if(justRunUnitTest)
        df <- df[1:2, ]    
    df
}


## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeinparanoid8ToAHMs <- function(currentMetadata, justRunUnitTest, BiocVersion) {
    baseUrl <- 'http://inparanoid.sbc.su.se/download/current/Orthologs_other_formats'
    ## Then make the metadata for these
    meta <- .inparanoidMetadataFromUrl(baseUrl, justRunUnitTest)
    ## then make AnnotationHubMetadata objects.
    Map(AnnotationHubMetadata,
        ## AnnotationHubRoot=meta$annotationHubRoot,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        RDataPath=meta$rDataPath,
        MoreArgs=list(
          BiocVersion=BiocVersion,
	  SourceType="Inparanoid",
          Coordinate_1_based = TRUE, ## TRUE unless it "needs" to be FALSE
          DataProvider = "Inparanoid8",
          Maintainer = "Marc Carlson <mcarlson@fhcrc.org>",
          RDataClass = "Inparanoid8Db",
	  DispatchClass="SQLiteFile",	
          RDataDateAdded = Sys.time(),
          Recipe = "AnnotationHubData:::inparanoid8ToDbsRecipe",
          Tags = c("Inparanoid", "Gene", "Homology", "Annotation")))
}



## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
inparanoid8ToDbsRecipe <- function(ahm){
    
    inputFiles <- metadata(ahm)$SourceFile 
    dbname <- makeInpDb(dir=file.path(inputFiles,""),
                        dataDir=tempdir())
    db <- loadDb(file=dbname)
    outputPath <- file.path(metadata(ahm)$AnnotationHubRoot,
                            metadata(ahm)$RDataPath)
    saveDb(db, file=outputPath) 
    outputFile(ahm)
}




## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("Inparanoid8ImportPreparer",
                          makeinparanoid8ToAHMs)

















