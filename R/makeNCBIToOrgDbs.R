## This is an example of how this new helper method can make things
## simpler and also provides a test case for how we can parse ensembl
## GTF files into GRanges objects.


## helper to make metadata list from the data
.NCBIMetadataFromUrl <- function(baseUrl, justRunUnitTest) {    
    ## These are the IDs (prescreened) for us to process
    load(system.file('extdata','viableIDs.rda', package='AnnotationForge'))
    ids <- names(results)[results]

    if(justRunUnitTest)
	ids <- head(ids)
    
    ## old school table of tax Ids
    load(system.file('extdata','taxNames.rda', package='AnnotationForge'))
    sd <- specData[!is.na(specData[[3]]),]

    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        AnnotationForge:::.lookupSpeciesFromTaxId(id)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- ids[ids %in% sd$tax_id]
    ## AND remove this one bad one that we discovered (an overly
    ## general barley ID)
    ids <- ids[!(ids %in% '4513')]

## TEMP HACK to avoid a 20 minute wait
##ids <- ids[1:2]
    ## This step takes a minute
    res <- lapply(ids,lookup)
    
    ## get the tax_ids like so (etc.)
    taxonomyId <- as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))  
    ## cleanup of complex names
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)
    ## then we need the full original genus and species etc.
    oriSpecies <- paste(genus, species, sep=" ") 
    fullSpecies <- gsub(" ", "_", oriSpecies)
    ## fullSpecies <- gsub("/", "_", fullSpecies)

    ## get the name for the DB
    title <- paste0("org.",
                    fullSpecies,
                    ".eg",
                    ".sqlite")    
    ## root <- setNames(rep(NA_character_, length(allDirs)), title)
    genome <- setNames(rep("NCBI genomes", length(fullSpecies)), title)
    dateMessage <- paste0('NCBI gene annotations as of ', as.character(date()))
    sourceVersion <- rep(dateMessage, length(fullSpecies))
    description <- paste("NCBI gene ID based annotations about", fullSpecies)
#     sourceUrl <- rep(baseUrl,length(fullSpecies))
    sourceUrls <- c(baseUrl,"http://www.blast2go.de/")
    sourceUrl <- rep(list(sourceUrls), length(fullSpecies))
    rDataPath <- paste0("ncbi/",title)
    ## return as a list
    list(##annotationHubRoot = root,
        title=title, species = oriSpecies,
        taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
        sourceVersion = sourceVersion,
        description=description, rDataPath=rDataPath)
}


## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeNCBIToAHMs <- function(currentMetadata, justRunUnitTest=FALSE){
    baseUrl <- 'ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/'
    ## Then make the metadata for these
    meta <- .NCBIMetadataFromUrl(baseUrl, justRunUnitTest)
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
            Coordinate_1_based = TRUE, ## TRUE unless it "needs" to be FALSE
            DataProvider = baseUrl,
            Maintainer = "Marc Carlson <mcarlson@fhcrc.org>",
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/blast2GO",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHUbData:::NCBIToOrgDbsRecipe",
            Tags = c("NCBI", "Gene", "Annotation")))
}


## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
## REMEMBER: inputFiles will be file.path(AnnotationHubRoot,SourceFile)
## (from the AHM)
## and outputFile will be file.path(AnnotationHubRoot,RDataPath)
NCBIToOrgDbsRecipe <- function(ahm){
    require(AnnotationForge)
    ## make use of file.path to put on a trailing slash of the appropriate kind
    ## dbname <- makeInpDb(dir=file.path(inputFiles(ahm, useRoot=FALSE),""),
    ##                     dataDir=tempdir())
    fullSpecies <- ahm@Species
    genus <- unlist(strsplit(fullSpecies,split=" "))[1]
    species <- unlist(strsplit(fullSpecies,split=" "))[2]
    dbname <- makeOrgPackageFromNCBI(tax_id=ahm@TaxonomyId,
                                     genus=genus,
                                     species=species,
                                     author=ahm@Maintainer,
                                     maintainer=ahm@Maintainer,
                                     databaseOnly=TRUE,
                                     outputDir=getwd(),
                                     NCBIFilesDir=getwd())
    db <- loadDb(file=dbname)
    saveDb(db, file=outputFile(ahm))
    outputFile(ahm)
}




## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("NCBIImportPreparer",
                          makeNCBIToAHMs)

















