.NCBIMetadataFromUrl <- function(baseUrl, justRunUnitTest, biocVersion) {
    load(system.file('extdata','viableIDs.rda', package='AnnotationForge'))
    ids <- results

    if (justRunUnitTest)
    ids <- head(ids)
    ## FIXME: need different solution; this subset produces NAs
    if (length(biocVersion) > 1) {
        stop(paste("'biocVersion' must be a single value. Make sure new",
                   "'OrgDbs' go into the CORRECT Bioconductor version!"))
    }
    ## need to find an alternative to this...
    ## old school table of tax Ids
    if (!exists("specData")) {
     data(specData, package = "GenomeInfoDb")
    }
    sd <- specData[!is.na(specData[[3]]),]
    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::.lookupSpeciesFromTaxId(id)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- ids[ids %in% sd$tax_id]
    ## AND remove this overly general barley ID
    ## ids <- ids[!(ids %in% '4513')]

    res <- lapply(ids,lookup)
    taxonomyId <- as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)

    oriSpecies <- paste(genus, species)
    fullSpecies <- gsub(" ", "_", oriSpecies)

    title <- paste0("org.", fullSpecies, ".eg", ".sqlite")
    rDataPath <- paste0("ncbi/uniprot/",biocVersion,"/",title)

    genome <- setNames(rep("NCBI genomes", length(fullSpecies)), title)
    dateMessage <- paste0('NCBI gene annotations as of ', as.character(date()))
    sourceVersion <- rep(dateMessage, length(fullSpecies))
    description <- paste("NCBI gene ID based annotations about", oriSpecies)
    sourceUrls <- c(baseUrl,"ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/idmapping_selected.tab.gz")
    sourceUrl <- rep(list(sourceUrls), length(fullSpecies))

    list(title=title, species = oriSpecies,
        taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
        sourceVersion = sourceVersion,
        description=description, rDataPath=rDataPath)
}

## STEP 1: make function to process metadata into AHMs
makeNCBIToOrgDbsToAHM <-
    function(currentMetadata,
             baseUrl = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",
             justRunUnitTest = FALSE,
             BiocVersion) {
    meta <- .NCBIMetadataFromUrl(baseUrl, justRunUnitTest,
                                 biocVersion=BiocVersion[[1]])
    Map(AnnotationHubMetadata,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        RDataPath=meta$rDataPath,
        MoreArgs=list(
            BiocVersion=package_version(BiocVersion),
            Coordinate_1_based = TRUE,
            DataProvider = baseUrl,
            Maintainer = paste("Bioconductor Package Maintainer",
                               "<maintainer@bioconductor.org>"),
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/UniProt",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::NCBIToOrgDbs",
            Tags = c("NCBI", "Gene", "Annotation")))
}

## STEP 2: Make a recipe function that takes an AnnotationHubRecipe object.
NCBIToOrgDbs <- function(ahm){
    fullSpecies <- ahm@Species
    genus <- unlist(strsplit(fullSpecies,split=" "))[1]
    species <- unlist(strsplit(fullSpecies,split=" "))[2]
    dbname <- makeOrgPackageFromNCBI(version="1.0.0",
                                     maintainer=ahm@Maintainer,
                                     author=ahm@Maintainer,
                                     outputDir=getwd(),
                                     tax_id=as.character(ahm@TaxonomyId),
                                     genus=genus,
                                     species=species,
                                     NCBIFilesDir=getwd(),
                                     databaseOnly=TRUE,
                                     rebuildCache=TRUE)
    db <- loadDb(file=dbname)
    saveDb(db, file=outputFile(ahm))
    outputFile(ahm)
}

## STEP 3: Call the helper to set up the newResources() method
makeAnnotationHubResource("NCBIImportPreparer",
                          makeNCBIToOrgDbsToAHM)
