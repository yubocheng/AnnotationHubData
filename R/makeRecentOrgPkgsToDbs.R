## The purpose of this recipe is to capture those OrgDb objects that
## are normally only available as packages and make them into OrgDb
## objects in the AnnotationHub.  The result should be that the list
## of OrgDb objects in the Hub will be 'complete' meaning it will have
## organisms for as many organisms as I can manage.



## helper function to install, load and extract OrgDb objects from the
## 18 different packages that we support.  It returns a list of OrgDbs.
## You can call this function with install=TRUE if you don't have the
## prerequisites installed yet.
.GetOrgDbs <- function(install=FALSE){
  dbNames <- OrganismDbi:::.packageTaxIds()
  if(install==TRUE){
      lapply(dbNames, biocLite)
  }
  lapply(dbNames, require, character.only=TRUE)
  res <- lapply(dbNames, get)
  names(res) <- dbNames
  res
}


## helper to make metadata list from the data
.orgDbPkgMetadataFromObjs <- function(orgDbs){
    ## title
    title <- paste0(names(orgDbs), '.sqlite')
    ## organism
    species <- unlist(lapply(orgDbs,
                function(x){m <- metadata(x); m[m$name=='ORGANISM', 2] }))
    ## tax ID
    taxonomyId <- as.integer(unlist(lapply(orgDbs,
                function(x){m <- metadata(x); m[m$name=='TAXID', 2] })))
    ## genome
    genome <- rep("NCBI genomes", length(title))
    ## sourceUrl
    urls <- c("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",
                       "ftp://ftp.ensembl.org/pub/current_fasta")
    sourceUrl <- rep(list(urls), length(title))
    ## sourceVersion
    dateMessage <- paste0('NCBI gene annotations as of ', as.character(date()))
    sourceVersion <- rep(dateMessage, length(title))
    ## description 
    description <- paste("NCBI gene ID based annotations about", species)
    ## rDataPath
    rDataPath <- paste0("ncbi/",title)
    ## return as a list
    list(##annotationHubRoot = root,
         title=title,
         species = species,
         taxonomyId = taxonomyId,
         genome = genome,
         sourceUrl=sourceUrl,
         sourceVersion = sourceVersion,
         description=description,
         rDataPath=rDataPath)
}



## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeOrgDbPkgsToAHMs <- function(currentMetadata, justRunUnitTest) {

    ## 1st get the OrgDbObjects as I want to get most of the metadata from those
    orgDbs <- .GetOrgDbs()

    ## Then make the metadata for these
    meta <- .orgDbPkgMetadataFromObjs(orgDbs)

    ## then make AnnotationHubMetadata objects.
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
            Coordinate_1_based = TRUE, ## TRUE unless it "needs" to be FALSE
            DataProvider = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",
            Maintainer = "Marc Carlson <mcarlson@fhcrc.org>",
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/ensembl",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::PackagedOrgDbsRecipe",
            Tags = c("NCBI", "Gene", "Annotation")))    
}



## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
## In this case, I just want the Recipe to load the object and call saveDb()
PackagedOrgDbsRecipe <- function(ahm){
    dbFile <- metadata(ahm)$Title
    orgDbName <- sub('.sqlite','',dbFile)
    orgDbs <- .GetOrgDbs()
    orgDb <- orgDbs[[orgDbName]]
    outputPath <- file.path(metadata(ahm)$AnnotationHubRoot,
                            metadata(ahm)$RDataPath)
    saveDb(orgDb, file=outputPath)
    outputFile(ahm)
}




## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("OrgDbFromPkgsImportPreparer",
                          makeOrgDbPkgsToAHMs)
