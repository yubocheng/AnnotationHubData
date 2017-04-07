### =========================================================================
### makeStandardOrgDbsToSqlite ('standard' OrgDbs)
### -------------------------------------------------------------------------
###

## This recipe extracts the sqlite files from the 'standard'
## OrgDb packages in the current Bioconductor repo:
## 
##   http://www.bioconductor.org/packages/release/BiocViews.html#___OrgDb.

## This recipe should be run after the new OrgDbs have been generated for the
## next release. The version should be the current devel version,
## soon to roll over to the new release.

## The 'non-standard' OrgDbs are generated with makeNCBIToOrgDbs.R.

## Returns list of OrgDb objects
## NOTE: OrganismDbi:::.packageTaxIds is a static named character vector
##       of package names and taxids. This file should be checked to
##       confirm the package names match the current batch of OrgDb packages.
.getOrgDbs <- function(downloadOrgDbs=FALSE) {
    dbNames <- OrganismDbi:::.packageTaxIds()
    if (downloadOrgDbs) {  ## download, install
        lapply(dbNames, function(xx) {
            if (!requireNamespace(xx)) {
                BiocInstaller::biocLite(xx, ask=FALSE)
            }
        })
    }
    res <- mapply(get, dbNames, lapply(dbNames, asNamespace), SIMPLIFY=FALSE)
    names(res) <- dbNames
    res
}

.orgDbPkgMetadataFromObjs <- function(orgDbs, biocversion) {
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
     rDataPath <- paste0("ncbi/standard/",biocversion,"/",title)
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

makeStandardOrgDbsToAHM <- function(currentMetadata, justRunUnitTest=FALSE, 
                                    BiocVersion=biocVersion(), 
                                    downloadOrgDbs=TRUE) {
    if (length(BiocVersion) > 1L)
        stop("length(BiocVersion) must == 1L")

    orgDbs <- .getOrgDbs(downloadOrgDbs)
    meta <- .orgDbPkgMetadataFromObjs(orgDbs, biocversion=BiocVersion)
    Map(AnnotationHubMetadata,
        AnnotationHubRoot=currentMetadata$AnnotationHubRoot,
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
            Coordinate_1_based = TRUE, ## TRUE unless it "needs" to be FALSE
            DataProvider = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/ensembl",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::extractOrgDbSqlite",
            Tags = c("NCBI", "Gene", "Annotation"))) 
}

## Load the object and call saveDb()
extractOrgDbSqlite <- function(ahm) {
    dbFile <- metadata(ahm)$Title
    orgDbName <- sub('.sqlite','',dbFile)
    orgDbs <- .getOrgDbs()
    orgDb <- orgDbs[[orgDbName]]
    outputPath <- file.path(metadata(ahm)$AnnotationHubRoot,
                            basename(metadata(ahm)$RDataPath))
    if (!isSingleString(outputPath)) 
        stop("'outputPath' must be a single string")
    sqliteCopyDatabase(dbconn(orgDb), outputPath)
    outputFile(ahm)
}

makeAnnotationHubResource("OrgDbFromPkgsImportPreparer", makeStandardOrgDbsToAHM)
