### =======================================================================
### makeStandardTxDbsToSqlite
### -------------------------------------------------------------------------
###

## Extracts the sqlite files from the 'standard' TxDb packages in the current 
## Bioconductor repo:
## 
##   http://www.bioconductor.org/packages/release/BiocViews.html#___TxDb.

## NOTES:
## - Recipe should be run after new TxDbs have been generated (right before
##   the next release).
## - BiocVersion() should be the impending release / current devel.
## - May need to run AnnotationHubData:::.getTxDbs(TRUE) to load all
##   TxDbs if not in local R install.

## Returns list of loaded TxDb objects.
.getTxDbs <- function(TxDbs) {
    require(BiocInstaller)
    lapply(TxDbs, function(xx) {
        if (!require(xx, character.only=TRUE))
            biocLite(xx, ask=FALSE)
    })
    lapply(TxDbs, require, character.only=TRUE)
    res <- lapply(TxDbs, get)
    names(res) <- TxDbs 
    res
}

.TxDbPkgMetadataFromObjs <- function(txdbs, biocversion) {
    title <- paste0(names(txdbs), '.sqlite')
    species <- unlist(lapply(txdbs,
        function(x){m <- metadata(x); m[m$name=='Organism', 2] }))
    taxonomyId <- as.integer(unlist(lapply(txdbs, 
        function(x) {
            m <- metadata(x) 
            id <- m[m$name=='TaxID', 2]
            if (!length(id))
                id <- m[m$name=='Taxonomy ID', 2]
            id
        })))

    sourceVersion <- sapply(txdbs, 
        function(x) {
            m <- metadata(x) 
            paste0('UCSC transcript based annotations generated ', 
                   strptime(m[m$name=='Creation time', 2], "%Y-%m-%d")) 
        }, simplify=FALSE)
    url <- list(c("http://genome.ucsc.edu/", 
                  "http://hgdownload.cse.ucsc.edu/goldenPath"))
    list(title=title,
         species=species,
         taxonomyId=taxonomyId,
         genome=rep("UCSC genomes", length(title)),
         sourceUrl=rep(url, length(title)),
         sourceVersion=sourceVersion,
         description=paste("UCSC transcript based annotations for", species),
         rDataPath=paste0("ucsc/standard/", biocversion, "/",title))
}

makeStandardTxDbsToAHM <- function(currentMetadata, justRunUnitTest = FALSE, 
                                   BiocVersion = biocVersion(),
                                   TxDbs) {
    if (length(BiocVersion) > 1L)
        stop("length(BiocVersion) must == 1L")

    txdbs <- .getTxDbs(TxDbs)
    meta <- .TxDbPkgMetadataFromObjs(txdbs, biocversion=BiocVersion)
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
            Coordinate_1_based=TRUE, ## TRUE unless it "needs" to be FALSE
            DataProvider="UCSC",
            Maintainer="Bioconductor Maintainer <maintainer@bioconductor.org>",
            RDataClass="TxDb",
            DispatchClass="SQLiteFile",
            SourceType="FASTA",
            RDataDateAdded = Sys.time(),
            Recipe="AnnotationHubData:::extractTxDbSqlite",
            Tags=c("UCSC", "Transcript", "Annotation"))) 
}

## Load the object and call saveDb()
extractTxDbSqlite <- function(ahm) {
    dbFile <- metadata(ahm)$Title
    txdb <- sub('.sqlite','',dbFile)
    outputPath <- file.path(metadata(ahm)$AnnotationHubRoot,
                            basename(metadata(ahm)$RDataPath))
    if (!isSingleString(outputPath)) 
        stop("'outputPath' must be a single string")
    sqliteCopyDatabase(dbconn(.getTxDbs(txdb)[[1]]), outputPath)
    outputFile(ahm)
}

makeAnnotationHubResource("TxDbFromPkgsImportPreparer", makeStandardTxDbsToAHM)
