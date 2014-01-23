## EnsemblGtfImportPreparer <-
##     setClass("EnsemblGtfImportPreparer", contains="ImportPreparer")

## retrieve GTF file urls from Ensembl
.ensemblGtfSourceUrls <-
    function(baseUrl)
{
    want <- .ensemblDirUrl(baseUrl, "gtf/")
    ## files in release
    unlist(lapply(want, function(url) {
        listing <- getURL(url=url, followlocation=TRUE, customrequest="LIST -R")
        listing<- strsplit(listing, "\n")[[1]]
        subdir <- sub(".* ", "", listing[grep("^drwx", listing)])
        gtfGz <- sub(".* ", "", listing[grep(".*gtf.gz$", listing)])
        sprintf("%s%s/%s", url, subdir, gtfGz)
    }), use.names=FALSE)
}

.ensemblGtfMetadata <-
    function(baseUrl, sourceUrl)
{
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
          Recipe = c("ensemblGtfToGRanges", package="AnnotationHubData"),
          Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
}

## setMethod(newResources, "EnsemblGtfImportPreparer",
##     function(importPreparer, currentMetadata = list(), ...)
## {
##     sourceUrls <- .ensemblGtfSourceUrls(.ensemblBaseUrl) # 122,  6 March, 2013

##     ## filter known
##     knownUrls <- sapply(currentMetadata, function(elt) {
##         metadata(elt)$SourceUrl
##     })
##     sourceUrls <- sourceUrls[!sourceUrls %in% knownUrls]

##     ## AnnotationHubMetadata
##     .ensemblGtfMetadata(.ensemblBaseUrl, sourceUrls)
## })
