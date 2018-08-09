## As of July 2016 this recipe was modified to store only metadata
## and no files in S3. AnnotationHub will expose available GTF files
## from Ensembl and the AnnotationHub::GTFFile dispatch class will
## convert the GTF to GRanges on the fly.

.ensemblGtfSourceUrls <-
    function(baseDir, baseUrl, release, justRunUnitTest, verbose=FALSE)
{
    want <- paste0(baseUrl, "release-", release, paste0("/", baseDir))
    urls <- unlist(lapply(want, function(url) {
        listing <- .ftpDirectoryInfo(url)
        subdir <- sub(".* ", "", listing[grep("^drwx", listing)])
        paste0(url, subdir, "/")
    }), use.names=FALSE)

    if(justRunUnitTest)
        urls <- urls[1:2] ## 2 organisms; possibly more files

    df <- .ftpFileInfo(urls, ".gtf.gz", verbose=verbose)
    rownames(df) <- NULL
    df
}

makeEnsemblGtfToAHM <-
    function(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
             baseDir = "gtf/", release, justRunUnitTest = FALSE, 
             BiocVersion = BiocManager::version(), ...)
{
    ## get all file urls, size, date
    df <- .ensemblGtfSourceUrls(baseDir, baseUrl, release, 
                                justRunUnitTest, ...)

    sourceUrls <- df$fileurl
    rdatapath <- gsub(baseUrl, "", sourceUrls)

    ## get genome, species, version, title
    meta <- .ensemblMetadataFromUrl(sourceUrls)
    description <- paste("Gene Annotation for", meta$species)

    Map(AnnotationHubMetadata,
        Description=description, Genome=meta$genome,
        SourceUrl=sourceUrls,
        SourceSize=as.numeric(df$size),
        SourceLastModifiedDate=df$date,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        RDataPath=rdatapath,
        TaxonomyId=meta$taxonomyId, Title=meta$title,
        MoreArgs=list(
            BiocVersion=BiocVersion,
            Coordinate_1_based=TRUE,
            DataProvider="Ensembl",
            Maintainer="Bioconductor Maintainer <maintainer@bioconductor.org>",
            RDataClass="GRanges",
            DispatchClass="GTFFile",
            SourceType="GTF",
            Location_Prefix=baseUrl,
            RDataDateAdded=Sys.time(),
            Recipe=NA_character_,
            Tags=c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
}

makeAnnotationHubResource("EnsemblGtfImportPreparer", makeEnsemblGtfToAHM)
