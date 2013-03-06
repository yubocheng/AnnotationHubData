.ensemblFastaTypes <-
    c("cdna.all", "dna_rm.toplevel", "dna_sm.toplevel",
      "dna.toplevel", "ncrna", "pep.all")

EnsemblFastaImportPreparer <- 
    setClass("EnsemblFastaImportPreparer", contains="ImportPreparer")

## retrieve FASTA file urls from Ensembl
.ensemblFastaSourceUrls <-
    function(baseUrl)
{
    want <- .ensemblDirUrl(baseUrl, "fasta/")
    ## files in release
    unlist(lapply(want, function(url) {
        listing <- getURL(url=url, followlocation=TRUE, customrequest="LIST -R")
        listing<- strsplit(listing, "\n")[[1]]

        subdirIdx <- grepl("\\./.*/.*:", listing)
        subdir <- sub("^\\./(.*):$", "\\1", listing[subdirIdx])

        fileTypes <- paste(.ensemblFastaTypes, collapse="|")
        pat <- sprintf(".*(%s)\\.fa\\.gz$", fileTypes)

        fastaIdx <- grepl(pat, listing)
        fasta <- sub(".* ", "", listing[fastaIdx])

        ## match subdir w/ fasta
        subdir <- subdir[cumsum(subdirIdx)[fastaIdx]]

        sprintf("%s%s/%s", url, subdir, fasta)
    }), use.names=FALSE)
}

.ensemblFastaMetadata <-
    function(baseUrl, sourceUrl)
{
    sourceFile <- .ensemblSourcePathFromUrl(baseUrl, sourceUrl)
    meta <- .ensemblMetadataFromUrl(sourceUrl)
    dnaType <- local({
        x <- basename(dirname(sourceFile))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("FASTA", dnaType, "sequence for", meta$species)

    Map(AnnotationHubMetadata,
        AnnotationHubRoot=meta$annotationHubRoot,
        Description=description, Genome=meta$genome,
        RDataPath=sourceFile, SourceFile=sourceFile,
        SourceUrl=sourceUrl, SourceVersion=meta$sourceVersion,
        Species=meta$species, Title=meta$title,
        MoreArgs=list(
          Coordinate_1_based = TRUE,
          DataProvider = "ftp.ensembl.org",
          Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
          RDataClass = "FaFile",
          RDataDateAdded = format(Sys.time(), "%Y-%m-%d GMT"),
          RDataVersion = "0.0.1",
          Recipe = c("ensemblFastaToFaFile", package="AnnotationHubData"),
          Tags = c("FASTA", "ensembl", "sequence")))
}

setMethod(newResources, "EnsemblFastaImportPreparer",
    function(importPreparer, currentMetadata = list(), ...)
{
    sourceUrls <- .ensemblFastaSourceUrls(.ensemblBaseUrl) # 732,  6 March, 2013

    ## filter known
    knownUrls <- sapply(currentMetadata, function(elt) {
        metadata(elt)$SourceUrl
    })
    sourceUrls <- sourceUrls[!sourceUrls %in% knownUrls]

    ## AnnotationHubMetadata
    .ensemblFastaMetadata(.ensemblBaseUrl, sourceUrls)
})
