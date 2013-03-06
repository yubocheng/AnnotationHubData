EnsemblFastaImportPreparer <- 
    setClass("EnsemblFastaImportPreparer", contains="ImportPreparer")

.ensemblFastaSourceUrls <-
    function(url)
    ## retrieve all possible files from Ensembl
{
    ## releases
    lst <- getURL(url=url, dirlistonly=TRUE, followlocation=TRUE)
    lst <- strsplit(lst, "\n")[[1]]
    releases <- paste0(url, lst)
    want <- paste0(grep(".*release-(69|7[[:digit:]])", releases, value=TRUE),
                   "/fasta/")
    ## files in release
    unlist(lapply(want, function(url) {
        listing <- getURL(url=url, followlocation=TRUE, customrequest="LIST -R")
        listing<- strsplit(listing, "\n")[[1]]

        subdirIdx <- grepl("\\./.*/.*:", listing)
        subdir <- sub("^\\./(.*):$", "\\1", listing[subdirIdx])

        fileTypes <-
            paste("cdna.all", "dna_rm.toplevel", "dna_sm.toplevel",
                  "dna.toplevel", "ncrna", "pep.all", sep="|")
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
    fasta <- sub(baseUrl, "ensembl/", sourceUrl)

    regex <- "^([[:alpha:]_]+)\\.([[:alpha:]]+).*"
    title <- sub(".gz$", "", basename(fasta))
    species <- gsub("_", " ", sub(regex, "\\1", title), fixed=TRUE)
    genome <- sub(regex, "\\2", title)
    dnaType <- local({
        x <- basename(dirname(fasta))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("FASTA", dnaType, "sequence for", species)

    root <- setNames(rep(NA_character_, length(fasta)), title)
    sourceVersion <- sub(".*(release-[[:digit:]]+).*", "\\1", sourceUrl)
    
    Map(AnnotationHubMetadata, AnnotationHubRoot=NA_character_,
        Description=description, Genome=genome,
        RDataPath=fasta, SourceFile=fasta,
        SourceUrl=sourceUrl, SourceVersion=sourceVersion,
        Species=species, Title=title,
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
    baseUrl <- "ftp://ftp.ensembl.org/pub/"
    sourceUrls <- .ensemblFastaSourceUrls(baseUrl)

    ## filter known
    knownUrls <- sapply(currentMetadata, function(elt) {
        metadata(elt)$SourceUrl
    })
    sourceUrls <- sourceUrls[!sourceUrls %in% knownUrls]

    ## AnnotationHubMetadata
    .ensemblFastaMetadata(baseUrl, sourceUrls)
})
