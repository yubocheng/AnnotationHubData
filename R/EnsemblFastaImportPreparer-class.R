.ensemblFastaTypes <-
    c("cdna.all", "dna_rm.toplevel", "dna_sm.toplevel",
      "dna.toplevel", "ncrna", "pep.all")

## retrieve FASTA file urls from Ensembl
## these should look "like" sourcUrls in the existing DB 
.ensemblFastaSourceUrls <-
    function(baseUrl)
{
    want <- .ensemblDirUrl(baseUrl, "fasta/")
    ## files in release ## BOOM

    .processUrl <- function(url) {
        listing <- getURL(url=url, followlocation=TRUE, customrequest="LIST -R")
        listing<- strsplit(listing, "\n")[[1]]

        subdirIdx <- grepl(".*/.*:", listing)  
        subdir <- sub("^(.*):$", "\\1", listing[subdirIdx])

        fileTypes <- paste(.ensemblFastaTypes, collapse="|")
        pat <- sprintf(".*(%s)\\.fa\\.gz$", fileTypes)

        fastaIdx <- grepl(pat, listing)
        fasta <- sub(".* ", "", listing[fastaIdx])

        ## match subdir w/ fasta
        subdir <- subdir[cumsum(subdirIdx)[fastaIdx]]

        sprintf("%s%s/%s", url, subdir, fasta)
    }
    
    res <- unlist(lapply(want, .processUrl), use.names=FALSE)
    if (length(res) == 0) {
        txt <- sprintf("no fasta files at %s",
                       paste(sQuote(want), collapse=", "))
        stop(paste(strwrap(txt, exdent=2), collapse="\n"))
    }
    res
}

## AHM generator
makeEnsemblFastaToAHMs <-
    function(currentMetadata)
{
    baseUrl = .ensemblBaseUrl
    ## get all possible sourceUrls
    sourceUrl <- .ensemblFastaSourceUrls(.ensemblBaseUrl) 
    
    sourceFile <- .ensemblSourcePathFromUrl(baseUrl, sourceUrl)
    meta <- .ensemblMetadataFromUrl(  ## BUG
        sourceUrl,
          "^([[:alpha:]_]+)\\.(.*)")
    dnaType <- local({
        x <- basename(dirname(sourceFile))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("FASTA", dnaType, "sequence for", meta$species)

    Map(AnnotationHubMetadata,
        AnnotationHubRoot=meta$annotationHubRoot,
        Description=description, Genome=meta$genome,
        RDataPath=sub(".gz$", ".rz", sourceFile), SourceFile=sourceFile,
        SourceUrl=sourceUrl, SourceVersion=meta$sourceVersion,
        Species=meta$species, TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        MoreArgs=list(
          Coordinate_1_based = TRUE,
          DataProvider = "ftp.ensembl.org",
          Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
          RDataClass = "FaFile",
          RDataDateAdded = Sys.time(),
          RDataVersion = "0.0.1",
          Recipe = c("ensemblFastaToFaFile", package="AnnotationHubData"),
          Tags = c("FASTA", "ensembl", "sequence")))
}

## recipe
ensemblFastaToFaFile <- function(ahm)
{
    require(Rsamtools)
    faIn <- normalizePath(inputFiles(ahm))
    faOut <- normalizePath(outputFile(ahm))

    tmp <- tempfile()
    system2("zcat", sprintf("%s > %s", faIn, tmp))
    razip(tmp, faOut)
    indexFa(faOut)
}



makeAnnotationHubResource("EnsemblFastaImportPreparer",
                          makeEnsemblFastaToAHMs)
