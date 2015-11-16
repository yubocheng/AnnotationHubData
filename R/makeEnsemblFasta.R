### =========================================================================
### makeEnsemblFasta()
### -------------------------------------------------------------------------
###

## Adjust this expression in order to save painful-reprocessing of older files.
## .ensemblReleaseRegex <- ".*release-(69|7[[:digit:]]|8[[:digit:]])"
## .ensemblReleaseRegex <- ".*release-(79|8[[:digit:]])"
## for a speed run just do one set
.ensemblReleaseRegex <- ".*release-81"

## list directories below url/dir satisfying regex
.ensemblDirUrl <-
    function(url, dir,  regex = .ensemblReleaseRegex)
{
    lst <- .listRemoteFiles(url)
    releases <- paste0(url, lst)
    paste(grep(regex, releases, value=TRUE), dir, sep="/")
}

## rename SourcePath by replacing baseUrl with 'ensembl/'
.ensemblSourcePathFromUrl <-
    function(baseUrl, sourceUrl)
{
    sub(baseUrl, "ensembl/", sourceUrl)
}

## mangle url to metadata where possible
.ensemblMetadataFromUrl <-
    function(sourceUrl){
    releaseRegex <- ".*(release-[[:digit:]]+).*"
    title <- sub(".gz$", "", basename(sourceUrl))
    root <- setNames(rep(NA_character_, length(sourceUrl)), title)
    species <- gsub("_", " ", sub("^([[:alpha:]_]+)\\.(.*)", "\\1", title),
                    fixed=TRUE)
    taxonomyId <- local({
        uspecies <- unique(species)
        GenomeInfoDb:::.taxonomyId(uspecies)[match(species, uspecies)]
    })
    ## extract info about source size and source mod date etc.
    ftpInfo <- .httrFileInfo(files=sourceUrl) 
    sourceSize <- ftpInfo$size
    sourceLastModDate <- ftpInfo$date

    list(annotationHubRoot = root, title=title, species = species,
         taxonomyId = as.integer(taxonomyId),
         genome = sub("^([[:alpha:]_]+)\\.(\\w*)\\.(.*)", "\\2", title,
           perl=TRUE),
         sourceSize=sourceSize,
         sourceLastModifiedDate=sourceLastModDate,
         sourceVersion = sub(releaseRegex, "\\1", sourceUrl))
}

.ensemblFastaTypes <-
    c("cdna.all", "dna_rm.toplevel", "dna_sm.toplevel",
      "dna.toplevel", "ncrna", "pep.all")

## get urls 
.ensemblFastaSourceUrls <- function(baseUrl)
{
    want <- .ensemblDirUrl(baseUrl, "fasta/")

    .processUrl <- function(url) {
        listing <- .ftpDirectoryInfo(url)

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

## metadata generator
makeEnsemblFastaToAHMs <-
    function(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
             justRunUnitTest = FALSE, BiocVersion = biocVersion())
{
    time1 <- Sys.time()
    sourceUrl <- .ensemblFastaSourceUrls(baseUrl)
    if (justRunUnitTest)
        sourceUrl <- sourceUrl[1:5]
 
    sourceFile <- .ensemblSourcePathFromUrl(baseUrl, sourceUrl)
    meta <- .ensemblMetadataFromUrl(sourceUrl) 
    dnaType <- local({
        x <- basename(dirname(sourceFile))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("FASTA", dnaType, "sequence for", meta$species)

    ## rdatapaths db table needs an extra row for the index file
    rdataPath <- sub(".gz$", ".rz", sourceFile)
    rdps <- rep(rdataPath, each=2)
    rdatapaths <- split(rdps, f=as.factor(rep(1:length(rdataPath),each=2)))
    ## second record of each set becomes the '.fai' file
    rdatapaths <- lapply(rdatapaths,
                         function(x){x[2] <- paste0(x[2],".fai") ; return(x)})
 
    Map(AnnotationHubMetadata,
        Description=description,
        Genome=meta$genome,
        RDataPath=rdatapaths,
        SourceUrl=sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        SourceSize=meta$sourceSize,
        SourceLastModifiedDate=meta$sourceLastModifiedDate,
        MoreArgs=list(
          BiocVersion=BiocVersion,
          Coordinate_1_based = TRUE,
          DataProvider="Ensembl",
          Maintainer="<maintainer@bioconductor.org>",
          SourceType="FASTA",
          DispatchClass="FaFile", 
          RDataClass=c("FaFile", "FaFile"),
          RDataDateAdded=Sys.time(),
          Recipe="AnnotationHubData:::ensemblFastaToFaFile",
          Tags=c("FASTA", "ensembl", "sequence")))
}

## recipe: unzips .gz file and indexes it; save as .rz and .rz.fai
ensemblFastaToFaFile <- function(ahm)
{
    ## FIXME: normalizePath() only works on the files that exist ...
    #faOut <- normalizePath(outputFile(ahm)[[1]])
    faOut <- outputFile(ahm)[[1]]  ## target out file
    srcFile <- sub('.rz$','.gz',faOut)
    razip(srcFile)    ## which we unzip
    indexFa(faOut)    ## and index
}

## create the class and newResources() method
makeAnnotationHubResource("EnsemblFastaImportPreparer", makeEnsemblFastaToAHMs)
