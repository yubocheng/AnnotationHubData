### =========================================================================
### makeEnsemblFastaAHM() and ensemblFastaToFaFile()
### -------------------------------------------------------------------------
###

## Adjust this expression in order to save painful-reprocessing of older files.
## .ensemblReleaseRegex <- ".*release-(69|7[[:digit:]]|8[[:digit:]])"
## .ensemblReleaseRegex <- ".*release-(79|8[[:digit:]])"
## for a speed run just do one set
## .ensemblReleaseRegex <- ".*release-81"

## list directories below url/dir satisfying regex
.ensemblDirUrl <-
    function(url, dir, regex)
{
    lst <- .listRemoteFiles(url)
    releases <- paste0(url, lst)
    paste(grep(regex, releases, value=TRUE), dir, sep="/")
}

## mangle url to metadata where possible
.ensemblMetadataFromUrl <- function(sourceUrl, twobit=FALSE) {
    releaseRegex <- ".*(release-[[:digit:]]+).*"
    if (!twobit)
      title <- sub("\\.gz$", "", basename(sourceUrl))
    else
      title <- sub("\\.fa\\.gz$", ".2bit", basename(sourceUrl))
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
    c("cdna\\.all", "dna_rm\\.toplevel", "dna_sm\\.toplevel",
      "dna\\.toplevel", "ncrna", "pep\\.all")

## get urls
.ensemblFastaSourceUrls <-
    function(baseUrl, baseDir, regex, baseTypes=.ensemblFastaTypes)
{
    want <- .ensemblDirUrl(baseUrl, baseDir, regex)

    .processUrl <- function(url) {
        listing <- .ftpDirectoryInfo(url)

        subdirIdx <- grepl(".*/.*:", listing)
        subdir <- sub("^(.*):$", "\\1", listing[subdirIdx])

        fileTypes <- paste(baseTypes, collapse="|")
        pat <- sprintf(".*(%s)\\.fa\\.gz$", fileTypes)

        fastaIdx <- grepl(pat, listing)
        fasta <- sub(".* ", "", listing[fastaIdx])

        ## match subdir w/ fasta
        subdir <- subdir[cumsum(subdirIdx)[fastaIdx]]

        ## Prefer "primary_assembly" to "toplevel" resources.
        organisms <- unique(sub("(.+?)\\..*", "\\1", fasta, perl=TRUE))
        keepIdxList <- sapply(organisms, function(x) {
            orgFiles <- fasta[grep(paste0("^", x, "\\."), fasta)]
            reBoth <- paste0("dna", c("_rm", "_sm", ""),
                "\\.(primary_assembly|toplevel)\\.")
            toplevelIdx <-
                # vapply(reBoth, function(x) length(grep(x, orgFiles)) > 1,
                #        logical(1))
                sapply(reBoth, function(x) length(grep(x, orgFiles)) > 1)
            reToplevel <- paste0("dna", c("_rm", "_sm", ""),
                "\\.toplevel\\.")[toplevelIdx]

            isRedundant <-
                # vapply(reToplevel, function(x) grepl(x, orgFiles),
                #        logical(length(x)))
                sapply(reToplevel, function(x) grepl(x, orgFiles))
            retVal <- rep(TRUE, length(orgFiles))
            if (!is.null(dim(isRedundant))) {
              retVal <- !apply(isRedundant, 1, any)
            }

            retVal
        })
        keepIdx <- base::unlist(keepIdxList)
        fasta <- fasta[keepIdx]
        subdir <- subdir[keepIdx]

        sprintf("%s%s/%s", url, subdir, fasta)
    }
    res <- base::unlist(lapply(want, .processUrl), use.names=FALSE)

    if (length(res) == 0) {
        txt <- sprintf("no fasta files at %s",
                       paste(sQuote(want), collapse=", "))
        stop(paste(strwrap(txt, exdent=2), collapse="\n"))
    }
    res
}

## metadata generator
makeEnsemblFastaToAHM <-
    function(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
             baseDir = "fasta/", regex,
             justRunUnitTest = FALSE, BiocVersion = biocVersion())
{
    time1 <- Sys.time()
    sourceUrl <- .ensemblFastaSourceUrls(baseUrl, baseDir, regex)
    if (justRunUnitTest)
        sourceUrl <- sourceUrl[1:5]

    sourceFile <- sub(baseUrl, "ensembl/", sourceUrl)
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
          Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
          SourceType="FASTA",
          DispatchClass="FaFile",
          RDataClass=c("FaFile", "FaFile"),
          RDataDateAdded=Sys.time(),
          Recipe="AnnotationHubData:::ensemblFastaToFaFile",
          Tags=c("FASTA", "ensembl", "sequence")))
}

## Used in makeEnsemblFastaAHM() and makeGencodeFastaToAHM():
## Unzips .gz file, indexes it and saves as .rz and .rz.fai.
.fastaToFaFile <- function(ahm)
{
    ## target output file
    faOut <- outputFile(ahm)[[1]]
    srcFile <- sub('.rz$','.gz',faOut)
    ## unzip and index
    razip(srcFile)
    indexFa(faOut)
}

ensemblFastaToFaFile <- function(ahm)
{
    .fastaToFaFile(ahm)
}

## create dispatch class and newResources() method
makeAnnotationHubResource("EnsemblFastaImportPreparer", makeEnsemblFastaToAHM)
