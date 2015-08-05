.ensemblBaseUrl <- "ftp://ftp.ensembl.org/pub/"

## Adjust this expression in order to save painful-reprocessing of older files.
## .ensemblReleaseRegex <- ".*release-(69|7[[:digit:]]|8[[:digit:]])"

##.ensemblReleaseRegex <- ".*release-(79|8[[:digit:]])"

## for a speed run just do one set
.ensemblReleaseRegex <- ".*release-80"


## list directories below url/dir satisfying regex
.ensemblDirUrl <-
    function(url, dir,  regex = .ensemblReleaseRegex)
{
    lst <- getURL(url=url, dirlistonly=TRUE, followlocation=TRUE) 
    lst <- strsplit(lst, "\n")[[1]]
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

## retrieve FASTA file urls from Ensembl
## these should look "like" sourcUrls in the existing DB 
.ensemblFastaSourceUrls <-
    function(baseUrl)
{
    ## handle pointer must exist in external scope.
    

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

## This recipe is 'tricky' because it has to include an extra row for the extra index file in the database.  That means that all relevant rows for the rdatapaths table needed to have extra elements in their slots...  So below you can see extra work done for RDataPath, RDataClass, RDataSize and RDataLastModifiedDate

## AHM generator
makeEnsemblFastaToAHMs <-
    function(currentMetadata, justRunUnitTest)
{
    time1 <- Sys.time()
    baseUrl = .ensemblBaseUrl
    ## get all possible sourceUrls
    sourceUrl <- .ensemblFastaSourceUrls(.ensemblBaseUrl) 
    
    if(justRunUnitTest)
        sourceUrl <- sourceUrl[1:5]
    
    sourceFile <- .ensemblSourcePathFromUrl(baseUrl, sourceUrl)
    meta <- .ensemblMetadataFromUrl(sourceUrl) 
    dnaType <- local({
        x <- basename(dirname(sourceFile))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("FASTA", dnaType, "sequence for", meta$species)

    ## For rdatapaths, I need two copies of each one
    rdataPath <- sub(".gz$", ".rz", sourceFile)
    rdps <- rep(rdataPath,each=2)
    rdatapaths <- split(rdps, f=as.factor(rep(1:length(rdataPath),each=2)))
    ## Then the 2nd record of each set need to become an '.fai' file
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
          Coordinate_1_based = TRUE,
          DataProvider = "Ensembl",
          Maintainer = "Martin Morgan <mtmorgan@fredhutch.org>",
          SourceType="FASTA",
          DispatchClass="FaFile", 
          RDataClass = c("FaFile", "FaFile"),
          RDataDateAdded = Sys.time(),
          Recipe = "AnnotationHubData:::ensemblFastaToFaFile",
          Tags = c("FASTA", "ensembl", "sequence")))
}

## recipe
ensemblFastaToFaFile <- function(ahm)
{
    
    ## faOut is target .rz file name
    faOut <- normalizePath(outputFile(ahm)[[1]] )
    ## from which we 'know' the name of the source file that will be present...
    srcFile <- sub('.rz$','.gz',faOut)
    razip(srcFile)    ## which we unzip
    indexFa(faOut)    ## and index
}



makeAnnotationHubResource("EnsemblFastaImportPreparer",
                          makeEnsemblFastaToAHMs)


## > ahms = updateResources(ahroot, BiocVersion,
## +   preparerClasses = "EnsemblFastaImportPreparer",
## +   insert = FALSE, metadataOnly=TRUE)
## INFO [2015-05-22 17:20:06] Preparer Class: EnsemblFastaImportPreparer
## Error in initialize(value, ...) : 
##   invalid names for slots of class “AnnotationHubMetadata”: Location_prefix, RDataSize, RDataLastModifiedDate
## In addition: Warning message:
## In if (is.na(species)) { :
##   the condition has length > 1 and only the first element will be used
