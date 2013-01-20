library(AnnotationHubData)
library(AnnotationHubServer)
library(RUnit)
#-------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#-------------------------------------------------------------------------------
createJson <- function()
{
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)

    # http://hgdownload.cse.ucsc.edu/goldenPath/hg19/encodeDCC/wgEncodeSydhTfbs/wgEncodeSydhTfbsK562Brf2StdPk.narrowPeak.gz
    dataFileName <- "wgEncodeSydhTfbsK562Brf2StdPk.narrowPeak.gz"
    stopifnot(dataFileName %in% rownames(tbl.md))
    tbl.bpk <- tbl.md[dataFileName,]
    annotationHubRoot <- "/home/ubuntu/AnnotationHubData/inst/extdata"
                         # "/mnt/extdata/AnnotationHubServer_data"
    directory <- tbl.bpk[1, "dataDir"]
    file.info <- as.list(tbl.bpk[1,])
    file.size <- tbl.bpk[1,"size"]
    printf("creating resource for %s of size: %d", dataFileName, file.size)

    webSiteRoot <- "http://hgdownload.cse.ucsc.edu"
        # check this against info, but don't always believe it!
    genomeVersion <- "hg19"

    experiment.md <- as.list(tbl.md[dataFileName,])
    projectName <- experiment.md$dataDir
    projectPath <- file.path("goldenpath", genomeVersion, "encodeDCC", projectName)
    localStorageDirectory <- file.path(annotationHubRoot, projectPath)
    webSiteSourceDirectory <- file.path(webSiteRoot, projectPath)
    
    checkEquals(localStorageDirectory,
                "/home/ubuntu/AnnotationHubData/inst/extdata/goldenpath/hg19/encodeDCC/wgEncodeSydhTfbs")
    checkEquals(projectPath, "goldenpath/hg19/encodeDCC/wgEncodeSydhTfbs")

    params <- assembleParams(importer, experiment.md, webSiteSourceDirectory,
                             annotationHubRoot, projectPath, genomeVersion,
                             dataFileName)

    md <- do.call(AnnotationHubMetadata, params)
       # the above ctor creates the versioned json file we need for our own
       # testing.  AND it calculates a name for the RData file run-recipe will
       # eventually create.  the json file names is easily deduced from that name.
       # inasmuch as the RData file is a relative file path, prepend the current
       # anntationHubRoot. 

    jsonFilePath <- sub(".RData", ".json", md@RDataPath, fixed=TRUE)
    fullJsonFilePath <- file.path(annotationHubRoot,jsonFilePath)
    checkTrue(file.exists(fullJsonFilePath))

    fullJsonFilePath

} # createJson
#-------------------------------------------------------------------------------
