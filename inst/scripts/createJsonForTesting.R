library(AnnotationHubData)
library(AnnotationHubServer)
library(RUnit)
#-------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#-------------------------------------------------------------------------------
createJson <- function(projectName, dataFileName)
{
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)

    stopifnot(dataFileName %in% rownames(tbl.md))
    tbl.bpk <- tbl.md[dataFileName,]
    annotationHubRoot <- "/home/ubuntu/AnnotationHubData/inst/extdata"
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
                file.path("/home/ubuntu/AnnotationHubData/inst/extdata/goldenpath/hg19/encodeDCC", projectName))
    checkEquals(projectPath, file.path("goldenpath/hg19/encodeDCC", projectName))

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
# from http://hgdownload.cse.ucsc.edu/goldenPath/hg19/encodeDCC/wgEncodeSydhTfbs/wgEncodeSydhTfbsK562Brf2StdPk.narrowPeak.gz
# projectName <- "wgEncodeSydhTfbs"
# dataFileName <- "wgEncodeSydhTfbsK562Brf2StdPk.narrowPeak.gz"
# jsonPath <- createJson(projectName, dataFileName)
# printf("created new jsonfile: %s", jsonPath)
# stopifnot(file.exists(jsonPath))

# projectName <- "wgEncodeCshlLongRnaSeq"
# dataFileName <- "wgEncodeCshlLongRnaSeqHmecCellPamGeneDeNovo.gtf.gz"
# jsonPath <- createJson(projectName, dataFileName)
# printf("created new jsonfile: %s", jsonPath)
# stopifnot(file.exists(jsonPath))

projectName <- "wgEncodeCshlShortRnaSeq"
dataFileName <- "wgEncodeCshlShortRnaSeqHepg2NucleusShorttotalTapContigs.bedRnaElements.gz"
jsonPath <- createJson(projectName, dataFileName)
printf("created new jsonfile: %s", jsonPath)
stopifnot(file.exists(jsonPath))

