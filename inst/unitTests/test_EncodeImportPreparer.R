#destinationDir <- "~/tmp/encodeDCC"
destinationDir <- tempdir()

library(AnnotationHubData)
library(RUnit)
library(RCurl)
#-------------------------------------------------------------------------------
paulsTests <- function()
{
    options(stringsAsFactors=FALSE)

    test_.extractLinksFromHtmlLines()
    test_.extractExperimentDirectoriesFromWebPage()

    test_.downloadFileInfo()
       # next test depends upon functions tested by preceeding two tests
    test_.retrieveEncodeDCCMetadataFiles(destinationDir)
    test_.learnAllEncodeMetadataCategories()
    test_parseMetadataFiles.1()
    test_parseMetadataFiles.2()
    test_saveAndLoadMetadata()
    
    #test_assemblParams()
    #test_createEncodeResource.1()
  
} # runTests
#-------------------------------------------------------------------------------
notest_assemblParams <- function()
{
    print('--- test_assembleParams')

    importer <- EncodeImportPreparer()
    tbl.md <- metadataTable(importer)
    
    annotationHubRoot <- "/foo/bar"
    webSiteRoot <- "http://hgdownload.cse.ucsc.edu"
        # check this against info, but don't always believe it!
    genomeVersion <- "hg19"
    dataFileName <- "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz"


    experiment.md <- as.list(tbl.md[dataFileName,])
    projectName <- experiment.md$dataDir
    projectPath <- file.path("goldenpath", genomeVersion, "encodeDCC", projectName)
    localStorageDirectory <- file.path(annotationHubRoot, projectPath)
    webSiteSourceDirectory <- file.path(webSiteRoot, projectPath)
    
    checkEquals(localStorageDirectory, "/foo/bar/goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt")
    checkEquals(projectPath, "goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt")

    params <- assembleParams(importer, experiment.md, webSiteSourceDirectory,
                             annotationHubRoot, projectPath, genomeVersion,
                             dataFileName)
    
    with(params, {
        checkEquals(Species, "Homo sapiens");
        checkEquals(Genome, "hg19");
        checkEquals(Recipe, "extendedBedToGRanges");
        checkTrue(is.list(RecipeArgs))
        checkEquals(RDataClass, "GRanges");
        checkEquals(RDataVersion, "0.0.1");
        checkEquals(Maintainer, "Paul Shannon <pshannon@fhcrc.org>");
        checkEquals(DataProvider, "hgdownload.cse.ucsc.edu");
        checkEquals(Coordinate_1_based, FALSE);
        checkEquals(RDataDateAdded, as.character(Sys.Date()))
        checkEquals(AnnotationHubRoot, annotationHubRoot)
        checkEquals(SourceFile,
              "goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt/wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz");
        checkEquals(Title, "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna");
        checkEquals(Description,
              "RbpAssocRna broadPeak H1-hESC GSM787620 wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna RipGeneSt wgEncodeEH001235 SunyAlbany");
        checkEquals(SourceUrl,
              "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt/wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz");
        checkEquals(SourceVersion, "Nov_69522,Mil_AB3790 antibodies");
        })
    

} # test_assembleParams
#-------------------------------------------------------------------------------
notest_createEncodeResource.1 <- function()
{
    print('--- test_createEncodeResource.1')
    importer <- EncodeImportPreparer()
    tbl.md <- metadataTable(importer)
    
    dataFileName <- "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz"
    checkTrue(dataFileName %in% rownames(tbl.md))
    
    directory <- tbl.md[dataFileName, "dataDir"]
    experiment.metadata <- as.list(tbl.md[dataFileName,])

    annotationHubRoot.tmp <- tempdir()

    webSiteRoot <- "http://hgdownload.cse.ucsc.edu"

    checkEquals(experiment.metadata$origAssembly, "hg18")
    genomeVersion <- "hg19"

    RDataFilename <- createResource(importer, annotationHubRoot.tmp,
                                    webSiteRoot, genomeVersion,
                                    dataFileName, experiment.metadata,
                                    insertIntoDatabase=FALSE,
                                    verbose=FALSE)

    load(RDataFilename)
    checkTrue(exists('gr'))
    checkEquals(length(gr), 147)
    md <- mcols(gr)
    checkEquals(dim(md), c(147, 5))
    checkEquals(colnames(md), c("name", "score", "signalValue", "pValue", "qValue"))

    TRUE

} # test_createEncodeResource.1 
#-------------------------------------------------------------------------------
# ensure that, in simple web page crawling, we can extract
#   "wgEncodeAffyRnaChip/"
# from
#  <a href=\"wgEncodeAffyRnaChip/\">wgEncodeAffyRnaChip/</a>       05-Jul-2012 06:57    -   "    
test_.extractLinksFromHtmlLines <- function()
{
    print("--- test_.extractLinksFromHtmlLines")

    lines <- strsplit(getURL(EncodeBaseURL()), "\n")[[1]]
    lines <- lines[grep("href", lines, ignore.case=TRUE)]
    checkEquals(length(grep ("wgEncodeAffyRnaChip", lines)), 1)
    
    removers <- grep ("Parent Directory", lines, ignore.case=TRUE)
    if(length(removers) > 0)
      lines <- lines[-removers]

    checkTrue(length(lines) > 50)  # 55 on (8 jan 2013)
    links <- AnnotationHubData:::.extractLinksFromHtmlLines(lines)
    checkEquals(length(lines), length(links))
    checkTrue("wgEncodeAffyRnaChip/" %in% links)
    
} # test_.extractLinksFromHtmlLines
#-------------------------------------------------------------------------------
test_.extractExperimentDirectoriesFromWebPage <- function()
{
    print("--- test_.extractExperimentDirectoriesFromWebPage")
    subdirs <- AnnotationHubData:::.extractExperimentDirectoriesFromWebPage(EncodeBaseURL())
    checkTrue(length(subdirs) > 50)
    checkTrue("wgEncodeUwTfbs/" %in% subdirs)

} # test_.extractExperimentDirectoriesFromWebPage
#-------------------------------------------------------------------------------
# download 3/55 file information files, save them in immediated subdirectory:
#  fileInfo/*.info
#
test_.downloadFileInfo <- function()
{
    print('--- test_.downloadFileInfo')
    destinationDir <- tempdir()
    all.dirs <- AnnotationHubData:::.extractExperimentDirectoriesFromWebPage(EncodeBaseURL())
       # no files.txt in these directories:  referenceSequences
    postponed <- grep("referenceSequences", all.dirs)
    if(length(postponed) > 0)
        all.dirs <- all.dirs[-postponed]

    selectedDirectories <- all.dirs[sample(1:length(all.dirs), 3)]
    AnnotationHubData:::.downloadFileInfo(EncodeBaseURL(),
                                          selectedDirectories,
                                          destinationDir, verbose=FALSE)
    checkTrue(length(list.files(destinationDir)) >=
              length(selectedDirectories))
    
    selectedDirectoriesJustNames <- sub("/", "", selectedDirectories)
    match.count <- sum(sapply (selectedDirectoriesJustNames,
                   function(dir) length(grep(dir, list.files(destinationDir)))))
    checkEquals(match.count, length(selectedDirectoriesJustNames))

} # test_.downloadFileInfo
#-------------------------------------------------------------------------------
# make sure we can get each files.txt (encodeDCC metadata file, one per
# project directory).  each file has 1 or more (and as many as 4000) lines, each
# describing a file in the project directory
test_.retrieveEncodeDCCMetadataFiles <- function(destinationDir)
{
    print("--- test_.retrieveEncodeDCCMetadataFiles")

    destinationDir <- tempdir()
      # max argument is only for testing.  
    AnnotationHubData:::.retrieveEncodeDCCMetadataFiles(destinationDir, max=3,
                                                        verbose=FALSE)
    checkTrue(length(grep("wgEncode", dir(destinationDir))) >= 3)

} # test_.retrieveEncodeDCCMetadataFiles
#-------------------------------------------------------------------------------
test_.learnAllEncodeMetadataCategories <- function()
{
    print("--- test_.learnAllEncodeMetadataCategories")

    destinationDir <- system.file("unitTests/cases/encodeDCCMetadata",
                                   package="AnnotationHubData")
    result <-
        AnnotationHubData:::.learnAllEncodeMetadataCategories(destinationDir,
                                                              verbose=FALSE)
    all.keys <- result$all.keys
        # we are testing with just 3 files.txt metadata fines, in just 3 
        # encodeDCC directories, so use lenient tests, on number of
        # total files described, number of keys found

    checkEquals(length(all.keys), 20)
    checkEquals(sort(all.keys),
                c("cell", "composite", "dataType", "dataVersion", "dateSubmitted",
                  "dateUnrestricted", "dccAccession", "grant", "lab", "localization",
                  "md5sum", "origAssembly", "project", "rnaExtract",  "size", "subId",
                  "tableName", "treatment", "type", "view"))



        # choose some basic keys (aka, metadata columns)
    checkEquals(result$total.lines, 12)
    invisible(result)

} # test_.learnAllEncodeMetadataCategories
#-------------------------------------------------------------------------------
# read one metadata file, make sure it has a sensible number of columns and rows
# and that three previously observed files are mentioned
#
test_parseMetadataFiles.1 <- function()
{
    print('--- test_parseMetadataFiles.1')
    importer <- EncodeImportPreparer()
    
    downloadDir <- system.file("unitTests/cases/encodeDCCMetadata",
                                   package="AnnotationHubData")
    result <-
        AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir,
                                                              verbose=FALSE)
    metadata.files <- dir(downloadDir)
    stopifnot(length(metadata.files) > 0)
    sample.file <- metadata.files[1]

    full.path <- file.path(downloadDir, sample.file)
    tbl <- parseMetadataFiles(importer, full.path, result$all.keys)
    checkEquals(dim(tbl), c(6,20))

    metadata.file.stem <- sub(".info", "", sample.file)

        # make sure the project-name/directoryName is embedded in all
        # of the individual filenames.
        # encodeDCC is very reliable on this count (25 feb 2013)
    
    checkEquals(length(grep(metadata.file.stem, rownames (tbl))), nrow(tbl))
    

} # test_parseMetadataFiles.1
#-------------------------------------------------------------------------------
# read two metadata files, make sure the combined data.frame returned
# has a sensible number of columns and rows
#
test_parseMetadataFiles.2 <- function()
{
    print('--- test_parseMetadataFiles.2')
    importer <- EncodeImportPreparer()
    downloadDir <- system.file("unitTests/cases/encodeDCCMetadata",
                                   package="AnnotationHubData")
    result <-
        AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir,
                                                              verbose=FALSE)
    metadata.files <- dir(downloadDir)
    stopifnot(length(metadata.files) == 2)

    full.path <- file.path(downloadDir, metadata.files)
    tbl <- parseMetadataFiles(importer, full.path, result$all.keys)
    expected.column.count <- length(result$all.keys)
    checkEquals(dim(tbl), c(12, expected.column.count))
    checkEquals(sort(result$all.keys), sort(colnames(tbl)))

} # test_parseMetadataFiles.2
#-------------------------------------------------------------------------------
test_saveAndLoadMetadata <- function()
{
    print("--- test_saveAndLoadMetadata")
    downloadDir <- system.file("unitTests/cases/encodeDCCMetadata",
                               package="AnnotationHubData")
    result <-
        AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir,
                                                              verbose=FALSE)
    metadata.files <- dir(downloadDir)
    stopifnot(length(metadata.files) == 2)
    full.paths <- file.path(downloadDir, metadata.files)
    
    importer <- EncodeImportPreparer()
    tbl <- parseMetadataFiles(importer, full.paths, result$all.keys)

    path <- tempdir()
    filename <- "enodeMDTest.RData"
    saveMetadata(importer, tbl, path, filename)
    checkTrue(file.exists(file.path(path, filename)))
    tbl.2 <- loadMetadata(importer, path, filename)
    checkIdentical(tbl, tbl.2)

} # test_saveAndLoadMetadata
#-------------------------------------------------------------------------------
