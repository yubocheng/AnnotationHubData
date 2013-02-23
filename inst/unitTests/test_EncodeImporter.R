library(AnnotationHubData)
library(RUnit)
library(RCurl)
#-------------------------------------------------------------------------------
options(stringsAsFactors=FALSE)
#destinationDir <- "~/tmp/encodeDCC"
destinationDir <- tempdir()
#-------------------------------------------------------------------------------
runTests <- function()
{
  notest_.extractLinksFromHtmlLines()
  notest_.extractExperimentDirectoriesFromWebPage()

  notest_.downloadFileInfo(destinationDir)
     # next test depends upon functions tested by preceeding two tests
  #test_.retrieveAllEncodeDCCMetadataFiles(destinationDir)
  #test_.learnAllEncodeMetadataCategories(destinationDir)
  #notest_.parseMetadataFiles.1(destinationDir)

  #notest_loadMetadata()
  #hotest_assemblParams()
  #notest_createEncodeResource.1()
  
} # runTests
#-------------------------------------------------------------------------------
runTiming <- function()
{
    abInitioMetadataLoad <- function(destinationDir){
        AnnotationHubData:::.retrieveAllEncodeDCCMetadataFiles (destinationDir)
        all.keys <-
           AnnotationHubData:::.learnAllEncodeMetadataCategories(destinationDir)$all.keys
        all.metadataFiles <- file.path(destinationDir, dir(destinationDir))
        tbl <- data.frame()
        browser("start")
        for (filename in all.metadataFiles) {
            tbl.new <- AnnotationHubData:::.parseMetadataFile (filename, all.keys)
            tbl <- rbind(tbl, tbl.new)
            printf("%40s new rows: %3d  total.rows: %4d", filename, nrow(tbl.new),
                   nrow(tbl))
          } # for file
        tbl
        }# abInitioMetadataLoad

    print(system.time({tbl <- abInitioMetadataLoad(destinationDir)}))

    tbl
                 

} # runTiming
#-------------------------------------------------------------------------------
notest_loadMetadata <- function ()
{
    print('--- test_loadMetadata')
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)
    checkEquals(dim(tbl.md), c(24396, 52))

} # test_loadMetadata
#-------------------------------------------------------------------------------
notest_assemblParams <- function()
{
    print('--- test_assembleParams')

    importer <- EncodeImporter()
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
    importer <- EncodeImporter()
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
notest_.extractLinksFromHtmlLines <- function()
{
    print("--- notest_.extractLinksFromHtmlLines")

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
    
} # notest_.extractLinksFromHtmlLines
#-------------------------------------------------------------------------------
notest_.extractExperimentDirectoriesFromWebPage <- function()
{
    print("--- notest_.extractExperimentDirectoriesFromWebPage")
    subdirs <- AnnotationHubData:::.extractExperimentDirectoriesFromWebPage(EncodeBaseURL())
    checkTrue(length(subdirs) > 50)
    checkTrue("wgEncodeUwTfbs/" %in% subdirs)

} # notest_.extractExperimentDirectoriesFromWebPage
#-------------------------------------------------------------------------------
# download 54/55 file information files, save them in immediated subdirectory:
#  fileInfo/*.info
#
notest_.downloadFileInfo <- function(destinationDir)
{
    print('--- notest_.downloadFileInfo')
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

} # notest_.downloadFileInfo
#-------------------------------------------------------------------------------
# make sure we can get each files.txt (encodeDCC metadata file, one per
# project directory).  each file has 1 or more (and as many as 4000) lines, each
# describing a file in the project
notest_.retrieveAllEncodeDCCMetadataFiles <- function(destinationDir)
{
    print("--- notest_.retrieveAllEncodeDCCMetadataFiles")
    AnnotationHubData:::.retrieveAllEncodeDCCMetadataFiles(destinationDir)
    checkTrue(length(grep("wgEncode", dir(destinationDir))) > 50) # 55 in feb 2013
    browser("trae")

} # notest_.retrieveAllEncodeDCCMetadataFiles
#-------------------------------------------------------------------------------
notest_.learnAllEncodeMetadataCategories <- function(destinationDir)
{
    print("--- notest_.learnAllEncodeMetadataCategories")
    filesPresent <- length(grep("wgEncode", dir(destinationDir))) > 50
    if(!filesPresent)
        AnnotationHubData:::.retrieveAllEncodeDCCMetadataFiles(destinationDir)
    result <-
        AnnotationHubData:::.learnAllEncodeMetadataCategories(destinationDir,
                                                              verbose=FALSE)
    all.keys <- result$all.keys
    checkTrue(length(all.keys) > 50) # 51 in feb 2013
       # a random sampling of empirically observed keys
    checkTrue(all(c("fragSize","geoSampleAccession","grant","insertLength","lab")
                    %in% all.keys))
    checkTrue(result$total.lines > 24000)    # 24521 in feb 2013
    result

} # notest_.learnAllEncodeMetadataCategories
#-------------------------------------------------------------------------------
# read one metadata file, make sure it has a sensible number of columns and rows
# and that three previously observed files are mentioned
#
notest_.parseMetadataFiles.1 <- function(directory)
{
    print('--- test_.parseMetadataFiles.1')
    
    result <- test_.learnAllEncodeMetadataCategories(directory)

        # test wgEncodeAffyRnaChip
    full.path <- file.path(directory, "wgEncodeAffyRnaChip.info")
    tbl <- AnnotationHubData:::.parseMetadataFiles(full.path, result$all.keys)
    checkTrue(ncol(tbl) > 50) # metadata columns
    checkTrue(nrow(tbl) > 50) # metadata rows, 52 in feb 2013, one for each
                              # data file in this directory
    checkTrue(all(
         c("wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal.broadPeak.gz",
           "wgEncodeAffyRnaChipFiltTransfragsGm12878CytosolLongnonpolya.broadPeak.gz",
           "wgEncodeAffyRnaChipFiltTransfragsGm12878CytosolLongpolya.broadPeak.gz")
                %in% rownames(tbl)))


} # test_.parseMetadataFiles.1
#-------------------------------------------------------------------------------
# read three metadata file, make sure the combined data.frame returned
# has a sensible number of columns and rows
#
notest_.parseMetadataFiles.2 <- function(directory)
{
    print('--- notest_.parseMetadataFiles.2')
    
    summary <- notest_.learnAllEncodeMetadataCategories(directory)

    metadata.files.2 <- file.path(destinationDir, list.files(destinationDir)[1:2])


    tbl <- AnnotationHubData:::.parseMetadataFiles(metadata.files.2,
                                                   summary$all.keys,
                                                   summary$total.lines)
      # the composite metadata field is, in a sense, a project directory name
      # shared by all files that come from the same encodeDCC directory

    project.names <- unique(tbl$composite)
    checkEquals(length(project.names), length(metadata.files.2))
    
      # now check the colnames, and row count of the returned tbl
    checkEquals(ncol(tbl), length(summary$all.keys))
    data.file.count <- 0
    for (metadata.file in metadata.files.2)
        data.file.count <- data.file.count +
            length(scan(metadata.file, what=character(), sep="\n", quiet=TRUE))
    checkEquals(nrow(tbl), data.file.count)
    

} # notest_.parseMetadataFiles.3
#-------------------------------------------------------------------------------
