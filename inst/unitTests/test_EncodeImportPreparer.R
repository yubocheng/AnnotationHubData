#destinationDir <- "~/tmp/encodeDCC"
destinationDir <- tempdir()

library(AnnotationHubData)
library(RUnit)
library(RCurl)
library(httr)
#-------------------------------------------------------------------------------
url.exists <- function(url)
{
   HEAD(url)$headers$status == "200"
}
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
    
    test_.mapFormatToRecipe()
    test_encodeMetadataToAnnotationHubMetadata()
    
} # runTests
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
       # only files which end in .info:
    metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
    checkEquals(length(metadata.files), 2)
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
       # only files which end in .info:
    metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
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
    metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
    checkEquals(length(metadata.files), 2)
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
test_.mapFormatToRecipe <- function()
{
    print("--- test_.mapFormatToRecipe")
    mapFormat <- AnnotationHubData:::.mapFormatToRecipe
    checkTrue(is.null(mapFormat("bogus")$recipe))
    checkEquals(mapFormat("bogus")$recipeArgs, list())

    checkEquals(mapFormat("gtf")$recipe, "rtrackLayerImport")
    checkEquals(mapFormat("gtf")$recipeArgs, list())

    checkEquals(mapFormat("broadPeak")$recipe, "extendedBedToGRanges")
    checkEquals(mapFormat("broadpeak")$recipe, "extendedBedToGRanges")
    checkEquals(sort(names(mapFormat("broadPeak")$recipeArgs$colClasses)),
                    c("end", "name", "pValue", "qValue",
                      "score", "seqnames", "signalValue",
                      "start", "strand"))

} # test_.mapFormatToRecipe
#-------------------------------------------------------------------------------
# take the first line of the first metadata file cached with the package
# (inst/unitTests/casesencodeDCCMetadata/wgEncodeAffyRnaChip.info), and
# turn it into an AnnotationHubData 
test_encodeMetadataToAnnotationHubMetadata <- function()
{
    print("--- test_encodeMetadataToAnnotationHubMetadata")

    downloadDir <- system.file("unitTests/cases/encodeDCCMetadata", package="AnnotationHubData")
    full.path <- file.path(downloadDir, "tbl.mdTest.RData")

    load(
    checkEquals(dim(tbl.mdTest), c(2,51))
    importPrep <- EncodeImportPreparer(tbl.mdTest)

    ahRoot <- tempdir()

    ahmd.list <- encodeMetadataToAnnotationHubMetadata(importPrep, ahRoot, 1)
    checkEquals(length(ahmd.list), 1)
    amd <- ahmd.list[[1]]

      ## check the easy fields, avoid the ones not assigned by me, or which may
      ## not be stable over time
    
    checkEquals(amd@AnnotationHubRoot, ahRoot)
      ## checkEquals(amd@BiocVersion, "")
    checkEquals(amd@Coordinate_1_based, TRUE)
    checkEquals(amd@DataProvider, "EncodeDCC")
      ## checkEquals(amd@DerivedMd5, "")
    checkEquals(amd@Description, "wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal")
    checkEquals(amd@Genome, "hg19")
      ## checkEquals(amd@Maintainer, "")
      ## checkEquals(amd@Notes, "")
    checkEquals(amd@RDataClass, "GRanges")
      ## checkEquals(amd@RDataDateAdded, "")
      ## checkEquals(amd@RDataLastModifiedDate, "")
      ## checkEquals(amd@RDataPath, "")
      ## checkEquals(amd@RDataSize, "")
      ## checkEquals(amd@RDataVersion, "")
    checkEquals(amd@Recipe, "extendedBedToGRanges")
    checkEquals(length(amd@RecipeArgs[[1]]), 9)
    checkEquals(amd@SourceFile,
       "goldenpath/hg19/encodeDCC/wgEncodeAffyRnaChip/wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal.broadPeak.gz")
      ## checkEquals(amd@SourceLastModifiedDate, "")
      ## checkEquals(amd@SourceMd5, "")
      ## checkEquals(amd@SourceSize, "")
    checkEquals(amd@SourceUrl,
       "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC//wgEncodeAffyRnaChip/wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal.broadPeak.gz")
    checkEquals(amd@SourceVersion, "ENCODE Feb 2009 Freeze")
    checkEquals(amd@Species, "Homo sapiens")
    checkEquals(length(amd@Tags), 19)
    checkEquals(amd@TaxonomyId, "9606")
    checkEquals(amd@Title, "wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal")


    checkTrue(url.exists(amd@SourceUrl))

    ahmd.2 <- encodeMetadataToAnnotationHubMetadata(importPrep, ahRoot, 1:2)
    checkEquals(length(ahmd.2), 2)
    checkEquals(ahmd.2[[1]]@Title, "wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal")
    checkEquals(ahmd.2[[2]]@Title, "wgEncodeAwgDnaseDuke8988tUniPk")


} # test_encodeMetadataToAnnotationHubMetadata
#-------------------------------------------------------------------------------
