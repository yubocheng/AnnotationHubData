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
    test_parseMetadataFiles()
    test_saveAndLoadMetadata()
    
    test_.assignRecipeAndArgs()
    test_encodeMetadataToAnnotationHubMetadata()
    test_newResources()
    
} # runTests
#-------------------------------------------------------------------------------
test_ctor <- function()
{
    print("--- test_ctor")

       # an instance of EncodeImportPreparer is always constructed with
       # the path to its AnnotationHubRoot ("ahroot" for short), which may
       # be a test path, a production path, or even a bogus path
       # 
       # there are three ways the instance can get its metadata, which is
       # a data.frame of (currently) 52 columns:
       #   1) it finds and loads a serialized "tbl.md" from a file
       #      named "tbl.parsedEncodeMetadata.RData" in the ahroot directory
       #   2) it is passed a tbl.md by the caller
       #   3) if no tbl.md is supplied, and no serialized object is found
       #      it creates an empty data.frame


       # test way 1: load ahroot/tbl.parsedEncodeMetadata.RData
    ahroot <- system.file(package="AnnotationHubData", "unitTests", "cases",
                          "encodeDCCMetadata")
    eip <- EncodeImportPreparer(ahroot)
    checkEquals(dim(metadataTable(eip)), c(2,21))

       # test way 2: explicitly pass in tbl.md
    variable.name <- load(system.file(package="AnnotationHubData", "unitTests",
                                      "cases","encodeDCCMetadata",
                                      "tbl.parsedEncodeMetadata.RData"))
    checkEquals(variable.name, "tbl.md")
    eip <- EncodeImportPreparer(ahroot, tbl.md)
    checkEquals(dim(metadataTable(eip)), c(2,21))

       # test way 3: nothing passed, nothing found, create empty data.frame
    ahroot <- "/my/bogus/ahrootPath"
    eip <- EncodeImportPreparer(ahroot)
    checkEquals(annotationHubRoot(eip), ahroot)
    checkEquals(dim(metadataTable(eip)), c(0,0))

} # test_ctor
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
test_.downloadFileInfo <- function(verbose=FALSE)
{
    print('--- test_.downloadFileInfo')
    destinationDir <- tempdir()
    all.dirs <- AnnotationHubData:::.extractExperimentDirectoriesFromWebPage(EncodeBaseURL())
       # no files.txt in these directories:  referenceSequences
    postponed <- grep("referenceSequences", all.dirs)
    if(length(postponed) > 0)
        all.dirs <- all.dirs[-postponed]

    selectedDirectories <- all.dirs [sample(1:length(all.dirs), 3)]
    AnnotationHubData:::.downloadFileInfo(EncodeBaseURL(),
                                          selectedDirectories,
                                          destinationDir, verbose=verbose)
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
test_parseMetadataFiles <- function()
{
    print('--- test_parseMetadataFiles')

    ahroot <- system.file(package="AnnotationHubData",
                         "unitTests", "cases", "encodeDCCMetadata")
    downloadDir <- ahroot # file.path(ahroot, "downloads")

    importer <- EncodeImportPreparer(ahroot)
    data.file.info <- AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir, verbose=FALSE)
       # only files which end in .info:
    downloaded.metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
    checkEquals(length(downloaded.metadata.files), 2)

    full.paths <- file.path(downloadDir, downloaded.metadata.files)
    checkTrue(all(sapply(full.paths, file.exists)))
    tbl <- parseMetadataFiles(importer, full.paths, data.file.info)
    checkEquals(dim(tbl), c(12,21))

        # "remoteDirectory" is a column we add explicitly
        # (in EncodeImportPreparer-class, parseMetadataFiles)
        # bassed upon the name of the "*.info" files, which
        # were renamed when downloaded, from files.txt to
        # <remoteDirectory>.info.   make sure this is a column
        # in the data.frame
    checkTrue("remoteDirectory" %in% colnames(tbl))
    checkEquals(sort(unique(tbl$composite)), c("wgEncodeAffyRnaChip","wgEncodeAwgDnaseUniPk"))
    

} # test_parseMetadataFiles
#-------------------------------------------------------------------------------
test_saveAndLoadMetadata <- function()
{
    print("--- test_saveAndLoadMetadata")
    ahroot <- system.file(package="AnnotationHubData",
                          "unitTests", "cases", "encodeDCCMetadata")

    file.info <-
        AnnotationHubData:::.learnAllEncodeMetadataCategories(ahroot,
                                                              verbose=FALSE)
    metadata.files <- grep("\\.info$", dir(ahroot), value=TRUE)
    checkEquals(length(metadata.files), 2)
    full.paths <- file.path(ahroot, metadata.files)
    
    importer <- EncodeImportPreparer(ahroot)
    tbl <- parseMetadataFiles(importer, full.paths, file.info)

    path <- tempdir()
    filename <- "enodeMDTest.RData"
    saveMetadata(importer, tbl, path, filename)
    checkTrue(file.exists(file.path(path, filename)))
    tbl.2 <- loadMetadata(importer, path, filename)
    checkIdentical(tbl, tbl.2)

} # test_saveAndLoadMetadata
#-------------------------------------------------------------------------------
test_.convertSizeStringsToNumeric <- function()
{
    print("--- test_.convertSizeStringsToNumeric")
    s <- c("1", "1K", "1M", "1G")
    x <- AnnotationHubData:::.convertSizeStringsToNumeric(s)
    checkEqualsNumeric(x, c(1, 1000, 1000000, 1000000000))

} # test_.convertSizeStringsToNumeric 
#-------------------------------------------------------------------------------
test_.assignRecipeAndArgs <- function()
{
    print("--- test_.assignRecipeAndArgs")
    mapFormat <- AnnotationHubData:::.assignRecipeAndArgs
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

} # test_.assignRecipeAndArgs
#-------------------------------------------------------------------------------
# take the first line of the first metadata file cached with the package
# (inst/unitTests/casesencodeDCCMetadata/wgEncodeAffyRnaChip.info), and
# turn it into an AnnotationHubData 
test_encodeMetadataToAnnotationHubMetadata <- function()
{
    print("--- test_encodeMetadataToAnnotationHubMetadata")

    packageTestDataDir <- system.file("unitTests/cases/encodeDCCMetadata", package="AnnotationHubData")
    full.path <- file.path(packageTestDataDir, "tbl.parsedEncodeMetadata.RData")
    load(full.path)
    
    checkEquals(dim(tbl.md), c(2,21))
    ahRoot <- tempdir()
    eip <- EncodeImportPreparer(ahRoot, tbl.md)


        # test with just one entry
    eip <- encodeMetadataToAnnotationHubMetadata(eip, subset=1, verbose=FALSE)
    ahmd.list <- eip@ahmd

    checkEquals(length(ahmd.list), 1)
    amd <- ahmd.list[[1]]

      ## check the easy fields, avoid the ones not assigned by us, or which may
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
    checkEquals(length(amd@Tags), 20)
    checkEquals(amd@TaxonomyId, "9606")
    checkEquals(amd@Title, "wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal")


    checkTrue(url.exists(amd@SourceUrl))

    eip <- encodeMetadataToAnnotationHubMetadata(eip, subset=1:2, verbose=TRUE)
    ahmd.2 <- eip@ahmd
    checkEquals(length(ahmd.2), 2)
    checkEquals(ahmd.2[[1]]@Title, "wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal")
    checkEquals(ahmd.2[[2]]@Title, "wgEncodeAwgDnaseDuke8988tUniPk")

} # test_encodeMetadataToAnnotationHubMetadata
#-------------------------------------------------------------------------------
test_newResources <- function()
{
    print("--- test_newResources")

    packageTestDataDir <- system.file("unitTests/cases/encodeDCCMetadata", package="AnnotationHubData")
    full.path <- file.path(packageTestDataDir, "tbl.parsedEncodeMetadata.RData")
    load(full.path)
    
    checkEquals(dim(tbl.md), c(2,21))
    ahRoot <- tempdir()
    eip <- EncodeImportPreparer(ahRoot, tbl.md)
    eip <- encodeMetadataToAnnotationHubMetadata(eip,
                                                 subset=NA,
                                                 verbose=FALSE)
    checkEquals(length(newResources(eip, currentMetadata=eip@ahmd[1])), 1)
    checkEquals(length(newResources(eip)),2)
    checkEquals(length(newResources(eip, eip@ahmd)), 0)

} # test_newResources
#-------------------------------------------------------------------------------
# a utility method for the package maintainter, to create a 2-row instance
# of our slighly augmented data.frame version of downloaded EncodeDCC metadata
# this describes data files in two different EncodeDCC directories
admin_createTinyMetadataTableForTesting <- function()
{
    tbl.mdTest <- test_parseMetadataFiles.2()[c(1,7),]
    checkEquals(dim(tbl.mdTest), c(2, 21))
    checkEquals(rownames(tbl.mdTest),
                c("wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal.broadPeak.gz",
                  "wgEncodeAwgDnaseDuke8988tUniPk.narrowPeak.gz"))
    checkEquals(tbl.mdTest$remoteDirectory,
                c("wgEncodeAffyRnaChip", "wgEncodeAwgDnaseUniform"))
       # here is the problem that the new column "remoteDirectory" solves:
       # in this one case, the composite field holds a value which differs
       # from the remoteDirectory:
       # AwgDnaseUniPk != AwgDnaseUniform
    checkEquals(tbl.mdTest$composite,
                c("wgEncodeAffyRnaChip", "wgEncodeAwgDnaseUniPk"))
    
       # ensure the working directory is what we need to write this package
       # into its proper home in the built package, from which it is
       # read by test_encodeMetadataToAnnotationHubMetadata, below.
    checkEquals(basename(getwd()), "unitTests")
    save(tbl.mdTest, file="cases/encodeDCCMetadata/tbl.mdTest.RData")
    browser("ctmdt")

} # admin_createTinyMetadataTableForTesting
#-------------------------------------------------------------------------------
# 0.6 seconds for this test
admin_speedupMetadataCreation <- function()
{
    print("-- admin_speedupMetadataCreation")
    downloadDir <- "downloadTmp/tfbs" # system.file("unitTests/cases/encodeDCCMetadata", package="AnnotationHubData")

    print(system.time({
        data.file.info <- AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir, verbose=TRUE)
        importer <- EncodeImportPreparer()
           # only files which end in .info:
        metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
           #    52442 Mar  4 10:29 wgEncodeUchicagoTfbs.info
           #   536582 Mar  4 10:29 wgEncodeUwTfbs.info
           #  1859779 Mar  4 10:29 wgEncodeSydhTfbs.info
        metadata.files <- c("wgEncodeUchicagoTfbs.info",
                            "wgEncodeUwTfbs.info") #,
                            #"wgEncodeSydhTfbs.info")
        full.path <- file.path(downloadDir, metadata.files)
        tbl <- parseMetadataFiles(importer, full.path, data.file.info)
        #browser("post-fast")
        checkEquals(dim(tbl), c (1044,30))
        }))
         

} # admin_speedupMetadataCreation
#-------------------------------------------------------------------------------
# 88 seconds to download all 55 files
# 17 seconds to translate into one big 
admin_downloadAndTranslateteAllMetadata <- function(download=FALSE, verbose=FALSE)
{
    print("-- admin_downloadAndTranslateAllMetadata")

    downloadDir <- "cases/encodeDCCMetadata/downloads"
    if(!download)
        stopifnot(file.exists(downloadDir))
    
    if(download) {
        downloadDir <- tempdir()
         print(system.time({
              AnnotationHubData:::.retrieveEncodeDCCMetadataFiles(downloadDir, max=NA,
                                                                   verbose=verbose)
           }))
        printf("downloaded %d files from encodeDCC to %s", length(list.files(downloadDir)),
               downloadDir)
        }# if download
    
    print(system.time({
        dataFile.summary <- AnnotationHubData:::.learnAllEncodeMetadataCategories(
                downloadDir, verbose=verbose)
        importer <- EncodeImportPreparer(downloadDir)
           # only files which end in .info:
        metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
        full.path <- file.path(downloadDir, metadata.files)
        tbl.md <- parseMetadataFiles(importer, full.path, dataFile.summary,
                                      verbose=verbose)
        save(tbl.md, file="./tbl.parsedEncodeMetadata.RData")
        }))

    # save(tbl, file="cases/encodeDCCMetadata/tbl.mdFull.RData")
    # save(tbl, file="/Users/pshannon/s/data/public/encode/tbl.encode.metadata.RData")
    invisible(tbl)

} # admin_downloadAndTranslateAllMetadata
#-------------------------------------------------------------------------------
admin_createAllAHMetadata <- function()
{
    print("--- admin_createAllAHMetadata")
    file <- "/Users/pshannon/s/data/public/encode/tbl.encode.metadata.RData"
    load(file)
    checkEquals(dim(tbl.md), c (24521, 52))
    supported.formats <- c("narrowPeak", "broadPeak", "gtf", "bedRnaElements")
    tbl.md <- subset(tbl.md, type %in% supported.formats)
    record.count <- 4581
    checkEquals(dim(tbl.md), c (record.count, 52))
    importPrep <- EncodeImportPreparer(tbl.md)
    ahRoot <- tempdir()
    #ahmd <- encodeMetadataToAnnotationHubMetadata(importPrep, ahRoot, subset=1:200, verbose=TRUE)
    ahmd <- encodeMetadataToAnnotationHubMetadata(importPrep, subset=1:200, verbose=TRUE)
    browser("ahmd")
    #checkEquals(length(ahmd), record.count)
    for(i in 1:record.count){
        link <- ahmd[[i]]@SourceUrl
        printf("%4d: %20s: %s %60s", i, tbl.md$composite[i], url.exists(link), link)
        }
      # ahmd.2[[125]]
      #  wgEncodeAwgDnaseUniform/
      # url.exists("http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC//wgEncodeAwgDnaseUniPk/wgEncodeAwgDnaseUwHl60UniPk.narrowPeak.gz")
    
    browser("bulk")

} # bulkTest_createAllAHMetadata
#-------------------------------------------------------------------------------
admin_findSmallSampleFiles <- function()
{
    print("--- admin_findSmallSampleFiles")

    file <- "/Users/pshannon/s/data/public/encode/tbl.encode.metadata.RData"
    load(file)

    supported.formats <- c("narrowPeak", "broadPeak", "gtf", "bedRnaElements")
    for(format in supported.formats){
        tbl.sub <- subset(tbl.md, type==format)
        tbl.sub <- tbl.sub[order(tbl.sub$size),]
        printf("%30s: %s", rownames(tbl.sub)[1], tbl.sub$size[1])
        } # for format

} # admin_findSmallSampleFiles
#-------------------------------------------------------------------------------
admin_runSmallSampleFiles <- function()
{
    file <- "/Users/pshannon/s/data/public/encode/tbl.encode.metadata.RData"
    load(file)

    print("---admin_runSmallSampleFiles") # 536, 204, 156k & 260k bytes respectively
        # the result of admin_findSmallSampleFiles above
    small.files <- list(narrowPeak="wgEncodeSydhTfbsK562Xrcc4StdPk.narrowPeak.gz",
                        broadPeak="wgEncodeSunySwitchgearHt1080Elavl1RbpAssocRna.broadPeak.gz",
                        gtf="wgEncodeCshlLongRnaSeqHmecCellPamGeneDeNovo.gtf.gz",
                        bedRna="wgEncodeCshlShortRnaSeqHepg2NucleusShorttotalTapContigs.bedRnaElements.gz")

        # where are they in the big table?
    sample.indices <- match(as.character(small.files), rownames(tbl.md))

    importPrep <- EncodeImportPreparer(tbl.md)
    ahRoot <- tempdir()
    ahmd <- encodeMetadataToAnnotationHubMetadata(importPrep, ahRoot,
                                                  subset=sample.indices,
                                                  verbose=TRUE)
    for(i in 1:length(small.files)) {
        remote.file <- ahmd[[i]]@SourceUrl
        checkTrue(url.exists(remote.file))
        directory.path <- file.path (ahRoot, dirname(ahmd[[i]]@SourceFile))
        if(!file.exists(directory.path))
            dir.create(directory.path, recursive=TRUE)
        local.file <- file.path(directory.path, basename(ahmd[[i]]@SourceFile))
        download.file(remote.file, local.file, quiet=TRUE)
        checkTrue(file.exists(local.file))
        recipe <- AnnotationHubRecipe(ahmd[[i]])
        zz <- run(recipe)
        #browser("runRecipe")
        print(load(zz))
        printf("%s: as gr: %d  as gzipped table: %d",
               basename(local.file), length(gr),
               nrow(read.table(local.file, sep="\t", header=FALSE)))
        printf("mcols: %s", list.to.string(colnames(mcols(gr))))
        checkEquals(length(gr), nrow(read.table(local.file, sep="\t", header=FALSE)))
        } # for i

} # admin_runSmallSampleFiles
#-------------------------------------------------------------------------------
import.md <- function(verbose=FALSE)
{
    ahroot <- "~/s/data/public/encode";
    eip <- EncodeImportPreparer(ahroot)
    checkEquals(nrow(metadataTable(eip)), 0)

    downloadDir <- file.path(ahroot, "downloads")
    if(!file.exists(downloadDir))
         dir.create(downloadDir)
    
    AnnotationHubData:::.retrieveEncodeDCCMetadataFiles(downloadDir, max=NA,
                                                        verbose=verbose)
    printf("downloaded %d files from encodeDCC to %s", length(list.files(downloadDir)),
               downloadDir)
    
   dataFile.summary <- AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir, verbose=verbose)
   metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
   full.path <- file.path(downloadDir, metadata.files)
   tbl.md <- parseMetadataFiles(eip, full.path, dataFile.summary,verbose=verbose)

   serializedFileName <- file.path(ahroot, "tbl.parsedEncodeMetadata.RData")

   save(tbl.md, file=serializedFileName)
   invisible(tbl.md)

    
} # import.md
#-------------------------------------------------------------------------------
# 7 minutes
to.ahmd <- function()
{
    ahroot <- "~/s/data/public/encode";
    eip <- EncodeImportPreparer(ahroot)
    zz <- encodeMetadataToAnnotationHubMetadata(eip, subset=NA, verbose=FALSE)
    zz
    
} # to.ahmd
#-------------------------------------------------------------------------------
