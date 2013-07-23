    library(AnnotationHubData)
    library(RUnit)
    library(RCurl)
    library(httr)

#-------------------------------------------------------------------------------
paulsTests <- function()
{
    library(AnnotationHubData)
    library(RUnit)
    library(RCurl)
    library(httr)
    test_.extractLinksFromHtmlLines()
    test_.extractExperimentDirectoriesFromWebPage()

    test_.downloadFileInfo()
       # next test depends upon functions tested by preceeding two tests
    test_.retrieveEncodeDCCMetadataFiles()
    test_.learnAllEncodeMetadataCategories()
    test_.assignRecipeAndArgs()
    test_.convertSizeStringsToNumeric()
    test_.parseMetadataFiles()
    test_.encodeMetadataToAnnotationHubMetadata()

    test_missingTableNameInProvidedMetadata()
    
    test_ctor()
    test_newResources()

    admin_test_threeEncodeDirectories()
    admin_test_endToEndFourResources()
    

} # runTests
#-------------------------------------------------------------------------------
test_ctor <- function()
{
    print("--- test_ctor")
    
    checkTrue(validObject(new("EncodeImportPreparer")))
    checkTrue(validObject(EncodeImportPreparer(annotationHubRoot=tempdir(),
                                               maxForTesting=0)))
    checkException(!validObject(
          EncodeImportPreparer(annotationHubRoot="/bogus/fake",
                               maxForTesting=0)),
          silent=TRUE)

} # test_ctor
#-------------------------------------------------------------------------------
# ensure that, in simple web page crawling, we can extract
#   "wgEncodeAffyRnaChip/"
# from
#  <a href=\"wgEncodeAffyRnaChip/\">wgEncodeAffyRnaChip/</a>       05-Jul-2012 06:57    -   "    
test_.extractLinksFromHtmlLines <- function()
{
    print("--- test_.extractLinksFromHtmlLines")

    lines <- strsplit(RCurl::getURL(EncodeBaseURL()), "\n")[[1]]
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
    subdirs <- AnnotationHubData:::.extractExperimentDirectoriesFromWebPage(
                                       EncodeBaseURL())
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
    all.dirs <- AnnotationHubData:::.extractExperimentDirectoriesFromWebPage(
                                         EncodeBaseURL())
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
test_.retrieveEncodeDCCMetadataFiles <- function()
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
test_.parseMetadataFiles <- function()
{
    print('--- test_.parseMetadataFiles')

        # two truncated metadata files (6 lines only) are stored in the package
        # for testing: unitTests/cases/encodeDCCMetadata
        #       wgEncodeAffyRnaChip.info
        #   wgEncodeAwgDnaseUniform.info
        # in normal use of the EncodeImportPreparer, files like these are
        # downloaded from the ucsc encodeDCC web site, then parsed
        # hence "downloadDir"

    downloadDir <- system.file(package="AnnotationHubData",
                               "unitTests", "cases", "encodeDCCMetadata")
    dataFile.summary <-
         AnnotationHubData:::.learnAllEncodeMetadataCategories(downloadDir,
                                                               verbose=FALSE)
       # only files which end in .info:
    downloaded.metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
    checkEquals(length(downloaded.metadata.files), 2)

    full.paths <- file.path(downloadDir, downloaded.metadata.files)
    checkTrue(all(sapply(full.paths, file.exists)))
    tbl.md <- AnnotationHubData:::.parseMetadataFiles(full.paths,
                                                      dataFile.summary)
    checkEquals(dim(tbl.md), c(12,21))

        # "remoteDirectory" is a column we add explicitly
        # (in EncodeImportPreparer-class, parseMetadataFiles)
        # bassed upon the name of the "*.info" files, which
        # were renamed when downloaded, from files.txt to
        # <remoteDirectory>.info.   make sure this is a column
        # in the data.frame
    checkTrue("remoteDirectory" %in% colnames(tbl.md))
    checkEquals(sort(unique(tbl.md$composite)),
                c("wgEncodeAffyRnaChip","wgEncodeAwgDnaseUniPk"))

    tbl.md   # for possible use by other test functions
    

} # test_.parseMetadataFiles
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
# ask for a list of AnnotationHubMetadata items for 12 files, then check details of
# one of them.
# perturb filename #2 so that it names a file (a url) which does not exist.
# this should cause a list of 11 ahmd items to be returned
test_.encodeMetadataToAnnotationHubMetadata <- function()
{
    print("--- test_.encodeMetadataToAnnotationHubMetadata")

    tbl.md <- test_.parseMetadataFiles()

    checkEquals(dim(tbl.md), c(12,21))
        # test with just one entry
    ahRoot <- "/bogus/ahroot"
    rownames(tbl.md)[2] <- "bogusDoesNotExist.broadpeak.gz"

    ahmd.list <- AnnotationHubData:::.encodeMetadataToAnnotationHubMetadata(
                                          tbl.md,  ahRoot, verbose=FALSE)

    checkEquals(length(ahmd.list), 11)
    amd <- ahmd.list[[1]]

      ## check the easy fields, avoid the ones not assigned by us, or which may
      ## not be stable over time
    
    checkEquals(amd@AnnotationHubRoot, ahRoot)
      ## checkEquals(amd@BiocVersion, "")
    checkEquals(amd@Coordinate_1_based, TRUE)
    checkEquals(amd@DataProvider, "EncodeDCC")
      ## checkEquals(amd@DerivedMd5, "")
    checkEquals(amd@Description,
                "wgEncodeAffyRnaChipFiltTransfragsGm12878CellTotal")
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


    checkTrue(RCurl::url.exists(amd@SourceUrl))
    
} # test_.encodeMetadataToAnnotationHubMetadata
#-------------------------------------------------------------------------------
admin_test_threeEncodeDirectories <- function()
{
    print("--- admin_test_threeEncodeDirectories")
    ahroot <- tempdir()

    count <- 3
    eic <- EncodeImportPreparer(ahroot, verbose=FALSE, maxForTesting=count)
    checkTrue(length(metadataList(eic)) > count)   # 456 elements (mar 2013)
       # any non-existent urls, such as
       #    http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/\
       #  /wgEncodeAwgDnaseUniform/wgEncodeAwgDnaseDukeHuh7.5UniPk.narrowPeak.gz
       # cause the metadataList to be shorter than the metdataTable
       # allow for this, but check to make sure that it is relatively
       # rare
    checkTrue(nrow(metadataTable(eic))>= length(metadataList(eic)))
    checkTrue(length(metadataList(eic))/nrow(metadataTable(eic)) > 0.95)

        # "composite" is supplied by encodeDCC, and is usually the
        # name of the directory from which the files.txt was
        # was obtained.  but not always.  so we add
        # "remoteDirectory".  the counts of these two should be
        # the same

    composite <- unique(metadataTable(eic)$composite)
        # sometimes the encode directory and the files.txt are out
        # of sync, leading to a largely blank entry.  root those out
    chaff <- which(!nzchar(composite))
    if(length(chaff) > 0)
        composite <- composite[-chaff]
    
    checkEquals(length(composite), count)
    remoteDirectory <- unique(metadataTable(eic)$remoteDirectory)
    chaff <- which(!nzchar(remoteDirectory))
    if(length(chaff) > 0)
        remoteDirectory <- remoteDirectory[-chaff]

    checkEquals(length(remoteDirectory), count)

    urls <- sapply(metadataList(eic), function(elt) metadata(elt)$SourceUrl)
    checkTrue(length(urls) > 10)   # 456 on (7 mar 2013)
        # pick 5 urls at random
    urls <- urls[sample(1:length(urls), 5)]
    checkTrue(all(sapply(urls, function(url) RCurl::url.exists(url))))

} # admin_test_threeEncodeDirectories
#-------------------------------------------------------------------------------
test_newResources <- function()
{
    print("--- test_newResources")

    eip1 <- EncodeImportPreparer(tempdir(), maxForTesting=1)
    eip2 <- EncodeImportPreparer(tempdir(), maxForTesting=2)
    delta <- length(sourceUrls(eip2)) - length(sourceUrls(eip1))
    
    ahmd.delta <- newResources(eip2, metadataList(eip1))
    checkEquals(delta, length(ahmd.delta))
    

} # test_newResources
#-------------------------------------------------------------------------------
# takes about 2 minutes elapsed time
admin_test_endToEndFourResources <- function()
{
    print("--- test_endToEndFourResources")
    unlink(file.path(tempdir(), dir(tempdir())))
    eip4 <- EncodeImportPreparer(tempdir(), verbose=FALSE, maxForTesting=6)

         # two directories dropped because they contain only files
         # for which we have no recipes
    tbl.md <- metadataTable(eip4)
    checkEquals(length(unique(tbl.md$remoteDirectory)), 4)

    indices <- match(unique(tbl.md$remoteDirectory), tbl.md$remoteDirectory)
    tbl.md[indices, c("remoteDirectory", "size")]

    ahmds <- metadataList(eip4)[indices]
    ahRoot <- annotationHubRoot(eip4)

    for(ahmd in ahmds) {
        remote.file <- ahmd@SourceUrl
        checkTrue(RCurl::url.exists(remote.file))
        directory.path <- file.path (ahRoot, dirname(ahmd@SourceFile))
        if(!file.exists(directory.path))
            dir.create(directory.path, recursive=TRUE)
        local.file <- file.path(directory.path, basename(ahmd@SourceFile))
        download.file(remote.file, local.file, quiet=TRUE)
        checkTrue(file.exists(local.file))
        recipe <- AnnotationHubRecipe(ahmd)
        md <- run(recipe)
        serializedDataFileName <- file.path(md@AnnotationHubRoot, md@RDataPath)
        checkTrue(file.exists(serializedDataFileName))
        load(serializedDataFileName)
        #printf("%s: as gr: %d  as gzipped table: %d",
        #       basename(local.file), length(gr),
        #       nrow(read.table(local.file, sep="\t", header=FALSE)))
        #printf("mcols: %s", paste(colnames(mcols(gr)), collapse=" "))
        checkEquals(length(gr), nrow(read.table(local.file, sep="\t",
                                                header=FALSE)))
        } # for ahmd
    
} # admin_test_endToEndFourResources 
#-------------------------------------------------------------------------------
admin_test_runLargerSelection <- function()
{
    print("--- admin_test_runLargerSelection")

    unlink(file.path(tempdir(), dir(tempdir())))

        # takes about 40 seconds elapsed time
    eip <- EncodeImportPreparer(tempdir(), verbose=FALSE, maxForTesting=NA)
    ahRoot <- annotationHubRoot(eip)
    ahmds <- metadataList(eip)
    tbl.md <- metadataTable(eip)
    indices <- which(tbl.md$size<50000)
    ahmds <- ahmds[indices]

    #printf("total resources: %d", length(ahmds))
    max <- length(ahmds)
    
    for(i in 1:max) {
        ahmd <- ahmds[[i]]
        #printf("%s: %d", rownames(tbl.md)[i], tbl.md$size[i])
        remote.file <- ahmd@SourceUrl
        checkTrue(RCurl::url.exists(remote.file))
        directory.path <- file.path (ahRoot, dirname(ahmd@SourceFile))
        if(!file.exists(directory.path))
            dir.create(directory.path, recursive=TRUE)
        local.file <- file.path(directory.path, basename(ahmd@SourceFile))
        download.file(remote.file, local.file, quiet=TRUE)
        checkTrue(file.exists(local.file))
        recipe <- AnnotationHubRecipe(ahmd)
        md <- run(recipe)
        serializedDataFileName <- file.path(md@AnnotationHubRoot, md@RDataPath)
        checkTrue(file.exists(serializedDataFileName))
        load(serializedDataFileName)
        #printf("%s: as gr: %d  as gzipped table: %d",
        #       basename(local.file), length(gr),
        #       nrow(read.table(local.file, sep="\t", header=FALSE)))
        #printf("mcols: %s", paste(colnames(mcols(gr)), collapse=" "))
        checkEquals(length(gr), nrow(read.table(local.file, sep="\t",
                                                header=FALSE)))
        } # for i

} # admin_test_runLargerSelection
#-------------------------------------------------------------------------------
test_missingTableNameInProvidedMetadata <- function()
{
    print("--- test_missingTableNameInProvidedMetadata")
    full.path.dir <- system.file(package="AnnotationHubData", "extdata",
                                 "parseEncodeMetadataBug")
    stopifnot(file.exists(full.path.dir))
    full.path.file <- file.path(full.path.dir, "wgEncodeHaibRnaSeq.info")
    stopifnot(file.exists(full.path.file))
    dataFile.summary <-
       AnnotationHubData:::.learnAllEncodeMetadataCategories(full.path.dir,
                                                             verbose=FALSE)
    tbl.md <- AnnotationHubData:::.parseMetadataFiles(full.path.file,
                                                      dataFile.summary)

       # bam file metadata lines should be removed
       # todo: make this filtering part of .parseMetadataFiles
    
    supported.formats <- c("narrowPeak", "broadPeak", "bedRnaElements", "gtf")
    tbl.md <- tbl.md[tbl.md$type %in% supported.formats, ]
    checkTrue(!all(nzchar(tbl.md$tableName)))
    checkEquals(nrow(tbl.md), 2)
    ahmd.list <-
      AnnotationHubData:::.encodeMetadataToAnnotationHubMetadata(tbl.md,
                                                                "/bogus/ahroot",
                                                                 verbose=FALSE)

       # all (that is, both) entries should have a valid Description field
    checkTrue(all(nzchar(sapply(ahmd.list, function(ahmd) ahmd@Description))))

} # test_missingTableNameInProvidedMetadata
#-------------------------------------------------------------------------------
