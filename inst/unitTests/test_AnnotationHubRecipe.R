library(AnnotationHubData)
library(rjson)
library(rtracklayer)
library(RUnit)
#-------------------------------------------------------------------------------
test_.createWorkingDirectory <- function()
{
    print ("--- test_.createWorkingDirectory")
    sourceDirectory <- system.file('extdata',
                                    package='AnnotationHubData')
    
    originalFiles <- sort(list.files(sourceDirectory, recursive=TRUE))
  
       # recursive list.files ends up at the bottom of the extdata/goldenpath/hg19/...
       # path, returning only the 3 (or more) files found there
       # PKG-ROOT/extdata/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/
    checkTrue(length(originalFiles) >= 3)
    
    newDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    movedFiles <- sort(list.files(newDirectory, recursive=TRUE))
    checkTrue(length(match(originalFiles, movedFiles)) == length (originalFiles))

} # test_.createWorkingDirectory
#-------------------------------------------------------------------------------
test_simpleConstructor <- function()
{
    print ("--- test_simpleConstructor")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <-
        AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)

    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(metadata(md)$Recipe, "extendedBedToGRanges")
    checkEquals(recipeName(recipe), "extendedBedToGRanges")
    checkTrue(file.exists(inputFiles(recipe)))

       # the output file has the same path and name as the 'main' (and often only)
       # input file, with '.RData' added to it remove that suffix, then compare it
       # to the full path to that input file, aka 'RDataPath'
    checkEquals(file.path(metadata(md)$AnnotationHubRoot,
                metadata(md)$RDataPath),
                outputFile(recipe))
    TRUE

} # test_simpleConstructor
#-------------------------------------------------------------------------------
# the 'nullRecipe' simply copies the specified input file to the specified
# output file with no transformations on the contents.
test_nullRecipe <- function()
{
    print ("--- test_nullRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    metadata(md)$Recipe <- "nullRecipe"
    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(metadata(md)$Recipe, "nullRecipe")
    checkEquals(recipeName(recipe), "nullRecipe")
    checkTrue(file.exists(inputFiles(recipe)[1]))
    #run(recipe)
    #checkTrue(file.exists(outputFile(recipe)))
    run(recipe)
    checkTrue(file.exists(outputFile(recipe)))
    #runWild(recipe)

} # test_nullRecipe
#-------------------------------------------------------------------------------
# demonstrate and test the use of a locally defined function, one which does
# no transformation of data, just reports the number of characters in the
# name of the recipe's input data file
test_adhocRecipe <- function()
{
    print ("--- test_adhocRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    adhoc <- function(recipe) {
        result <- nchar(inputFiles(recipe)[1])
        save(result, file=outputFile(recipe))
        result
        }
    
    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))

       # adhoc  does character count on the input file specified
       # in the json file.  will change on each run due to the
       # random tmp directory name.  check for identical counts
    checkEquals(run(recipe, adhoc), nchar(inputFiles(recipe)[1]))
       # every recipe should save its results to the specified 
       # output file.  check this
    if(exists('result')) rm(result)
    load(outputFile(recipe))
    checkEquals(result, nchar(inputFiles(recipe)[1]))

} # test_adhocRecipe
#-------------------------------------------------------------------------------
test_constructSeqInfo <- function()
{
    species <- "Homo sapiens"
    genome <- "hg19"
  
    si.hg19 <- constructSeqInfo(species, genome)
    expected.names <- paste("chr", c(1:22,"X","Y","M"), sep="")
    checkEquals(names(si.hg19), expected.names)
    checkEquals(unique(genome(si.hg19)), genome)
    min.max <- fivenum(as.integer(seqlengths(si.hg19)))[c(1,5)]
    checkTrue(min.max[1] < 17000)     # chrM
    checkTrue(min.max[2] > 24000000)  # chr1
  
    genome <- "hg18"
    si.hg18 <- constructSeqInfo(species, genome)

        # chr17 grew in hg19
    checkTrue(seqlengths(si.hg18['chr17']) < seqlengths(si.hg19['chr17']))

        # make sure we can subset the seqinfo when the GRanges for which it
        # is intended has fewer chromosomes than 25

    
} # test_constructSeqInfo
#-------------------------------------------------------------------------------
test_extendedBedToGRanges <- function()
{
    print ("--- test_extendedBedToGRanges")

        # copy the source data to a writable temporary directory

    sourceDirectory <- system.file("extdata", package="AnnotationHubData")

    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage"
    jsonPath <- file.path(resourcePath, jsonFile)

    checkTrue(file.exists(file.path(annotationHubRoot, jsonPath)))

        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot,
                                                     jsonPath)

        # now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)

    checkEquals(recipeName(recipe), "extendedBedToGRanges")

        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)

        # check the result
    load(pathToRDataFile)

    checkEquals(length(gr), 50)
    checkEquals(names(mcols(gr)), c("level", "significance"))
    checkEquals(start(gr[9]), 54704676)
    checkEquals(end(gr[9]),   54704735)
    checkEquals(width(gr[9]), 60)

    checkSeqInfo(gr)

} # test_extendedBedToGRanges
#-------------------------------------------------------------------------------
test_extendedBedWithAuxiliaryTableToGRanges <- function()
{
    print ("--- test_extendedBedWithAuxiliaryTableToGRanges")

        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")
    workingDirectory <-
        AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <-
       "wgEncodeRegDnaseClustered.bed-wgEncodeRegDnaseClusteredInputs.tab_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    jsonPath <- file.path(resourcePath, jsonFile)

        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    recipe <- AnnotationHubRecipe(md)
    RDataFilename <- run(recipe)
    checkEquals(RDataFilename, outputFile(recipe))
    loadedDataName <- load(RDataFilename)
    checkEquals(loadedDataName, 'gr')
    checkEquals(length(gr), 100)
    checkEquals(dim(mcols(gr)), c(100,8))
    checkEquals(colnames(mcols(gr)), c("experimentID", "score", "track", "cellType",
                                       "treatment", "replicate", "source", "date"))
      # hand-check one range, extracted from our sample data
      # --- from the bed file
      #    seqname     start       end experimentID score
      # 71   chr13  95923360  95924150          118  1000
      # --- from auxiliary data
      #                             track        cellType treatment replicate source       date rowIndex
      # 118 wgEncodeUwDnaseMonocd14PkRep1 Monocytes-CD14+      None         1     UW 2011-10-10      118

    x <- gr[start(gr)==95923360]
    checkEquals(end(x), 95924150)
    checkEquals(as.character(seqnames(x)), "chr13")
    z <- as.list(mcols(x))
    checkEquals(z$experimentID, 118L)
    checkEquals(z$score, 1000)
    checkEquals(z$track, "wgEncodeUwDnaseMonocd14PkRep1")
    checkEquals(z$cellType, "Monocytes-CD14+")
    checkEquals(z$treatment, "None")
    checkEquals(z$replicate, 1L)
    checkEquals(z$source, "UW")
    checkEquals(z$date, "2011-10-10")

    checkSeqInfo(gr)

} # test_extendedBedWithAuxiliaryTableToGRanges
#-------------------------------------------------------------------------------
## test_trackWithAuxiliaryTablesToGRanges <- function()
## {
##     DEACTIVATED()
##     print ("--- test_trackWithAuxiliaryTablesToGRanges")
##         # copy the source data to a writable temporary directory
##     sourceDirectory <- system.file("extdata", package="AnnotationHubData")
##     workingDirectory <-
##         AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
##     annotationHubRoot <- workingDirectory

##         # locate the json metadata file
##     jsonFile <-
##        "oreganno.txt-oregannoAttr.txt-oregannoLink.txt_0.0.1.json"
##     resourcePath <- "goldenpath/hg19/database/oreganno"
##     jsonPath <- file.path(resourcePath, jsonFile)

##         # create a metadata object from this file
##     md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)    
##     recipe <- AnnotationHubRecipe(md)

##         # run the recipe
##     RDataFilename <- run(recipe)
##         ## the recipe run is one in trackWithAuxiliaryTablesToGRanges
    
##     checkEquals(RDataFilename, outputFile(recipe))
##     loadedDataName <- load(RDataFilename)
##     checkEquals(loadedDataName, 'gr')
##     checkEquals(length(gr), 74084)
##     checkEquals(dim(mcols(gr)), c(74084,8))
##     checkEquals(colnames(mcols(gr)), c("id", "index", "name", "attribute.x",
##                                        "attVal", "attribute.y", "raKey",
##                                        "attAcc"))
##         ## test to make sure contents are accurate.
##     x <- gr[start(gr)==873498]
##     checkEquals(end(x)[1], 873849)
##     checkEquals(as.character(seqnames(x))[1], "chr1")
##     z <- as.list(mcols(x[1]))
##     checkEquals(z$id, "OREG0012989")
##     checkEquals(z$index, 591)    
##     checkEquals(z$name, "OREG0012989")
##     checkTrue(is.na(z$attribute.x))
##     checkTrue(is.na(z$attVal))
##     checkEquals(z$attribute.y, "SrcLink")
##     checkEquals(z$raKey, "ORegAnno")
##     checkEquals(z$attAcc, "OREG0012989")

##     checkSeqInfo(gr)

## } # test_extendedBedWithAuxiliaryTableToGRanges
## #-------------------------------------------------------------------------------
## test_trackandTablesToGRangesRecipe <- function()
## {
##     DEACTIVATED()
##     ## TODO: modify this so that we can do the mcols as compressed
##     ## character lists (right now the ranges are getting duplicated
##     ## which is not cool.
    
##     print ("--- test_trackandTablesToGRangesRecipe")
##     ##   require(AnnotationHubData)    
##     ##   debug(AnnotationHubData:::.UCSCTrackMetadata)
    
##     ## Lets begin by just making all the AHMs (that takes WAY too long BTW)
##     ahms <- newResources(new("UCSCFullTrackImportPreparer"),
##                          numberGenomesToProcess=1)

##     ## Then grab the one for oreganno        
##     oregAHM <- ahms[[86]]

##     ## now assign AHMRoot path
##     metadata(oregAHM)$AnnotationHubRoot <- tempdir()    
##     workingDirectory  = file.path(metadata(oregAHM)$AnnotationHubRoot,
##       dirname(metadata(oregAHM)$RDataPath))
##     dir.create(workingDirectory, recursive=TRUE)
##     checkTrue(file.exists(workingDirectory))

##     ## now spawn a recipe
##     recipe <- AnnotationHubRecipe(oregAHM)

##     ## debug(AnnotationHubData:::trackandTablesToGRangesRecipe)
##     ## debug(AnnotationHubData:::.compressTable)
##     RDataFilename <- run(recipe)  

    
##     checkEquals(RDataFilename, outputFile(recipe))
##     loadedDataName <- load(RDataFilename)
##     checkEquals(loadedDataName, 'gr')
##     checkEquals(length(gr), 23113)
##     checkEquals(dim(mcols(gr)), c(23113,8))
##     checkEquals(colnames(mcols(gr)), c("bin", "id", "name", "attribute",
##                                        "attrVal", "attribute.1", "raKey",
##                                        "attrAcc"))
##         ## test to make sure contents are accurate.
##     x <- gr[start(gr)==873498]
##     checkEquals(end(x)[1], 873849)
##     checkEquals(as.character(seqnames(x))[1], "chr1")
##     z <- as.list(mcols(x[1]))
##     checkEquals(as.character(z$id),"OREG0012989")
##     checkEquals(as.integer(z$bin), 591)    
##     checkEquals(as.character(z$name), "OREG0012989")
##     checkEquals(as.character(unlist(z$attribute))[1],"type" )
##     checkEquals(as.character(unlist(z$attrVal))[1], "REGULATORY REGION")
##     checkEquals(as.character(unlist(z$attribute.1))[1], "SrcLink")
##     checkEquals(as.character(unlist(z$raKey))[1], "ORegAnno")
##     checkEquals(as.character(unlist(z$attrAcc))[1], "OREG0012975")

##     checkSeqInfo(gr)

## } # test_trackandTablesToGRangesRecipe
#-------------------------------------------------------------------------------
test_trackToGRangesRecipe <- function()
{
    print ("--- test_trackToGRanges")
    ##   require(AnnotationHubData);library(RUnit)

    ##   debug(AnnotationHubData:::.UCSCTrackMetadata)
    
    ## Lets begin by just making all the AHMs (that takes WAY too long BTW)
    ahms <- newResources(new("UCSCTrackImportPreparer"),
                         numberGenomesToProcess=1)

    ## Then grab the one for oreganno        
    oregAHM <- ahms[[86]]
    ## now assign AHMRoot path
    metadata(oregAHM)$AnnotationHubRoot <- tempdir()    
    workingDirectory  = file.path(metadata(oregAHM)$AnnotationHubRoot,
      dirname(metadata(oregAHM)$RDataPath))
    dir.create(workingDirectory, recursive=TRUE)
    checkTrue(file.exists(workingDirectory))
    
    ## now spawn a recipe
    recipe <- AnnotationHubRecipe(oregAHM)

    ## debug(AnnotationHubData:::trackToGRangesRecipe)
    RDataFilename <- run(recipe)  
    
##     checkEquals(RDataFilename@RDataPath, outputFile(recipe))
    loadedDataName <- load(outputFile(recipe))
    checkEquals(loadedDataName, 'gr')
    checkEquals(length(gr), 23118)
    checkEquals(dim(mcols(gr)), c(23118,2))
    checkEquals(colnames(mcols(gr)), c("name", "score"))
    ## test to make sure contents are accurate.
    x <- gr[start(gr)==886938]
    checkEquals(end(x)[1], 886958)
    checkEquals(as.character(seqnames(x))[1], "chr1")
    z <- as.list(mcols(x[1]))
    checkEquals(z$name, "OREG0007909")
    checkEquals(z$score, 0)    

    checkSeqInfo(gr)

} # test_trackToGRangesRecipe
#-------------------------------------------------------------------------------
test_ensemblGtfToGRanges <- function()
{
    print('--- test_ensemblGtfToGRanges')
        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")
    workingDirectory <-
      AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "Homo_sapiens.GRCh37.69.gtf_0.0.1.json"
    resourcePath <- "pub/release-69/gtf/homo_sapiens"
    jsonPath <- file.path(resourcePath, jsonFile)
    checkTrue(file.exists(file.path(annotationHubRoot,jsonPath)))

        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    recipe <- AnnotationHubRecipe(md)
    RDataFilename <- run(recipe)
    checkEquals(RDataFilename, outputFile(recipe))
    checkTrue(file.exists(RDataFilename))
    loadedDataName <- load(RDataFilename)
    checkTrue(is(eval(parse(text=loadedDataName)), "GRanges"))
    checkEquals(length(gr), 50)
    checkEquals(dim(mcols(gr)), c(50,12))
    checkEquals(sort(colnames(mcols(gr))),
                c("exon_id", "exon_number", "gene_biotype", "gene_id",
                  "gene_name", "phase", "protein_id","score", "source",
                  "transcript_id", "transcript_name", "type"))
       # check one record closely, chosen at randome
    x <- gr[42]
    checkEquals(as.character(seqnames(x)), "HSCHR6_MHC_QBL")
    checkEquals(start(x), 31606931)
    checkEquals(end(x),   31606984)
    checkEquals(as.character(strand(gr[42])), '-')
    y <- as.list(mcols(gr[42]))
    checkEquals(as.character(y$source), "protein_coding")
    checkEquals(as.character(y$type), "exon")
    checkTrue(is.na(y$score))
    checkTrue(is.na(y$phase))
    checkEquals(y$gene_id, "ENSG00000096155")
    checkEquals(y$transcript_id, "ENST00000432539")
    checkEqualsNumeric(y$exon_number, 5)
    checkEquals(y$gene_name, "BAG6")
    checkEquals(y$gene_biotype, "protein_coding")
    checkEquals(y$transcript_name ,"BAG6-005")
    checkTrue(is.na(y$protein_id))
    checkEquals(y$exon_id, "ENSE00002923654")


} # test_ensemblGtfToGRanges
#-------------------------------------------------------------------------------
test_broadPeakToGRanges <- function()
{
    print ("--- test_broadPeakToGRanges")

        # copy the source data to a writable temporary directory

    sourceDirectory <- system.file("extdata", package="AnnotationHubData")

    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "wgEncodeSunyAlbanyGeneStK562SlbpRbpAssocRnaV2.broadPeak_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt"
    jsonPath <- file.path(resourcePath, jsonFile)

    checkTrue(file.exists(file.path(annotationHubRoot, jsonPath)))
    
        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)

        # now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)

    checkEquals(recipeName(recipe), "extendedBedToGRanges")

        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)


        # check the result
    load(pathToRDataFile)

    checkEquals(length(gr), 25)
    checkEquals(names(mcols(gr)), c("name","score","signalValue","pValue","qValue"))

    checkSeqInfo(gr)

} # test_broadPeakToGRanges
#-------------------------------------------------------------------------------
test_narrowPeakToGRanges <- function()
{
    print ("--- test_narrowPeakToGRanges")

        # copy the source data to a writable temporary directory

    sourceDirectory <- system.file("extdata", package="AnnotationHubData")

    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "wgEncodeSydhTfbsK562Brf2StdPk.narrowPeak_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeSydhTfbs"
    jsonPath <- file.path(resourcePath, jsonFile)
    fullJsonPath <- file.path(annotationHubRoot, jsonPath)
    checkTrue(file.exists(fullJsonPath))
    
        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
        # now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)

    checkEquals(recipeName(recipe), "extendedBedToGRanges")

        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)


        # check the result
    load(pathToRDataFile)

    checkEquals(length(gr), 19)
    checkEquals(names(mcols(gr)), c("name","score","signalValue",
                                    "pValue","qValue", "peak"))
    checkSeqInfo(gr)


} # test_narrowPeakToGRanges
#-------------------------------------------------------------------------------
test_rtracklayerGenericImportOfEncodeGtf <- function ()
{

    print ("--- test_rtracklayerGenericImportOfEncodeGtf")
        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")

    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "wgEncodeCshlLongRnaSeqHmecCellPamGeneDeNovo.gtf_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeCshlLongRnaSeq"
    jsonPath <- file.path(resourcePath, jsonFile)
    fullJsonPath <- file.path(annotationHubRoot, jsonPath)
    checkTrue(file.exists(fullJsonPath))
    
        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
        # now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)

    checkEquals(recipeName(recipe), "rtrackLayerImport")

        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)

        # check the result
    load(pathToRDataFile)

    checkEquals(length(gr), 8556)
    checkEquals(names(mcols(gr)), c("source","type","score","phase","gene_id",
                                     "IDR", "FPKM1", "FPKM2"))
    checkSeqInfo(gr)

    TRUE

} # test_rtracklayerGenericImportOfEncodeGtf 
#-------------------------------------------------------------------------------
test_bedRnaElementsToGRanges <- function()
{
    print("--- test_bedRnaElementsToGRanges")

        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")

    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "wgEncodeCshlShortRnaSeqHepg2NucleusShorttotalTapContigs.bedRnaElements_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeCshlShortRnaSeq"
    jsonPath <- file.path(resourcePath, jsonFile)
    fullJsonPath <- file.path(annotationHubRoot, jsonPath)
    checkTrue(file.exists(fullJsonPath))
    
        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
        # now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)

    checkEquals(recipeName(recipe), "extendedBedToGRanges")

        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)

        # check the result
    load(pathToRDataFile)
    checkEquals(length(gr), 11342)
    checkEquals(names(mcols(gr)), c("name", "score", "level", "signif", "score2"))
    checkSeqInfo(gr)

} # test_bedRnaElementsToGRanges
#-------------------------------------------------------------------------------
checkSeqInfo <- function(gr)
{
       # a spot check to insure that seqinfo has been properly assigned
       # hg19 (21 jan 2013):       seqLengths  isCircular   genome
       #                 chr1      249250621        FALSE     hg19

    chr1.length <- seqlengths(seqinfo(gr)["chr1"])
    checkTrue(!is.na(chr1.length))
    return(checkTrue(chr1.length > 200000000))


} # checkSeqInfo
#-------------------------------------------------------------------------------
