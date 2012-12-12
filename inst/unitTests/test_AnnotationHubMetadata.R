library(AnnotationHubData)
library(RUnit)

runTests <- function()
{
    test_constructor()
    test_from_json()
    test_validity()
}



test_constructor <- function()
{
    subdir <- 'goldenpath'
    sourceDirectory <- system.file('extdata', subdir,
          package='AnnotationHubData')
    ahroot <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    ahm <- AnnotationHubMetadata(ahroot,
        "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements",
        Url="http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements",
        Title="CD20 CAGE defined Transcriptional Start Sites",
        Description="120785  TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD20 cells, from Timo Lassmann",
        Species="Homo sapiens", Genome="hg19", Recipe="extendedBedToGranges", Tags="gene regulation",
        ResourceClass="GRanges",
        Version="0.0.1", SourceVersion="unknown", Coordinate_1_based=TRUE,
        Maintainer="Paul Shannon <pshannon@fhcrc.org>",
        DataProvider="hgdownload.cse.ucsc.edu",
        Notes="9 total columns in the bed file, 8 of which are presented here ('empty' is omitted)  (a) Chromosome   (b) Start  (c) End  (d) (coordinates):(paraclu cluster strength):(TSS prediction strength)  (e) empty   (f) Strand  (g) level - expression level in tpm  (h) signif - currently empty - will be IDR  (i) score2 - raw number of reads   wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.gz project=wgEncode; grant=Gingeras; lab=RIKEN; composite=wgEncodeRikenCage; dataType=Cage; view=TssHmm; cell=CD20+; localization=cell; rnaExtract=longPolyA; readType=1x50; donorId=RO01794,RO01778; dataVersion=ENCODE Mar 2012 Freeze; dccAccession=wgEncodeEH002738; dateSubmitted=2012-03-30; dateUnrestricted=2012-12-30; subId=6744; geoSampleAccession=GSM979634; labExpId=CThi10023,CThi10024; bioRep=041WC,042WC; seqPlatform=Illumina_HiSeq_2000; tableName=wgEncodeRikenCageCd20CellPapTssHmm; type=bedRnaElements; md5sum=c69036e9a1bf0eb39d0b73687fc31ec1; size=2.5M")
    checkTrue(validObject(ahm))
}



test_validity <- function()
{
    subdir <- 'goldenpath'
    sourceDirectory <- system.file('extdata', subdir,
          package='AnnotationHubData')
    ahroot <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    l <- list()
    l$AnnotationHubRoot <- ahroot
    l$OriginalFile <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements"
    l$Url <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements"
    l$Title <- "CD20 CAGE defined Transcriptional Start Sites"
    l$Description="120785  TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD20 cells, from Timo Lassmann"

    l$Species <- "Homo sapiens"
    l$Genome <- "hg19"
    l$Recipe <- "extendedBedToGranges"
    l$Tags <- "gene regulation"
    l$ResourceClass <- "GRanges"
    l$Version <- "0.0.1"
    l$SourceVersion <- "unknown"
    l$Coordinate_1_based <- TRUE
    l$Maintainer = "Paul Shannon <pshannon at fhcrc.org>"
    l$DataProvider="hgdownload.cse.ucsc.edu"
    l$Notes <- "9 total columns in the bed file, 8 of which are presented here ('empty' is omitted)  (a) Chromosome   (b) Start  (c) End  (d) (coordinates):(paraclu cluster strength):(TSS prediction strength)  (e) empty   (f) Strand  (g) level - expression level in tpm  (h) signif - currently empty - will be IDR  (i) score2 - raw number of reads   wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.gz project=wgEncode; grant=Gingeras; lab=RIKEN; composite=wgEncodeRikenCage; dataType=Cage; view=TssHmm; cell=CD20+; localization=cell; rnaExtract=longPolyA; readType=1x50; donorId=RO01794,RO01778; dataVersion=ENCODE Mar 2012 Freeze; dccAccession=wgEncodeEH002738; dateSubmitted=2012-03-30; dateUnrestricted=2012-12-30; subId=6744; geoSampleAccession=GSM979634; labExpId=CThi10023,CThi10024; bioRep=041WC,042WC; seqPlatform=Illumina_HiSeq_2000; tableName=wgEncodeRikenCageCd20CellPapTssHmm; type=bedRnaElements; md5sum=c69036e9a1bf0eb39d0b73687fc31ec1; size=2.5M"
    checkException(do.call(AnnotationHubMetadata, l),
        "(1)validObject says invalid object is valid",
        silent=TRUE)
    l$Maintainer = "Paul Shannon <pshannon@hcrc.org>"
    l$Title <- NULL
    checkException(do.call(AnnotationHubMetadata, l),
        "(2)validObject says invalid object is valid",
        silent=TRUE)
    l$Title <- "placeholder"
    l$Species <- "My funny valentine"
    checkException(do.call(AnnotationHubMetadata, l),
        "(3)validObject says invalid object is valid",
        silent=TRUE)
}


test_from_json <- function()
{

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    ahroot <- system.file('extdata', package='AnnotationHubData')

    ahm <- constructAnnotationHubMetadataFromJsonPath(ahroot, jsonPath)
    checkTrue(validObject(ahm))
}

