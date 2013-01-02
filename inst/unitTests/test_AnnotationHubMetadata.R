library(AnnotationHubData)
library(RUnit)

.printf <- function(...) print(noquote(sprintf(...)))

runTests <- function()
{
    test_constructor()
    test_from_json()
    test_validity()
    test_multi_input()
    test_post_processing()
}





test_post_processing <- function()
{

        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")
    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

        # locate the json metadata file
    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage"
    jsonPath <- file.path(resourcePath, jsonFile)

        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(workingDirectory, jsonPath)

        # now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)


        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)


        # reload the metadata
    md2 <- constructMetadataFromJsonPath(workingDirectory, jsonPath)

    info <- file.info(pathToRDataFile)
    checkEquals(as.integer(info$size), metadata(md2)$RDataSize)

    checkEquals(strftime(info$mtime, "%Y-%m-%d"),
        strftime(metadata(md2)$RDataLastModifiedDate, "%Y-%m-%d"))
}



test_constructor <- function()
{
    sourceDirectory <- system.file('extdata', 
          package='AnnotationHubData')
    ahroot <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    ahm <- AnnotationHubMetadata(ahroot,
        "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements",
        SourceUrl="http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements",
        Title="CD20 CAGE defined Transcriptional Start Sites",
        Description="120785  TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD20 cells, from Timo Lassmann",
        Species="Homo sapiens", Genome="hg19", Recipe="extendedBedToGranges", Tags="gene regulation",
        RDataClass="GRanges",
        RDataVersion="0.0.1", SourceVersion="unknown", Coordinate_1_based=TRUE,
        Maintainer="Paul Shannon <pshannon@fhcrc.org>",
        DataProvider="hgdownload.cse.ucsc.edu",
        Notes="9 total columns in the bed file, 8 of which are presented here ('empty' is omitted)  (a) Chromosome   (b) Start  (c) End  (d) (coordinates):(paraclu cluster strength):(TSS prediction strength)  (e) empty   (f) Strand  (g) level - expression level in tpm  (h) signif - currently empty - will be IDR  (i) score2 - raw number of reads   wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.gz project=wgEncode; grant=Gingeras; lab=RIKEN; composite=wgEncodeRikenCage; dataType=Cage; view=TssHmm; cell=CD20+; localization=cell; rnaExtract=longPolyA; readType=1x50; donorId=RO01794,RO01778; dataVersion=ENCODE Mar 2012 Freeze; dccAccession=wgEncodeEH002738; dateSubmitted=2012-03-30; dateUnrestricted=2012-12-30; subId=6744; geoSampleAccession=GSM979634; labExpId=CThi10023,CThi10024; bioRep=041WC,042WC; seqPlatform=Illumina_HiSeq_2000; tableName=wgEncodeRikenCageCd20CellPapTssHmm; type=bedRnaElements; md5sum=c69036e9a1bf0eb39d0b73687fc31ec1; size=2.5M",
        RDataDateAdded="2013-01-01 00:00:00"
)
    checkTrue(validObject(ahm))
    expected <- 
        "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.RData"
    ##.printf("expected == %s, actual == %s", expected, metadata(ahm)$RDataPath)
    checkEquals(expected, metadata(ahm)$RDataPath)

}



test_validity <- function()
{
    sourceDirectory <- system.file('extdata',
          package='AnnotationHubData')
    ahroot <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    l <- list()
    l$AnnotationHubRoot <- ahroot
    l$SourceFile <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements"
    l$SourceUrl <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements"
    l$Title <- "CD20 CAGE defined Transcriptional Start Sites"
    l$Description="120785  TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD20 cells, from Timo Lassmann"

    l$Species <- "Homo sapiens"
    l$Genome <- "hg19"
    l$Recipe <- "extendedBedToGranges"
    l$Tags <- "gene regulation"
    l$RDataClass <- "GRanges"
    l$RDataVersion <- "0.0.1"
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

test_multi_input <- function()
{

    sourceDirectory <- system.file('extdata', 
          package='AnnotationHubData')
    ahroot <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

    rp <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    files <- file.path(rp, c("wgEncodeRegDnaseClustered.bed.gz",
        "wgEncodeRegDnaseClusteredInputs.tab"))


    l <- list()
    l$AnnotationHubRoot <- ahroot
    l$SourceFile <- files
    l$Species <- "Homo sapiens"
    l$Genome <- "hg19"
    l$Recipe <- "unknown" # FIXME
    l$RecipeArgs <- list() # FIXME
    l$RDataClass <- "GRanges"
    l$RDataVersion <- "0.0.1"
    l$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    l$DataProvider <- "hgdownload.cse.ucsc.edu"
    l$Coordinate_1_based <- TRUE
    l$Title <- " ENCODE Composite DNaseI Hypersensitivity Regions"
    l$Description <- "999,988 DNaseI hypersensitivity regions, combined from 75 cell types"
    l$SourceUrl <- c("http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab.gz",
        "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClustered.bed.gz")
    l$SourceVersion <- "dateSubmitted=2011-04-28"
    l$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    l$Tags <- "gene regulation"
    l$RDataDateAdded = "2013-01-01 00:00:00"

    x <- do.call(AnnotationHubMetadata, l)
## don't postprocess because there is no derived file yet.    
##    x <- postProcessMetadata(ahroot, x@SourceFile)
    checkEquals(2L, length(x@SourceUrl))
    checkEquals(2L, length(x@SourceMd5))
    checkEquals(2L, length(x@SourceSize))
    checkEquals(2L, length(x@SourceFile))
}


test_from_json <- function()
{
    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    ahroot <- system.file('extdata', package='AnnotationHubData')

    ahm <- constructMetadataFromJsonPath(ahroot, jsonPath)
    checkTrue(validObject(ahm))
}

