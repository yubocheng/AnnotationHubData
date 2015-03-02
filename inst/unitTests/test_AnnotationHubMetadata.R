## Many of the tests in here (and other files) are from before a major
## refactor.  Many of them are therefore likely to be meaningless in
## the new context, but others will need to be replaced with
## modernized tests.

## Here we will try to put tests that make sure that the constructor
## is behaving itself.

.AnnotationHubMetadata_args <- local({
    ## local copy of AnnotationHub, and arg list for AHMetadata; singleton
    sourceDirectory <-
        system.file('extdata', package='AnnotationHubData')
    ahroot <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    ##ahroot <- tempdir()
    basepath <- paste0("goldenpath/hg19/encodeDCC/wgEncodeRikenCage/",
                       "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements")
    
    list(AnnotationHubRoot=ahroot,
         SourceUrl=sprintf("http://hgdownload.cse.ucsc.edu/%s", basepath),
         SourceVersion=NA_character_,
         SourceLastModifiedDate=as.POSIXct("2015-01-01", tz="GMT"),
         SourceSize=as.numeric(99999),
         SourceMd5="2",
         SourceType="BED",
         Title="CD20 CAGE defined Transcriptional Start Sites",
         Description="120785 TSS sites ...",
         Species="Homo sapiens",
         TaxonomyId=9606L,
         Genome="hg19",
         Recipe="extendedBedToGranges",
         Tags=c("gene regulation", "ranged genomic data"),
         RDataClass="GRanges",
         Coordinate_1_based=TRUE,
         Maintainer="Paul Shannon <pshannon@fhcrc.org>",
         DataProvider="hgdownload.cse.ucsc.edu",
         Notes="9 total columns...",
         RDataDateAdded=as.POSIXct("2013-01-01", tz="GMT"),
         DispatchClass="GRanges",
         PreparerClass="EncodeImportPreparer")
})

.AnnotationHubMetadata <- 
    ## singleton AHMetadata class instance
    do.call("AnnotationHubMetadata", .AnnotationHubMetadata_args)

test_constructor <- function()
{
    ## null constructor (remove this - IOW we don't intend to support 'new')
    ## checkTrue(validObject(new("AnnotationHubMetadata")))

    ## construction from complete args
    args <- .AnnotationHubMetadata_args
    ahm <- do.call("AnnotationHubMetadata", args)
    checkTrue(validObject(ahm))
    ## ... correctly inserted into slots
    values <- metadata(ahm)
    test <- unlist(Map(identical, args, values[names(args)]))
    checkTrue(all(test))

    ## date / version coercion
    idx <- grep("(Version)", names(args))
    args[idx] <- sapply(args[idx], as.character)

    ahm1 <- do.call("AnnotationHubMetadata", args)
    checkIdentical(ahm, ahm1)
}




test_isComplete <- function()
{
    .isComplete <- AnnotationHubData:::.isComplete
    valid <- .AnnotationHubMetadata
    checkTrue(.isComplete(valid))

    ## zero-length 'required' field
    invalid <- valid
    metadata(invalid)$Title <- character()
    checkException(.isComplete(invalid), silent=TRUE)

    ## invalid email address
    invalid <- valid
    metadata(invalid)$Maintainer <- "User <user at fhcrc.org>"
    checkException(.isComplete(invalid), silent=TRUE)

    ## species not in database
    invalid <- valid
    metadata(invalid)$Species <- "Unknown"
    checkException(.isComplete(invalid), silent=TRUE)
}

test_multi_input <- function()
{
    args <- .AnnotationHubMetadata_args
    rp <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    files <- c("wgEncodeRegDnaseClustered.bed.gz",
               "wgEncodeRegDnaseClusteredInputs.tab")
    args$SourceUrl <-
        sprintf("http://hgdownload.cse.ucsc.edu/%s/%s", rp, files)
    args$SourceMd5 <- c("2","2")
        args$SourceSize <- c(as.numeric(99999),as.numeric(99999))

    x <- do.call("AnnotationHubMetadata", args)
    checkEquals(2L, length(metadata(x)$SourceUrl))
    checkEquals(2L, length(metadata(x)$SourceMd5))
    checkEquals(2L, length(metadata(x)$SourceSize))
}

## test_json_prerequisites <- function()
## {
##     ## handling NA's
##     ## we'll opt for c(x=NA) --> list(x="NA") on round trip
##     checkTrue(grepl("rjson", packageDescription("AnnotationHubData")$Imports))
##     obs <- rjson::fromJSON(rjson::toJSON(c(x=NA)))
##     checkIdentical(list(x="NA"), obs)

##     ## encoding NA as .__NA__<class>_
##     encode <- AnnotationHubData:::.encodeNA
##     decode <- AnnotationHubData:::.decodeNA

##     nas <- list(NA, NA_integer_, NA_character_, NA_real_, NA_complex_)
##     checkIdentical(nas, decode(encode(nas)))

##     nas <- list(x=c(1, NA), y =c("a", NA))
##     checkIdentical(nas, decode(encode(nas)))
## }

## ## FIXME - should also test a file containing more than one record,
## ## since this tests a different code path (which seems broken!).
## test_AnnotationHubMetadataFromJson <- function()
## {
##     ahroot <- system.file('extdata', package='AnnotationHubData')
##     jsonPath <-
##         file.path(ahroot, "goldenpath/hg19/encodeDCC/wgEncodeRikenCage",
##                   "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json")
##     ahm <- AnnotationHubMetadataFromJson(jsonPath)
##     checkTrue(validObject(ahm))
## }

## test_toJson <- function()
## {
##     ahm <- .AnnotationHubMetadata
##     path <- tempfile()
##     cat(toJson(ahm), file=path)
##     ahroot <- metadata(.AnnotationHubMetadata)$AnnotationHubRoot
##     obs <- AnnotationHubMetadataFromJson(path, ahroot)
##     checkIdentical(ahm, obs)
## }

## test_post_processing <- function()
## {
##     path <- jsonPath(.AnnotationHubMetadata)
##     ahroot <- metadata(.AnnotationHubMetadata)$AnnotationHubRoot
##     md <- AnnotationHubMetadataFromJson(path, ahroot)

##     ## now create a Recipe instance
##     recipe <- AnnotationHubRecipe(md)

##     ## create GRanges from the extended bed file, save as RData where
##     ## instructed by the recipe
##     postProcessedMd <- run(recipe)
##     pathToRDataFile <- file.path(ahroot, metadata(postProcessedMd)$RDataPath)

##     info <- file.info(pathToRDataFile)
##     checkEquals(info$size, metadata(postProcessedMd)$RDataSize)

##     difft <- info$mtime - metadata(postProcessedMd)$RDataLastModifiedDate
##     checkEqualsNumeric(0, difft, tolerance=1) # rouding to 1s diff
## }












################################################################################
## Tests to just see if we can run all of our recipes

.recipeSetup <- function(){
    require(RUnit)              ## just for convenience
    require(AnnotationHubData)  ## just for convenience
    ahroot <- "/var/FastRWeb/web"
    BiocVersion <- c("3.1")      
}

test_HaemCodeImportPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
      preparerClasses = "HaemCodeImportPreparer",
      insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)    
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_BioPaxImportPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
      preparerClasses = "BioPaxImportPreparer",
      insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_UCSCChainPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "UCSCChainPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}
    
test_UCSC2BitPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "UCSC2BitPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_EncodeImportPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "EncodeImportPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}


test_dbSNPVCFPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "dbSNPVCFPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}
    
test_RefNetImportPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "RefNetImportPreparer",
        insert = FALSE, metadataOnly=TRUE)
     checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_PazarImportPreparer_recipe <- function() {
    .recipeSetup()
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "PazarImportPreparer",
        insert = FALSE, metadataOnly=TRUE)
     checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}



