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


################################################################################
## Tests to just see if we can run all of our recipes
## 
## VO deleted these on Aug 14 2015 because they were duplicates of 
## the tests in test_recipe.R
