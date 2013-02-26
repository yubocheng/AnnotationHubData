.AnnotationHubMetadata_args <- local({
    ## local copy of AnnotationHub, and arg list for AHMetadata; singleton
    sourceDirectory <-
        system.file('extdata', package='AnnotationHubData')
    ahroot <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    basepath <- paste0("goldenpath/hg19/encodeDCC/wgEncodeRikenCage/",
                       "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements")

    list(AnnotationHubRoot=ahroot,
        SourceFile=basepath,
        SourceUrl=sprintf("http://hgdownload.cse.ucsc.edu/%s", basepath),
        SourceVersion=NA_character_,
        Title="CD20 CAGE defined Transcriptional Start Sites",
        Description="120785 TSS sites ...",
        Species="Homo sapiens", Genome="hg19",
        Recipe="extendedBedToGranges", Tags="gene regulation",
        RDataClass="GRanges", RDataVersion=numeric_version("0.0.1"),
        Coordinate_1_based=TRUE,
        Maintainer="Paul Shannon <pshannon@fhcrc.org>",
        DataProvider="hgdownload.cse.ucsc.edu",
        Notes="9 total columns...", RDataDateAdded=as.POSIXct("2013-01-01"))
})

.AnnotationHubMetadata <- 
    ## singleton AHMetadata class instance
    do.call("AnnotationHubMetadata", .AnnotationHubMetadata_args)

test_constructor <- function()
{
    ## null constructor
    checkTrue(validObject(new("AnnotationHubMetadata")))

    ## construction from complete args
    args <- .AnnotationHubMetadata_args
    ahm <- do.call("AnnotationHubMetadata", args)
    checkTrue(validObject(ahm))
    ## ... correctly inserted into slots
    values <- metadata(ahm)
    test <- unlist(Map(identical, args, values[names(args)]))
    checkTrue(all(test))

    ## date / version coercion
    idx <- grep("(Version|Date)", names(args))
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
    args$SourceFile <- file.path(rp, files)
    args$SourceUrl <-
        sprintf("http://hgdownload.cse.ucsc.edu/%s/%s", rp, files)

    x <- do.call("AnnotationHubMetadata", args)
    checkEquals(2L, length(metadata(x)$SourceUrl))
    checkEquals(2L, length(metadata(x)$SourceMd5))
    checkEquals(2L, length(metadata(x)$SourceSize))
    checkEquals(2L, length(metadata(x)$SourceFile))
}

test_fromJson <- function()
{
    ahroot <- system.file('extdata', package='AnnotationHubData')
    jsonPath <-
        file.path(ahroot, "goldenpath/hg19/encodeDCC/wgEncodeRikenCage",
                  "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json")
    ahm <- fromJson(jsonPath)
    checkTrue(validObject(ahm))
}

test_toJson <- function()
{
    ahm <- .AnnotationHubMetadata
    path <- tempfile()
    cat(toJson(ahm), file=path)
    obs <- fromJson(path, metadata(.AnnotationHubMetadata)$AnnotationHubRoot)
    checkIdentical(ahm, obs)
}

test_post_processing <- function()
{
    path <- jsonPath(.AnnotationHubMetadata)
    ahroot <- metadata(.AnnotationHubMetadata)$AnnotationHubRoot
    md <- fromJson(path, ahroot)

    ## now create a Recipe instance
    recipe <- AnnotationHubRecipe(md)

    ## create GRanges from the extended bed file, save as RData where
    ## instructed by the recipe
    pathToRDataFile <- run(recipe)

    ## reload the metadata
    md2 <- fromJson(path)

    info <- file.info(pathToRDataFile)
    checkEquals(as.integer(info$size), metadata(md2)$RDataSize)

    difft <- info$mtime - metadata(md2)$RDataLastModifiedDate
    checkEqualsNumeric(0, difft, tolerance=1) # rouding to 1s diff
}
