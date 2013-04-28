## ------------------------------------------------------------------------------
## helper classes
## 

setOldClass(c("POSIXct", "POSIXt"))
setOldClass("numeric_version")
setOldClass(c("package_version", "numeric_version"))

.NA_version_ <- numeric_version("0.0")  # proxy for unknown version
.as.numeric_version <-
    function(x, ...)
{
    if (is(x, "character"))
        x[x == "unknown"] <- as.character(.NA_version_)
    base::as.numeric_version(x)
}

## ------------------------------------------------------------------------------
## Class defintion
## 
## The prototype needs to be fully specified, using 'NA' to indicate
## unknown, otherwise to / from JSON is confused

setClass("AnnotationHubMetadata",
    representation(
        AnnotationHubRoot="character",
        BiocVersion="package_version",
        Coordinate_1_based="logical",
        DataProvider="character",
        DerivedMd5="character",
        Description='character',
        Genome="character",
        Maintainer="character",
        Notes='character',
        RDataClass="character",
        RDataDateAdded="POSIXct",
        RDataLastModifiedDate="POSIXct",
        RDataPath="character",
        RDataSize="numeric",
        RDataVersion="numeric_version",
        Recipe="character",
        RecipeArgs="list",
        SourceFile="character",
        SourceLastModifiedDate="POSIXct",
        SourceMd5="character",
        SourceSize="numeric",
        SourceUrl="character",
        SourceVersion="character",
        Species="character",
        Tags='character',
        TaxonomyId="character",
        Title="character"
    ),
    prototype = prototype(
        AnnotationHubRoot=NA_character_,
        BiocVersion=biocVersion(),
        Coordinate_1_based=NA,
        DataProvider=NA_character_,
        DerivedMd5=NA_character_,
        Description=NA_character_,
        Genome=NA_character_,
        Maintainer=
            "Bioconductor Package Maintainer <maintainer@bioconductor.org>",
        Notes=NA_character_,
        RDataClass=NA_character_,
        RDataDateAdded=as.POSIXct(NA_character_),
        RDataLastModifiedDate=as.POSIXct(NA_character_),
        RDataPath=NA_character_,
        RDataSize=NA_real_,
        RDataVersion=.NA_version_,
        Recipe=NA_character_,
        RecipeArgs=list(),
        SourceFile=NA_character_,
        SourceLastModifiedDate=as.POSIXct(NA_character_),
        SourceMd5=NA_character_,
        SourceSize=NA_real_,
        SourceVersion=NA_character_,
        Species=NA_character_,
        Tags=NA_character_,
        TaxonomyId=NA_character_,
        Title=NA_character_
    )        
)

## ------------------------------------------------------------------------------
## constructor, validity, isComplete
## 

.derivedFileName <-
    function(originalFile, RDataVersion, suffix)
{
    ret <- sub(".gz$", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    sprintf("%s_%s.%s", ret, RDataVersion, suffix)
}

.taxonomyId <-
    function(species)
{
    if (!exists("speciesMap"))
        data(speciesMap, package="AnnotationHubData")
    species <- gsub(" {2,}", " ", species)
    species <- gsub(",", " ", species, fixed=TRUE)
    idx <- match(species, speciesMap$species)
    if (any(is.na(idx)))
        stop(sum(is.na(idx)), " unknown species: ",
             paste(sQuote(head(species[is.na(idx)])), collapse=" "))
    as.character(speciesMap$taxon[idx])
}

AnnotationHubMetadata <-
    function(AnnotationHubRoot, SourceFile, SourceUrl, SourceVersion,
        SourceMd5, SourceSize, DataProvider, Title, Description,
        Species, TaxonomyId, Genome, Tags, Recipe, RecipeArgs =
        list(), RDataClass, RDataVersion, RDataDateAdded, RDataPath, Maintainer,
        ..., BiocVersion=biocVersion(), Coordinate_1_based = TRUE,
        Notes=NA_character_)
{
    if (missing(SourceMd5))
        SourceMd5 <- unname(tools::md5sum(SourceFile))
    if (missing(SourceSize))
        SourceSize <- file.info(SourceFile)$size
    if (missing(TaxonomyId))
    {
        if (!is.na(Species))
            TaxonomyId <- .taxonomyId(Species)
        else
            TaxonomyId <- NA_character_
    }
    if (missing(RDataPath)) {
        resourceDir <- dirname(SourceFile[1])
        resourceFiles <- .derivedFileName(SourceFile,  RDataVersion, "RData")
        RDataPath <- file.path(resourceDir, resourceFiles)
    }

    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
        BiocVersion=biocVersion(),
        Coordinate_1_based=Coordinate_1_based,
        DataProvider=DataProvider,
        Description=Description,
        Genome=Genome,
        Maintainer=Maintainer,
        Notes=Notes,
        RDataClass=RDataClass,
        RDataDateAdded=as.POSIXct(RDataDateAdded),
        RDataPath=RDataPath,
        RDataVersion=numeric_version(RDataVersion),
        Recipe=Recipe,
        RecipeArgs=RecipeArgs,
        SourceFile=SourceFile,
        SourceMd5=SourceMd5,
        SourceSize=SourceSize,
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        ...
    )
}

.isComplete <-
    function(object)
{
    rc <- .Message()

    ## required fields must have non-zero length
    requiredFields <- c("AnnotationHubRoot", "SourceFile",
        "SourceUrl", "Title", "Species", "Genome", "Recipe", "Tags",
        "RDataClass", "RDataVersion", "SourceVersion",
        "Coordinate_1_based", "Maintainer", "DataProvider",
        "RDataDateAdded")
    values <- metadata(object)[requiredFields]
    idx <- sapply(values, length) == 0L
    if (any(idx))
        rc$append("slots(s) must have non-zero length: %s",
                  paste(sQuote(requiredFields[idx]), collapse=", "))

    ## look up species id in data table
    taxonomyId <- .taxonomyId(metadata(object)$Species)
    if (!length(taxonomyId))
        rc$append("'Species' unknown: %s", sQuote(metadata(object)$Species))

    ## valid e-mail address
    emailRegex <- 
        "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"
    if (!grepl(emailRegex, metadata(object)$Maintainer, ignore.case=TRUE))
        rc$append("'Maintainer' not a valid email address: %s",
          sQuote(metadata(object)$Maintainer))

    rc$isComplete()
}

## ------------------------------------------------------------------------------
## getters and setters
## 

setMethod("metadata", "AnnotationHubMetadata",
    function(x, ...) 
{
    nms <- slotNames(class(x))
    names(nms) <- nms
    lapply(nms, slot, object=x)
})

setReplaceMethod("metadata", c("AnnotationHubMetadata", "list"),
     function(x, ..., value)
{
    do.call(new, c(class(x), x, value))
})

## ------------------------------------------------------------------------------
## to / from Json
## 

jsonPath <-
    function(x)
{
    with(metadata(x), {
        fl <- sprintf("%s_%s.json", SourceFile, RDataVersion)
        file.path(AnnotationHubRoot, fl)
    })
}

.encodeNA <- function(lst)
{
    rapply(lst, function(elt) {
        isNA <- is.na(elt)
        if (any(isNA))
            ## NA token, coerce to 'character'
            elt[isNA] <- sprintf(".__NA__%s_", class(elt[isNA]))
        elt
    }, how="replace")
}

.decodeNA <- function(lst)
{
    .NAmap <- setNames(list(NA, NA_integer_, NA_character_, NA_real_,
                            NA_complex_),
                       c(".__NA__logical_", ".__NA__integer_",
                         ".__NA__character_", ".__NA__numeric_",
                         ".__NA__complex_"))
    rapply(lst, function(elt) {
        isNA <- elt %in% names(.NAmap)
        if (any(isNA)) {
            type <- elt[isNA][[1]]
            ## reverse coercion
            elt[isNA] <- NA
            elt <- as(elt, class(.NAmap[[type]]))
        }
        elt
    }, "character", how="replace")
}

toJson <-
    function(x)
{
    lst <- metadata(x)

    lst$AnnotationHubRoot <- NULL       # drop AHRoot

    idx <- grep("(RData|Bioc)Version", names(lst))  # version as character
    lst[idx] <- lapply(lst[idx], as.character)

    idx <- grep("Date", names(lst))
    lst[idx] <- lapply(lst[idx], format, "%Y-%m-%d %T UTC")

    ## encode NA to survive json via mangling
    toJSON(.encodeNA(lst))
}

AnnotationHubMetadataFromJson <-
    function(path, ahroot=NA_character_)
{
    lst <- .decodeNA(fromJSON(file=path))
    lst <- lst[!sapply(lst, is.null)]         # replace with default values

    ## coerce types
    lst[["RDataVersion"]] <- .as.numeric_version(lst[["RDataVersion"]])
    lst[["BiocVersion"]] <- package_version(lst[["BiocVersion"]])
    idx <- grep("Date", names(lst))
    lst[idx] <- rapply(lst[idx], function(x) {
        x[!nzchar(x)] <- NA_character_
        as.POSIXct(x)
    }, "character", how="list")

    idx <- sapply(lst, is, "AsIs")
    lst[idx] <- lapply(lst[idx], unclass)

    idx <- grep("Size", names(lst))
    lst[idx] <- rapply(lst[idx], as.numeric, how="list")

    ## create AnnotationHubMetadata object
    if (1L == length(lst$Title)) {
        ahm <- do.call(AnnotationHubMetadata, c(AnnotationHubRoot=ahroot, lst))
        ok <- .isComplete(ahm)
    } else {
        args <- c(list(AnnotationHubMetadata, AnnotationHubRoot=ahroot), lst)
        ahm <- do.call(Map, args)
        ok <- sapply(ahm, .isComplete)
    }
    if (!all(ok))
        stop("some AnnotationHubMetadata objects incomplete")
    ahm
}

writeJSON <- function(ahroot, metadata, flat=FALSE, filename=NULL)
{
    json <- toJson(metadata)
    sourceFile <- metadata(metadata)$SourceFile[1]
    resourceDir <- dirname(sourceFile)
    if (is.null(filename))
    {
        filename <- .derivedFileName(sourceFile,
            metadata(metadata)$RDataVersion, "json")
    }
    if (flat)
        outfile <- file.path(ahroot, filename)
    else
        outfile <- file.path(ahroot, resourceDir, filename)
    cat(json, "\n", file=outfile)
    outfile
}

constructAnnotationHubMetadataFromSourceFilePath <-
    function(ahroot, RDataVersion, originalFile)
{
    dir <- dirname(file.path(ahroot, originalFile))
    jsonFile <- .derivedFileName(originalFile, RDataVersion, "json")
    jsonFile <- file.path(dir[1], jsonFile)
    AnnotationHubMetadataFromJson(jsonFile, ahroot)
}

constructMetadataFromJsonPath <-
    function(ahroot, jsonpath)
{
    jsonFile <- file.path(ahroot, jsonpath)[1]
    AnnotationHubMetadataFromJson(jsonFile, ahroot)
}

.getExistingResources <-
    function(BiocVersion=biocVersion(), RDataDateAdded="2013-01-22")
{
    url <-
        sprintf("http://annotationhub.bioconductor.org/ah/%s/%s/query/cols/all",
                BiocVersion, RDataDateAdded)
    t <- tempfile()
    download.file(url, t, quiet=TRUE)
    AnnotationHubMetadataFromJson(t)
}

## ------------------------------------------------------------------------------
## postProcess
## 

postProcessMetadata <- function(ahm)
{

    derived <- file.path(metadata(ahm)$AnnotationHubRoot,
        metadata(ahm)$RDataPath)
    metadata(ahm)$RDataSize <- as.integer(file.info(derived)$size)
    metadata(ahm)$RDataLastModifiedDate <- unname(file.info(derived)$mtime)
    json <- toJson(ahm)
    resourceDir <- dirname(metadata(ahm)$SourceFile[1])
    outfile <- file.path(metadata(ahm)$AnnotationHubRoot,
        resourceDir, .derivedFileName(metadata(ahm)$SourceFile, 
        metadata(ahm)$RDataVersion, "json"))
    cat(json, "\n", file=outfile)
    ahm
}

## ------------------------------------------------------------------------------
## show
## 

setMethod(show, "AnnotationHubMetadata",
    function(object)
{
    cat("class: ", class(object), '\n', sep='')
    for (slt in sort(slotNames(object))) {
        value <- slot(object, slt)
        txt <- paste0(slt, ": ", paste0(as.character(value), collapse=" "))
        cat(strwrap(txt), sep="\n  ")
    }
})
