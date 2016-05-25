### =========================================================================
### AnnotationHubMetadata objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHubMetadata",
    contains="HubMetadata",
    representation(
        AnnotationHubRoot="character"
    ),
    prototype = prototype(
        AnnotationHubRoot=NA_character_
    )
)

## -----------------------------------------------------------------------------
## constructor
## 

.derivedFileName <-
    function(originalFile, suffix)
{
    ret <- sub(".gz$", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    sprintf("%s_%s.%s", ret, suffix)
}

## single value or NA
.checkThatSingleStringOrNA <- function(value) {
    valStr <- deparse(substitute(value))
    if(!isSingleStringOrNA(value))
        stop(wmsg(paste0(valStr, "must be single value or NA")))
}

## single value or NA, no commas
.checkThatSingleStringOrNAAndNoCommas <- function(value) {
    valStr <- deparse(substitute(value))
    .checkThatSingleStringOrNA(value)
    if(grepl(",",value))
        stop(wmsg(paste0(valStr, "must not contain commas")))
}

## single value, not NA, no commas
.checkThatSingleStringAndNoCommas <- function(value) {
    valStr <- deparse(substitute(value))
    if(!isSingleString(value))
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one ",valStr)))
    if(grepl(",",value))
        stop(wmsg(paste0("The ",valStr," in an AnnotationHubMetdata object",
                         " must not contain any commas")))
}

AnnotationHubMetadata <-
    function(AnnotationHubRoot=NA_character_, SourceUrl, SourceType, 
        SourceVersion,
        SourceLastModifiedDate= as.POSIXct(NA_character_), 
        SourceMd5=NA_character_, SourceSize=NA_real_,
        DataProvider, Title, Description,
        Species, TaxonomyId, Genome, Tags, Recipe,
        RDataClass, RDataDateAdded, RDataPath,
        Maintainer, ..., BiocVersion=biocVersion(), Coordinate_1_based = TRUE,
        Notes=NA_character_, DispatchClass,
        Location_Prefix='http://s3.amazonaws.com/annotationhub/')
{
    if (missing(TaxonomyId))
    {
        if (!is.na(Species) &&
            requireNamespace("AnnotationHubData", quietly=TRUE))
            TaxonomyId <- GenomeInfoDb:::.taxonomyId(Species)
        else
            TaxonomyId <- NA_integer_
    }
    TaxonomyId <- as.integer(TaxonomyId)
    if(!(isSingleInteger(TaxonomyId) || is.na(TaxonomyId)))
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one taxonomy ID or NA")))

    if(any(is.na(SourceUrl)))
        stop(wmsg(paste0("AnnotationHubMetdata SourceUrl slot cannot",
                         " contain NAs")))

    if (missing(RDataPath)) { 
        ## Add two characters: one for substr starting AT clipChars
        ## and one for extra slash
        clipChars <- nchar(Location_Prefix) + 2  
        RDataPath <- substr(SourceUrl, clipChars, nchar(SourceUrl))
    }

    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    mustBeSingleStringNoCommasOrNA <- 
        c(SourceType, Location_Prefix, DispatchClass, RDataClass)
    lapply(mustBeSingleStringNoCommasOrNA, .checkThatSingleStringAndNoCommas) 

    lapply(c(Recipe, Genome, Species), .checkThatSingleStringOrNA)

    .checkThatSingleStringOrNAAndNoCommas(SourceVersion)

    new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
        HubRoot=AnnotationHubRoot,
        BiocVersion=BiocVersion,
        Coordinate_1_based=Coordinate_1_based,
        DataProvider=DataProvider,
        Description=Description,
        Genome=Genome,
        Maintainer=Maintainer,
        Notes=Notes,
        RDataClass=RDataClass,
        RDataDateAdded=as.POSIXct(RDataDateAdded),
        RDataPath=RDataPath,
        Recipe=Recipe,
        SourceLastModifiedDate=SourceLastModifiedDate,
        SourceMd5=SourceMd5, 
        SourceSize=SourceSize,
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        SourceType=SourceType,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        Location_Prefix=Location_Prefix,
        DispatchClass=DispatchClass, 
        ...
    )
}

## -----------------------------------------------------------------------------
## validity 
## 

.checkSourceurlPrefixesAreValid <- function(url){
    safePrefixes <- c('http://','https://','ftp://','rtracklayer://')
    lst <- lapply(safePrefixes, grepl, x=url)
    if(!all(Reduce('|', lst))){
        stop(wmsg(paste0("sourceurl provided has an invalid prefix (missing ",
                        "protocol). Source urls should be full uris that point ",
                        "to the original resources used in a recipe.")))
    }
} 

.checkSourceurlsFreeOfDoubleSlashes <- function(url){
    if(any(grepl("\\w//", url, perl=TRUE))){
        stop(wmsg(paste0("sourceurl provided has a double slash outside of the ",
                         "protocol). Source urls should be working uris that ",
                         "point to the original resources used in a recipe.")))
    }
}

## try to make sure genomes do not contain weird suffixes.. (should be short)
.checkThatGenomeLooksReasonable <- function(genome){
    if(!is.na(genome) && nchar(genome) > 30){
        warning(wmsg("genome provided is suspiciously long. ",
                         "Check to make sure that the genome is legitimate and ",
                         "does not contain unnecessary extensions etc."))
    }
}

.checkRdataclassIsReal <- function(class){
    tryCatch(isClass(class), error = function(err){
        stop("The rdataclass must be a valid R data type. \n",
             conditionMessage(err))})
}

expectedSourceTypes <- c("BED", "UCSC track", "VCF", "GTF", "GFF", "CSV", "TSV",
                         "BigWig", "TwoBit", "Chain",
                         "FASTA", "BioPax", "BioPaxLevel2", "BioPaxLevel3",
                         "Inparanoid", "NCBI/blast2GO", "NCBI/UniProt",
                         "NCBI/ensembl", "GRASP", "Zip", "RData", "tar.gz",
                         "tab", "mzML", "mzTab", "mzid")
.checkThatSourceTypeSoundsReasonable <- function(sourcetype) {
    if(!(sourcetype %in% expectedSourceTypes)) {
        stop(paste0("'SourceType' should be one of: ",
                    paste(expectedSourceTypes, collapse=", ")))
  }
}

.checkThatRDataPathIsOK <- function(rdatapath) {
    ## no spaces are allowed in RDataPath field
    if(any(grepl(" ", rdatapath)))
        stop(wmsg("The string for RDataPath cannot contain spaces."))

    protocolPrefixes <- c('^http://','^https://','^ftp://','^rtracklayer://')
    prefixesFound <- unlist(lapply(protocolPrefixes, FUN=grepl, x=rdatapath))
    if(any(prefixesFound))
        stop(wmsg(paste0("The string for an RDataPath should only contain",
                         " the partial path after the location_Prefix",
                         " (including the protocol) has been trimmed off")))
}

setValidity("AnnotationHubMetadata",function(object) {
    msg = NULL
    ## if the location prefix is "non-standard" (IOW not stored in S3) and 
    ## if the source URL is not the same as rdatapath 
    ## then we need to add a message and fail out
    standardLocationPrefix <- 'http://s3.amazonaws.com/annotationhub/'
    if(object@Location_Prefix != standardLocationPrefix){
        object@RDataPath <- object@RDataPath[1]
        if(object@RDataPath != object@SourceUrl){
            msg <- c(msg, "the string for RDataPath must match the SourceUrl.")
        }
    }
    if (is.null(msg)) TRUE else msg 
 
    ## more checks
    .checkSourceurlPrefixesAreValid(object@SourceUrl)
    .checkSourceurlsFreeOfDoubleSlashes(object@SourceUrl)
    .checkThatGenomeLooksReasonable(object@Genome)
    .checkRdataclassIsReal(object@RDataClass)
    .checkThatSourceTypeSoundsReasonable(object@SourceType)
    GenomeInfoDb:::.checkForAValidTaxonomyId(object@TaxonomyId)
    .checkThatRDataPathIsOK(object@RDataPath)
})

## ------------------------------------------------------------------------------
## run
##

setMethod("run", "AnnotationHubMetadata",
    function(object, recipeFunction, ...) {
       if (missing(recipeFunction)) {
         temp <- strsplit(recipeName(object), ":::")[[1]]  
         functionName <- temp[2]
         pkgName <- temp[1]
         recipeFunction <- get(functionName, envir=getNamespace(pkgName))
       }
       stopifnot(is.function(recipeFunction))
       recipeFunction(object) ## disregard return value
       object
})

## ------------------------------------------------------------------------------
## to / from Json
## 

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

.isComplete <-
    function(object)
{
    rc <- .Message()

    ## required fields must have non-zero length
    requiredFields <- c("HubRoot", 
        "SourceUrl", "Title", "Species", "Genome", "Recipe", "Tags",
        "RDataClass", "SourceVersion",
        "Coordinate_1_based", "Maintainer", "DataProvider",
        "RDataDateAdded")
    values <- metadata(object)[requiredFields]
    idx <- sapply(values, length) == 0L
    if (any(idx))
        rc$append("slots(s) must have non-zero length: %s",
                  paste(sQuote(requiredFields[idx]), collapse=", "))

    ## look up species id in data table
    taxonomyId <- GenomeInfoDb:::.taxonomyId(metadata(object)$Species)
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

toJson <-
    function(x)
{
    lst <- metadata(x)
    ## FIXME: does this matter?
    lst$HubRoot <- NULL       # drop AHRoot

    idx <- grep("(RData|Bioc)Version", names(lst))  # version as character
    lst[idx] <- lapply(lst[idx], as.character)

    idx <- grep("Date", names(lst))
    lst[idx] <- lapply(lst[idx], format, "%Y-%m-%d %T UTC")

    ## encode NA to survive json via mangling
    toJSON(.encodeNA(lst))
}

HubMetadataFromJson <-
    function(path, ahroot=NA_character_)
{
    lst <- .decodeNA(fromJSON(file=path))
    lst <- lst[!sapply(lst, is.null)]         # replace with default values

    lst[["BiocVersion"]] <- package_version(lst$BiocVersion)
     # lst[["BiocVersion"]] <- lapply(lst$BiocVersion, package_version)

    idx <- grep("Date", names(lst))
    lst[idx] <- rapply(lst[idx], function(x) {
        x[!nzchar(x)] <- NA_character_
        as.POSIXct(x)
    }, "character", how="list")

    idx <- sapply(lst, is, "AsIs")
    lst[idx] <- lapply(lst[idx], unclass)

    x <- lst[["Recipe"]];
    setNames(as.character(x), names(x))
    idx <- grep("Size", names(lst))
    lst[idx] <- rapply(lst[idx], as.numeric, how="list")

    ## create AnnotationHubMetadata object
    if (1L == length(lst$Title)) {
        ahm <- do.call(AnnotationHubMetadata, c(HubRoot=ahroot, lst))
        ok <- .isComplete(ahm)
    } else {
        args <- c(list(AnnotationHubMetadata, HubRoot=ahroot), lst)
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
        filename <- .derivedFileName(sourceFile, "json")
    }
    if (flat)
        outfile <- file.path(ahroot, filename)
    else
        outfile <- file.path(ahroot, resourceDir, filename)
    cat(json, "\n", file=outfile)
    outfile
}

## ------------------------------------------------------------------------------
## FIXME: not used?
## 

jsonPath <-
    function(x)
{
    with(metadata(x), {
        fl <- sprintf("%s_%s.json", SourceFile)
        file.path(HubRoot, fl)
    })
}

.NA_version_ <- numeric_version("0.0")  ## proxy for unknown version
.as.numeric_version <-
    function(x, ...)
{
    if (is(x, "character"))
        x[x == "unknown"] <- as.character(.NA_version_)
    base::as.numeric_version(x)
}

constructAnnotationHubMetadataFromSourceFilePath <-
    function(ahroot, originalFile)
{
    dir <- dirname(file.path(ahroot, originalFile))
    jsonFile <- .derivedFileName(originalFile, "json")
    jsonFile <- file.path(dir[1], jsonFile)
    HubMetadataFromJson(jsonFile, ahroot)
}

constructMetadataFromJsonPath <-
    function(ahroot, jsonpath)
{
    jsonFile <- file.path(ahroot, jsonpath)[1]
    HubMetadataFromJson(jsonFile, ahroot)
}

.getExistingResources <-
    function(BiocVersion=biocVersion(), RDataDateAdded="2013-01-22")
{
    url <-
        sprintf("http://annotationhub.bioconductor.org/ah/%s/%s/query/cols/all",
                BiocVersion, RDataDateAdded)
    t <- tempfile()
    download.file(url, t, quiet=TRUE)
    HubMetadataFromJson(t)
}
