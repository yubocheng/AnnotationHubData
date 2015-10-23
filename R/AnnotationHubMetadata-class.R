## -----------------------------------------------------------------------------
##
## The formal class defintion etc. for AnnotationHub.
## 


setOldClass(c("POSIXct", "POSIXt"))
setOldClass("numeric_version")
setOldClass(c("package_version", "numeric_version"))

.NA_version_ <- numeric_version("0.0")  ## proxy for unknown version


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
        Genome="character",                    ## needed for record_id
        Maintainer="character",
        Notes='character',
        RDataClass="character",                ## needed for record_id
        RDataDateAdded="POSIXct",
        RDataPath="character",
        Recipe="character",                 ## no longer needed for record_id
        SourceLastModifiedDate="POSIXct",
        SourceMd5="character",
        SourceSize="numeric",
        SourceUrl="character",                 ## needed for record_id
        SourceVersion="character",
        SourceType="character",
        Species="character",
        Tags='character',
        TaxonomyId="integer",                  ## needed for record_id
        Title="character",
        Location_Prefix="character",
        DispatchClass="character",
        PreparerClass="character"              ## needed for record_id
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
        RDataPath=NA_character_,
        Recipe=NA_character_,
        SourceLastModifiedDate=as.POSIXct(NA_character_),
        SourceMd5=NA_character_,
        SourceSize=NA_real_,
        SourceVersion=NA_character_,
        SourceType=NA_character_,
        Species=NA_character_,
        Tags=NA_character_,
        TaxonomyId=NA_integer_,
        Title=NA_character_,
        Location_Prefix=NA_character_,
        DispatchClass=NA_character_,
        PreparerClass=NA_character_
    )
)


## -----------------------------------------------------------------------------
## constructor and validity
## 

.derivedFileName <-
    function(originalFile, suffix)
{
    ret <- sub(".gz$", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    sprintf("%s_%s.%s", ret, suffix)
}

## Helpers for strings that need to be single value or NA
.checkThatSingleStringOrNA <- function(value){
    valStr <- deparse(substitute(value))
    if(!isSingleStringOrNA(value)){
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one ",valStr," or NA")))}
}
.checkThatSingleStringOrNAAndNoCommas <- function(value){
    valStr <- deparse(substitute(value))
    .checkThatSingleStringOrNA(value)
    if(grepl(",",value)){
        stop(wmsg(paste0("The ",valStr," in an AnnotationHubMetdata object",
                         " must not contain any commas")))}
}

## Helper for strings that need to be single value (no commas) and can NOT be NA
.checkThatSingleStringAndNoCommas <- function(value){
    valStr <- deparse(substitute(value))
    if(!isSingleString(value)){
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one ",valStr)))}
    if(grepl(",",value)){
        stop(wmsg(paste0("The ",valStr," in an AnnotationHubMetdata object",
                         " must not contain any commas")))}
}



## alternative prefix (so far)
## http://hgdownload.cse.ucsc.edu/

AnnotationHubMetadata <-
    function(AnnotationHubRoot,  SourceUrl, SourceType, SourceVersion,
        SourceLastModifiedDate= as.POSIXct(NA_character_), 
        SourceMd5=NA_character_, SourceSize=NA_real_,
        DataProvider, Title, Description,
        Species, TaxonomyId, Genome, Tags, Recipe,
        RDataClass, RDataDateAdded, RDataPath,
        Maintainer, ..., BiocVersion=biocVersion(), Coordinate_1_based = TRUE,
        Notes=NA_character_, DispatchClass,
        Location_Prefix='http://s3.amazonaws.com/annotationhub/')
{
    #######################################################################
    ## Try to derive some of this stuff 
    #if (missing(SourceLastModifiedDate) & missing(SourceSize)) {
    #    res <- .httrFileInfo(SourceUrl)
    #    size <- res$size
    #	date <- res$date
    #    if(!all(is.na(date)))
    #        SourceLastModifiedDate <- as.POSIXct(date)
    #	else
    #	    SourceLastModifiedDate <- as.POSIXct(NA_character_) 
    #    if(!all(is.na(size)))
    #        SourceSize <- as.double(size)
    #	else
    #        SourceSize <- NA_real_
    #}
    if (missing(TaxonomyId))
    {
        if (!is.na(Species) &&
            requireNamespace("AnnotationHubData", quietly=TRUE))
            TaxonomyId <- GenomeInfoDb:::.taxonomyId(Species)
        else
            TaxonomyId <- NA_integer_
    }
    if (missing(RDataPath)) {        
        ## Add two. (one for substr starting AT clipChars,
        ## and one for that extra slash)
        clipChars <- nchar(Location_Prefix) + 2  
        RDataPath <- substr(SourceUrl, clipChars, nchar(SourceUrl))
    }
    if (missing(AnnotationHubRoot)){
        AnnotationHubRoot <- "/var/FastRWeb/web" 
    }
    
    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    #######################################################################
    ## More checking to see if we have supplied reasonable values for
    ## things (after guessing and before we call 'new')

    ## 1st check for things that need to be a single string with no
    ## commas (where NAs are allowed)
    mustBeSingleStringNoCommasOrNA <- c(SourceType, Location_Prefix,
                                        DispatchClass, RDataClass)
    lapply(mustBeSingleStringNoCommasOrNA, .checkThatSingleStringAndNoCommas) 

    ## check for things that need to be any single string or NA 
    mustBeSingleString <- c(Recipe, Genome, Species)
    lapply(mustBeSingleString, .checkThatSingleStringOrNA)

    ## 
    .checkThatSingleStringOrNAAndNoCommas(SourceVersion)

    ## Taxonomy Id must be an integer (or NA)
    if(!(isSingleInteger(TaxonomyId) || is.na(TaxonomyId))){
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one taxonomy ID or NA")))}

    ## SourceUrl can be a vector, but no NAs allowed in there
    if(any(is.na(SourceUrl))){
        stop(wmsg(paste0("AnnotationHubMetdata SourceUrl slot cannot",
                         " contain NAs")))}

    ## SourceSize must be a single number (or NA) (no commas you get for free)
    #if((!isSingleNumberOrNA(SourceSize))){
    #    stop(wmsg(paste0("AnnotationHubMetdata SourceSize slot must",
    #                     " contain a single number (with no commas)",
    #                     " or an NA")))}

    ## sourceLastModifiedDate must be only one thing (no commas is free)
    #if(length(SourceLastModifiedDate) > 1){
    #    stop(wmsg(paste0("AnnotationHubMetdata SourceLastModifiedDate slot",
    #                     " must contain a single date (with no commas)",
    #                     " or an NA")))}

    
    #######################################################################
    new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
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


## check function that verifies that all of a vector of values have a valid prefixes
.checkSourceurlPrefixesAreValid <- function(url){
    safePrefixes <- c('http://','https://','ftp://','rtracklayer://')
    lst <- lapply(safePrefixes, grepl, x=url)
    if(!all(Reduce('|', lst))){
        stop(wmsg(paste0("sourceurl provided has an invalid prefix (missing ",
                        "protocol). Source urls should be full uris that point ",
                        "to the original resources used in a recipe.")))
    }
}  ## it turns out the above is a bit overkill (did not need to be vectorised). :P

## check function to ensure that we don't have double slashes in url
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

## check that the rdataclass specified is a real class.
.checkRdataclassIsReal <- function(class){
    tryCatch(isClass(class), error = function(err){
        stop("The rdataclass must be a valid R data type. \n",
             conditionMessage(err))})
}


.checkThatSourceTypeSoundsReasonable <- function(sourcetype){
expectedSourceTypes <- c("BED",                                            
                         "UCSC track",
                         "VCF",
                         "GTF",
                         "GFF",
                         "CSV",
                         "TSV",
                         "BigWig",
                         "TwoBit",
                         "Chain",
                         "FASTA",
                         "BioPax",
                         "BioPaxLevel2",
                         "BioPaxLevel3",
                         "Inparanoid",
                         "NCBI/blast2GO",
                         "NCBI/UniProt",
                         "NCBI/ensembl",
                         "GRASP",
                         "Zip",
                         "RData",
                         "tar.gz", 
                         "tab", "mzML", "mzTab", "mzid" )
if(!(sourcetype %in% expectedSourceTypes)){
      warning(wmsg(paste0("The source type you have provided (",sourcetype,")",
                       " looks unusual.  We were expecting one of these",
                       " values: ",paste(expectedSourceTypes, collapse=", "),
                       ". Please check to make sure that yoour source type",
                       " is really what you want and if so, then please tell",
                       " us about it so that we can add your source type to",
                       " our list of expected values.."))) 
  }
}


.checkThatRDataPathIsOK <- function(rdatapath){
    ## no spaces are allowed int he RDataPath field
    if(any(grepl(" ", rdatapath))){
        stop(wmsg("The string for RDataPath cannot contain spaces."))
    }
    protocolPrefixes <- c('^http://','^https://','^ftp://','^rtracklayer://')
    prefixesFound <- unlist(lapply(protocolPrefixes, FUN=grepl, x=rdatapath))
    if(any(prefixesFound)){
        stop(wmsg(paste0("The string for an RDataPath should only contain",
                         " the partial path after the location_Prefix",
                         " (including the protocol) has been trimmed off")))
    }    
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
## helper classes and functions
## 

.as.numeric_version <-
    function(x, ...)
{
    if (is(x, "character"))
        x[x == "unknown"] <- as.character(.NA_version_)
    base::as.numeric_version(x)
}


## ------------------------------------------------------------------------------
## isComplete is used for translation from json
##

.isComplete <-
    function(object)
{
    rc <- .Message()

    ## required fields must have non-zero length
    requiredFields <- c("AnnotationHubRoot", 
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



## ----------------------------------------------------------------------------
## Generics

setGeneric("recipeName", signature="object",
           function(object)
           standardGeneric ("recipeName"))

setGeneric("inputFiles", signature="object",
           function(object, ...)
           standardGeneric ("inputFiles"))

setGeneric("outputFile", signature="object",
           function(object)
           standardGeneric ("outputFile"))

setGeneric("run", signature="object",
    function(object, recipeFunction, ...)
        standardGeneric ("run"))



## ------------------------------------------------------------------------------
## getter and setter methods
## 

setMethod("metadata", "AnnotationHubMetadata",
    function(x, ...) 
{
    nms <- slotNames(class(x))
    names(nms) <- nms
    lapply(nms, slot, object=x)
})

#------------------------------------------------------------------------------
setReplaceMethod("metadata", c("AnnotationHubMetadata", "list"),
     function(x, ..., value)
{
    do.call(new, c(class(x), x, value))
})

#------------------------------------------------------------------------------
setMethod("run", "AnnotationHubMetadata",
    function(object, recipeFunction, ...) {
       if (missing(recipeFunction)) {
         temp <- strsplit(recipeName(object), ":::")[[1]]  
         functionName <- temp[2]
         pkgName <- temp[1]
         recipeFunction <- get(functionName,
              envir=getNamespace(pkgName))
       }
       stopifnot(is.function(recipeFunction))
       recipeFunction(object) ## disregard return value
       postProcessMetadata(object)
       })

#------------------------------------------------------------------------------
setMethod("recipeName", "AnnotationHubMetadata",

    function(object) {
        metadata(object)$Recipe
        })

#------------------------------------------------------------------------------
setMethod("inputFiles", "AnnotationHubMetadata",
    function(object, useRoot=TRUE) {
        if(useRoot==TRUE){
            res <- file.path(metadata(object)$AnnotationHubRoot,
                             metadata(object)$RDataPath)            
        }else{
            res <- metadata(object)$SourceUrl
        }
        res
    })

#------------------------------------------------------------------------------
setMethod("outputFile", "AnnotationHubMetadata",
    function(object) {
        file.path(metadata(object)$AnnotationHubRoot,
                  metadata(object)$RDataPath)
        })










## ------------------------------------------------------------------------------
## to / from Json
## 

jsonPath <-
    function(x)
{
    with(metadata(x), {
        fl <- sprintf("%s_%s.json", SourceFile)
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
      # lapply produces a list.   but one character string is the only valid value
      # replace old lapply
      # lst[["Recipe"]] <- lapply(lst$Recipe, function(x) setNames(as.character(x),
      #                           names(x)))


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
        filename <- .derivedFileName(sourceFile, "json")
    }
    if (flat)
        outfile <- file.path(ahroot, filename)
    else
        outfile <- file.path(ahroot, resourceDir, filename)
    cat(json, "\n", file=outfile)
    outfile
}

constructAnnotationHubMetadataFromSourceFilePath <-
    function(ahroot, originalFile)
{
    dir <- dirname(file.path(ahroot, originalFile))
    jsonFile <- .derivedFileName(originalFile, "json")
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

    #derived <- file.path(metadata(ahm)$AnnotationHubRoot,
    #    metadata(ahm)$RDataPath)
    #metadata(ahm)$RDataSize <- as.integer(file.info(derived)$size)
    #metadata(ahm)$RDataLastModifiedDate <- unname(file.info(derived)$mtime)
    #json <- toJson(ahm)
    #resourceDir <- dirname(metadata(ahm)$SourceUrl[1])
    #outfile <- file.path(metadata(ahm)$AnnotationHubRoot,
    #    resourceDir, .derivedFileName(metadata(ahm)$SourceUrl, "json"))
    #cat(json, "\n", file=outfile)
    ahm
}



#------------------------------------------------------------------------------
# the GRanges that we assemble here need SeqInfo -- a generalized name
# for what is usually chromosome info:  chromosome name, chromosome length
# and circularity (common among prokaryotic organisms, but also found in
# metazoan mitochondrial chromosomes)
constructSeqInfo <- function(species, genome)
{
  recognized.human <- species=="Homo sapiens" & genome %in% c("hg18", "hg19")
  recognized.mouse <- species=="Mus musculus" & genome %in% c("mm10")
  recognized <- recognized.human | recognized.mouse
  
  stopifnot(recognized)
  
  suppressMessages({
       # chroms 1-22, X, Y, M are assumed to be the first 25 rows of the
       # data.frame
     if(recognized.human)
        tbl.chromInfo =
            GenomicFeatures:::.makeUCSCChrominfo (genome,
                                                  circ_seqs="chrM") [1:25,]
     if(recognized.mouse)
        tbl.chromInfo =
            GenomicFeatures:::.makeUCSCChrominfo (genome,
                                                  circ_seqs="chrM") [1:22,]
         
     })

   Seqinfo(as.character(tbl.chromInfo$chrom), 
           seqlengths=tbl.chromInfo$length, 
           isCircular=tbl.chromInfo$is_circular,
           genome=genome)


} # constructSeqInfo
#------------------------------------------------------------------------------
.sortTableByChromosomalLocation <- function(tbl)
{
  stopifnot (all (c ('seqname', 'start') %in% colnames (tbl)))
  factor.chromNames <- factor (tbl$seqname,
                               levels=paste("chr", c(1:22, "X", "Y", "M"),
                                            sep=''))
  tbl$seqname <- factor.chromNames
  tbl <- tbl [order (tbl$seqname, tbl$start), ]
  invisible (tbl)

} # .sortTableByChromsomalLocation 
#------------------------------------------------------------------------------
