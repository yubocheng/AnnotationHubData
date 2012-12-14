setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)


setClass("AnnotationHubMetadata",
    representation(
       AnnotationHubRoot="character",
       Title="character",
       Species="character",
       TaxonomyId="character",
       Genome="character",
       Recipe="character",
       RecipeArgs="list",
       Url="character",
       OriginalFile="character",
       ResourcePath="character",
       Md5="character",
       Derived.md5="character",
       Description='character',
       Tags='character',
       ResourceClass="character",
       Version="character",
       SourceVersion="character",
       SourceSize="integer",
       DerivedSize="integer",
       SourceLastModifiedDate="character",
       DerivedLastModifiedDate="character",
       Coordinate_1_based="logical",
       Maintainer="character",
       DataProvider="character",
       BiocVersion="character",
       Notes='character'
    )
)


.getModificationTime <- function(files)
{
    ret <- character()
    for (file in files)
    {
        ret <- c(ret, 
            strsplit(as.character(file.info(file)$mtime), " "))[[1]][1]
    }
    ret
}



.constructFromJson <- function(ahroot, pathToJson)
{
    x <- new("AnnotationHubMetadata")
    x@AnnotationHubRoot <- ahroot
    l <- fromJSON(pathToJson)
    for (name in names(l))
    {
        type <- getSlots("AnnotationHubMetadata")[[name]]
        if (type == "integer")
            l[[name]] <- as.integer(l[[name]])
        slot(x, name) <- l[[name]]
    }
    x
}

constructAnnotationHubMetadataFromOriginalFilePath <- function(ahroot, originalFile)
{
    dir <- dirname(file.path(ahroot, originalFile))
    jsonFile <- .getDerivedFileName(originalFile, "json")
    jsonFile <- file.path(dir[1], jsonFile)
    .constructFromJson(ahroot, jsonFile)
}

constructMetadataFromJsonPath <-
    function(ahroot, jsonpath)
{
    dir <- dirname(file.path(ahroot, jsonpath))
    jsonFile <- file.path(dir[1], basename(jsonpath))
    .constructFromJson(ahroot, jsonFile)
}

.getJsonFileName <- function(ahroot, originalFile)
{
    dir <- dirname(file.path(ahroot, originalFile))
#    b <- basename(originalFile)
    b <- .getDerivedFileName(originalFile, "json")
    b <- sub(".gz", "", b)
    b <- sprintf("%s.json", b)
    dir(dir, pattern=b, recursive=FALSE, full=TRUE)
}


postProcessMetadata <- function(ahroot, originalFile)
{
    x <- constructAnnotationHubMetadataFromOriginalFilePath(ahroot, originalFile)
    x@AnnotationHubRoot <- ahroot

    derived <- file.path(ahroot, x@ResourcePath)
    x@DerivedSize <- as.integer(file.info(derived)$size)
    x@DerivedLastModifiedDate <- .getModificationTime(derived)
    json <- as.json(x)
    resourceDir <- dirname(originalFile[1])
    outfile <- file.path(ahroot, resourceDir, .getDerivedFileName(originalFile, "json"))
    cat(json, file=outfile)
    x
}


.getDerivedFileName <- function(originalFile, suffix)
{
    ret <- sub(".gz", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    ret <- sprintf("%s.%s", ret, suffix)
    ret
}

AnnotationHubMetadata <- function(AnnotationHubRoot, OriginalFile, Url, Title,
    Description,
    Species, Genome, Recipe, RecipeArgs=list(), Tags, ResourceClass,
    Version, SourceVersion, Coordinate_1_based, Maintainer,
    DataProvider,
    Notes="")
{
    ## fixme do better than this
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)
    x <- new("AnnotationHubMetadata")

    f <- formals()
    for (i in names(f))
    {   
        item <- NULL
        tryCatch(item <- get(i, inherits=FALSE), error=function(e) {})
        if (!is.null(item))
        {
            if (class(item) %in% "call") item <- as.list(item) # rapply?
            slot(x, i) <- item
        }
    }
    x@BiocVersion <- as.character(biocVersion())
    x@SourceLastModifiedDate <- "1970-1-1"
    x@DerivedLastModifiedDate <- "1970-1-1"
    x@TaxonomyId <-
        as.character(with(speciesMap, taxon[species == Species]))
    x@Md5 <- unname(tools::md5sum(OriginalFile))
    x@SourceLastModifiedDate <-
        unlist(lapply(OriginalFile, .getModificationTime))
    x@SourceSize <- as.integer(file.info(OriginalFile)$size)
    validObject(x)
    jsonDir <- dirname(OriginalFile[1])

    resourceFile <- .getDerivedFileName(OriginalFile, "RData")
    jsonFile <- .getDerivedFileName(OriginalFile, "json")
    resourcePath <- file.path(jsonDir, resourceFile)
    x@ResourcePath <- resourcePath


    json <- as.json(x)

    cat(json, file=file.path(AnnotationHubRoot, jsonDir, jsonFile))

    x
}

as.json <- function(annotationHubMetadata)
{
    l <- list()
    for (name in slotNames(annotationHubMetadata))
    {
        item <- slot(annotationHubMetadata, name)
        if (length(item))
        {
            if (name != "AnnotationHubRoot")
            {
                l[[name]] <- item
            }
        }
    }
    l2<- rapply(l, as.list, "call", how="replace")
    toJSON(l)
}

.AnnotationHubMetadata.validity <- function(object)
{
    rc <- new("MsgClass", name=character(0))
    e <- function(m) {
       rc$name <- c(rc$name, m)
    }

    requiredFields <- c("AnnotationHubRoot", "OriginalFile", "Url", "Title",
        "Species", "Genome", "Recipe", "Tags", "ResourceClass",
        "Version", "SourceVersion",
        "Coordinate_1_based", "Maintainer", "DataProvider")

    empty <- function(x) {
        return(length(slot(object, x))==0)
    }

    lapply(requiredFields, function(x)
    {
        if (empty(x))
            e(sprintf("%s is required.", x))
    })

    ## fixme do better than this
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(object@AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)
    taxonomyId <- with(speciesMap, taxon[species == object@Species])
    if (!length(taxonomyId))
        e("Unknown species")


    ## dropping this for now, this fails with ftp:// urls.
    ## emailed Hadley, hope he can fix it.
    ##headers <- HEAD(object@Url)$headers
    ##if (headers$status != "200")
    ##    e(sprintf("Can't access URL %s"), object@Url)


    emailRegex <- 
        "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"
    if (!grepl(emailRegex, object@Maintainer, ignore.case=TRUE))
        e("Maintainer must contain an email address")

    
    # Make sure dates are valid


    if (length(rc$name) == 0) TRUE else rc$name

}

setValidity("AnnotationHubMetadata",
        function(object) .AnnotationHubMetadata.validity(object))


#setGeneric('metadataTitle', signature='object', function(object) standardGeneric('metadataTitle'))
#setGeneric('dcfFile', signature='object', function(object) standardGeneric('dcfFile'))
#setGeneric('dataFile', signature='object', function(object) standardGeneric ('dataFile'))


#setValidity('AnnotationHubMetadata', function(object) {
#   msg <- NULL
#   dcfFile <- dcfFile(object)
#     # the empty constructor is valid, if not very interesting nor useful
#   if(is.na(dcfFile))
#       return(TRUE)
#   if(!file.exists(dcfFile))
#       msg <- c(msg, sprintf("cannot read '%s'", dcfFile))
#   title <- metadataTitle(object)
#   if(nchar(title) == 0)
#       msg <- c(msg, sprintf("zero-length 'metadata title is missing"))
#   if (!is.null(msg))
#       return(msg)
#   
#   return(TRUE)
#})
#


#setMethod('dcfFile', 'AnnotationHubMetadata',
#    function(object){
#        object@dcfFile
#      })
#
#setMethod('metadataTitle', 'AnnotationHubMetadata',
#    function(object){
#        object@title
#      })
#
#setMethod('dataFile', 'AnnotationHubMetadata',
#    function(object){
#        object@dataFile
#      })
#
#setMethod('show', 'AnnotationHubMetadata',
#
#    function(object) {
#        msg = sprintf('AnnotationHubMetadata')
#        cat(msg, '\n', sep='')
#        })
#
#