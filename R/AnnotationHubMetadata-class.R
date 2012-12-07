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
       Url="character",
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


constructAnnotationHubMetadataFromJSON <- function(ahroot, resourceDir)
{
    x <- new("AnnotationHubMetadata")
    dir <- file.path(ahroot, resourceDir)
    x@AnnotationHubRoot <- ahroot
    l <- fromJSON(file.path(dir, "metadata.json"))
    for (name in names(l))
    {
        type <- getSlots("AnnotationHubMetadata")[[name]]
        if (type == "integer")
            l[[name]] <- as.integer(l[[name]])
        slot(x, name) <- l[[name]]
    }
    x
}


AnnotationHubMetadata <- function(AnnotationHubRoot, ResourcePath, Url, Title,
    Description,
    Species, Genome, Recipe, Tags, ResourceClass,
    Version, SourceVersion, Coordinate_1_based, Maintainer,
    DataProvider,
    Notes=NULL)
{
    ## fixme do better than this
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)
    x <- new("AnnotationHubMetadata")
    f <- formals()
    m <- match.call()
    for (i in names(f))
    {
        if (length(m[[i]]))
            slot(x, i) <- m[[i]]
        else if (length(f[[i]]))
            slot(x, i) <- f[[i]]

    }
    x@BiocVersion <- as.character(biocVersion())
    x@SourceLastModifiedDate <- "1970-1-1"
    x@DerivedLastModifiedDate <- "1970-1-1"
    x@TaxonomyId <-
        as.character(with(speciesMap, taxon[species == Species]))
    x@Md5 <- unname(tools::md5sum(ResourcePath))
    x@SourceLastModifiedDate <-
        unlist(lapply(ResourcePath, .getModificationTime))
    x@SourceSize <- as.integer(file.info(ResourcePath)$size)
    validObject(x)
    jsonDir <- dirname(ResourcePath[1])
    jsonFile <- basename(ResourcePath[1])
    jsonFile <- sub(".gz", "", jsonFile, fixed=TRUE)
    jsonFile <- "metadata.json" # bad idea?

    json <- as.json(x)
    fullJsonFile <- file.path(AnnotationHubRoot, jsonDir, jsonFile)
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
    toJSON(l)
}

setValidity("AnnotationHubMetadata", function(object)
{
    ## fixme do better than this
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(object@AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)
    rc <- new("MsgClass", name=character(0))

    e <- function(m) {
       rc$name <- c(rc$name, m)
    }


    requiredFields <- c("AnnotationHubRoot", "ResourcePath", "Url", "Title",
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
})


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