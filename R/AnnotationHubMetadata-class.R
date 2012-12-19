setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)

##' Metadata about a resource in AnnotationHub
##' @name AnnotationHubMetadata-class
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
    do.call(new, c(list(class(x)), value))
})

# So you can do stuff like this:
# set a to be an AnnotationHubMetadata object, then:
#  metadata(a)[c("AnnotationHubRoot","Maintainer")]
#   <- list("/etc", "foo@bar.com")

### generics, getters and setters

setGeneric("AnnotationHubRoot", function(x, ...)
    standardGeneric("AnnotationHubRoot"))
setMethod("AnnotationHubRoot", "AnnotationHubMetadata",
    function(x) {
        as.character(x@AnnotationHubRoot)
})
setGeneric("AnnotationHubRoot<-",
           function(x, ..., value) standardGeneric("AnnotationHubRoot<-"))
setReplaceMethod("AnnotationHubRoot", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'AnnotationHubRoot' value must be character")
        if (!file.exists(value))
            stop("replacement 'AnnotationHubRoot' must exist on filesystem")
        x@AnnotationHubRoot <- value
        x
})

setGeneric("Title", function(x, ...)
    standardGeneric("Title"))
setMethod("Title", "AnnotationHubMetadata",
    function(x) {
        x@Title
})
setGeneric("Title<-",
           function(x, ..., value) standardGeneric("Title<-"))
setReplaceMethod("Title", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Title' value must be character")
        x@Title <- value
        x
})



setGeneric("Species", function(x, ...)
    standardGeneric("Species"))
setMethod("Species", "AnnotationHubMetadata",
    function(x) {
        x@Species
})
setGeneric("Species<-",
           function(x, ..., value) standardGeneric("Species<-"))
setReplaceMethod("Species", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Species' value must be character")
        if (!exists("speciesMap")) data(speciesMap)
        taxonomyId <- with(speciesMap, taxon[species == value])
        if (!length(taxonomyId))
            stop("replacement 'Species' value is not valid")
        x@Species <- value
        x
})


setGeneric("TaxonomyId", function(x, ...)
    standardGeneric("TaxonomyId"))
setMethod("TaxonomyId", "AnnotationHubMetadata",
    function(x) {
        x@TaxonomyId
})
setGeneric("TaxonomyId<-",
           function(x, ..., value) standardGeneric("TaxonomyId<-"))
setReplaceMethod("TaxonomyId", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'TaxonomyId' value must be character")
        if (!exists("speciesMap")) data(speciesMap)
        species <- with(speciesMap, species[taxon == value])
        if (!length(species))
            stop("replacement 'TaxonomyId' value is not valid")
        x@TaxonomyId <- value
        x
})

setGeneric("Genome", function(x, ...)
    standardGeneric("Genome"))
setMethod("Genome", "AnnotationHubMetadata",
    function(x) {
        x@Genome
})
setGeneric("Genome<-",
           function(x, ..., value) standardGeneric("Genome<-"))
setReplaceMethod("Genome", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Genome' value must be character")
        x@Genome <- value
        x
})

setGeneric("Recipe", function(x, ...)
    standardGeneric("Recipe"))
setMethod("Recipe", "AnnotationHubMetadata",
    function(x) {
        x@Recipe
})
setGeneric("Recipe<-",
           function(x, ..., value) standardGeneric("Recipe<-"))
setReplaceMethod("Recipe", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Recipe' value must be character")
        x@Recipe <- value
        x
})

setGeneric("RecipeArgs", function(x, ...)
    standardGeneric("RecipeArgs"))
setMethod("RecipeArgs", "AnnotationHubMetadata",
    function(x) {
        x@RecipeArgs
})
setGeneric("RecipeArgs<-",
           function(x, ..., value) standardGeneric("RecipeArgs<-"))
setReplaceMethod("RecipeArgs", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.list(value))
            stop("replacement 'RecipeArgs' value must be a list")
        x@RecipeArgs <- value
        x
})


setGeneric("Url", function(x, ...)
    standardGeneric("Url"))
setMethod("Url", "AnnotationHubMetadata",
    function(x) {
        x@Url
})
setGeneric("Url<-",
           function(x, ..., value) standardGeneric("Url<-"))
setReplaceMethod("Url", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Url' value must be character")
        x@Url <- value
        x
})


setGeneric("OriginalFile", function(x, ...)
    standardGeneric("OriginalFile"))
setMethod("OriginalFile", "AnnotationHubMetadata",
    function(x) {
        x@OriginalFile
})
setGeneric("OriginalFile<-",
           function(x, ..., value) standardGeneric("OriginalFile<-"))
setReplaceMethod("OriginalFile", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'OriginalFile' value must be character")
        x@OriginalFile <- value
        x
})


setGeneric("ResourcePath", function(x, ...)
    standardGeneric("ResourcePath"))
setMethod("ResourcePath", "AnnotationHubMetadata",
    function(x) {
        x@ResourcePath
})
setGeneric("ResourcePath<-",
           function(x, ..., value) standardGeneric("ResourcePath<-"))
setReplaceMethod("ResourcePath", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'ResourcePath' value must be character")
        x@ResourcePath <- value
        x
})


setGeneric("Md5", function(x, ...)
    standardGeneric("Md5"))
setMethod("Md5", "AnnotationHubMetadata",
    function(x) {
        x@Md5
})
setGeneric("Md5<-",
           function(x, ..., value) standardGeneric("Md5<-"))
setReplaceMethod("Md5", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Md5' value must be character")
        x@Md5 <- value
        x
})


setGeneric("Derived.md5", function(x, ...)
    standardGeneric("Derived.md5"))
setMethod("Derived.md5", "AnnotationHubMetadata",
    function(x) {
        x@Derived.md5
})
setGeneric("Derived.md5<-",
           function(x, ..., value) standardGeneric("Derived.md5<-"))
setReplaceMethod("Derived.md5", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Derived.md5' value must be character")
        x@Derived.md5 <- value
        x
})

setGeneric("Description", function(x, ...)
    standardGeneric("Description"))
setMethod("Description", "AnnotationHubMetadata",
    function(x) {
        x@Description
})
setGeneric("Description<-",
           function(x, ..., value) standardGeneric("Description<-"))
setReplaceMethod("Description", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Description' value must be character")
        x@Description <- value
        x
})

setGeneric("Tags", function(x, ...)
    standardGeneric("Tags"))
setMethod("Tags", "AnnotationHubMetadata",
    function(x) {
        x@Tags
})
setGeneric("Tags<-",
           function(x, ..., value) standardGeneric("Tags<-"))
setReplaceMethod("Tags", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Tags' value must be character")
        x@Tags <- value
        x
})

setGeneric("ResourceClass", function(x, ...)
    standardGeneric("ResourceClass"))
setMethod("ResourceClass", "AnnotationHubMetadata",
    function(x) {
        x@ResourceClass
})
setGeneric("ResourceClass<-",
           function(x, ..., value) standardGeneric("ResourceClass<-"))
setReplaceMethod("ResourceClass", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'ResourceClass' value must be character")
        x@ResourceClass <- value
        x
})

setGeneric("Version", function(x, ...)
    standardGeneric("Version"))
setMethod("Version", "AnnotationHubMetadata",
    function(x) {
        x@Version
})
setGeneric("Version<-",
           function(x, ..., value) standardGeneric("Version<-"))
setReplaceMethod("Version", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Version' value must be character")
        x@Version <- value
        x
})

setGeneric("SourceVersion", function(x, ...)
    standardGeneric("SourceVersion"))
setMethod("SourceVersion", "AnnotationHubMetadata",
    function(x) {
        x@SourceVersion
})
setGeneric("SourceVersion<-",
           function(x, ..., value) standardGeneric("SourceVersion<-"))
setReplaceMethod("SourceVersion", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'SourceVersion' value must be character")
        x@SourceVersion <- value
        x
})

setGeneric("SourceSize", function(x, ...)
    standardGeneric("SourceSize"))
setMethod("SourceSize", "AnnotationHubMetadata",
    function(x) {
        x@SourceSize
})
setGeneric("SourceSize<-",
           function(x, ..., value) standardGeneric("SourceSize<-"))
setReplaceMethod("SourceSize", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.integer(value))
            stop("replacement 'SourceSize' value must be integer")
        x@SourceSize <- value
        x
})

setGeneric("DerivedSize", function(x, ...)
    standardGeneric("DerivedSize"))
setMethod("DerivedSize", "AnnotationHubMetadata",
    function(x) {
        x@DerivedSize
})
setGeneric("DerivedSize<-",
           function(x, ..., value) standardGeneric("DerivedSize<-"))
setReplaceMethod("DerivedSize", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.integer(value))
            stop("replacement 'DerivedSize' value must be integer")
        x@DerivedSize <- value
        x
})

setGeneric("SourceLastModifiedDate", function(x, ...)
    standardGeneric("SourceLastModifiedDate"))
setMethod("SourceLastModifiedDate", "AnnotationHubMetadata",
    function(x) {
        x@SourceLastModifiedDate
})
setGeneric("SourceLastModifiedDate<-",
           function(x, ..., value) standardGeneric("SourceLastModifiedDate<-"))
setReplaceMethod("SourceLastModifiedDate", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'SourceLastModifiedDate' value must be character")
        x@SourceLastModifiedDate <- value
        x
})

setGeneric("DerivedLastModifiedDate", function(x, ...)
    standardGeneric("DerivedLastModifiedDate"))
setMethod("DerivedLastModifiedDate", "AnnotationHubMetadata",
    function(x) {
        x@DerivedLastModifiedDate
})
setGeneric("DerivedLastModifiedDate<-",
           function(x, ..., value) standardGeneric("DerivedLastModifiedDate<-"))
setReplaceMethod("DerivedLastModifiedDate", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'DerivedLastModifiedDate' value must be character")
        x@DerivedLastModifiedDate <- value
        x
})

setGeneric("Coordinate_1_based", function(x, ...)
    standardGeneric("Coordinate_1_based"))
setMethod("Coordinate_1_based", "AnnotationHubMetadata",
    function(x) {
        x@Coordinate_1_based
})
setGeneric("Coordinate_1_based<-",
           function(x, ..., value) standardGeneric("Coordinate_1_based<-"))
setReplaceMethod("Coordinate_1_based", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.logical(value))
            stop("replacement 'Coordinate_1_based' value must be logical")
        x@Coordinate_1_based <- value
        x
})


setGeneric("Maintainer", function(x, ...)
    standardGeneric("Maintainer"))
setMethod("Maintainer", "AnnotationHubMetadata",
    function(x) {
        x@Maintainer
})
setGeneric("Maintainer<-",
           function(x, ..., value) standardGeneric("Maintainer<-"))
setReplaceMethod("Maintainer", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Maintainer' value must be character")
        # FIXME check for email regex
        x@Maintainer <- value
        x
})

setGeneric("DataProvider", function(x, ...)
    standardGeneric("DataProvider"))
setMethod("DataProvider", "AnnotationHubMetadata",
    function(x) {
        x@DataProvider
})
setGeneric("DataProvider<-",
           function(x, ..., value) standardGeneric("DataProvider<-"))
setReplaceMethod("DataProvider", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'DataProvider' value must be character")
        x@DataProvider <- value
        x
})

setGeneric("BiocVersion", function(x, ...)
    standardGeneric("BiocVersion"))
setMethod("BiocVersion", "AnnotationHubMetadata",
    function(x) {
        x@BiocVersion
})
setGeneric("BiocVersion<-",
           function(x, ..., value) standardGeneric("BiocVersion<-"))
setReplaceMethod("BiocVersion", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'BiocVersion' value must be character")
        x@BiocVersion <- value
        x
})

setGeneric("Notes", function(x, ...)
    standardGeneric("Notes"))
setMethod("Notes", "AnnotationHubMetadata",
    function(x) {
        x@Notes
})
setGeneric("Notes<-",
           function(x, ..., value) standardGeneric("Notes<-"))
setReplaceMethod("Notes", "AnnotationHubMetadata",
    function(x, value) {
        if (!is.character(value))
            stop("replacement 'Notes' value must be character")
        x@Notes <- value
        x
})

## end generics, getters and setters


.getModificationTime <- function(files)
{
    ret <- character()
    for (file in files)
    {
        ret <- c(ret, 
            strsplit(as.character(file.info(file)$mtime), " ")[[1]][1]
        )
    }
    ret
}



.constructFromJson <- function(ahroot, pathToJson)
{
    x <- new("AnnotationHubMetadata")
    #x@AnnotationHubRoot <- ahroot
    AnnotationHubRoot(x) <- ahroot
    l <- fromJSON(pathToJson)
    for (name in names(l))
    {
        type <- getSlots("AnnotationHubMetadata")[[name]]
        if (type == "integer")
        {
            l[[name]] <- as.integer(l[[name]])
        }
        slot(x, name) <- l[[name]]
        ###do.call(paste(name, "<-", sep=""), list(x=x, value=l[[name]]))
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
    #x@AnnotationHubRoot <- ahroot
    AnnotationHubRoot(x) <- ahroot

    derived <- file.path(ahroot, x@ResourcePath)
    #x@DerivedSize <- as.integer(file.info(derived)$size)
    DerivedSize(x) <- as.integer(file.info(derived)$size)
    #x@DerivedLastModifiedDate <- .getModificationTime(derived)
    DerivedLastModifiedDate(x) <- .getModificationTime(derived)
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

    jsonDir <- dirname(OriginalFile[1])
    resourceFile <- .getDerivedFileName(OriginalFile, "RData")
    jsonFile <- .getDerivedFileName(OriginalFile, "json")
    resourcePath <- file.path(jsonDir, resourceFile)

    x <- new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
        OriginalFile=OriginalFile,
        Url=Url,
        Title=Title,
        Description=Description,
        Species=Species,
        Genome=Genome,
        Recipe=Recipe,
        RecipeArgs=RecipeArgs,
        Tags=Tags,
        ResourceClass=ResourceClass,
        Version=Version,
        SourceVersion=SourceVersion,
        Coordinate_1_based=Coordinate_1_based,
        Maintainer=Maintainer,
        DataProvider=DataProvider,
        Notes=Notes,
        BiocVersion=as.character(biocVersion()),
        SourceLastModifiedDate=unlist(lapply(OriginalFile,
            .getModificationTime)),
        TaxonomyId=as.character(with(speciesMap, taxon[species == Species])),
        Md5=unname(tools::md5sum(OriginalFile)),
        DerivedLastModifiedDate="1970-1-1",
        SourceSize=as.integer(file.info(OriginalFile)$size),
        ResourcePath=resourcePath
    )


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
    #setwd(object@AnnotationHubRoot)
    setwd(AnnotationHubRoot(object))

    if (!exists("speciesMap")) data(speciesMap)
    taxonomyId <- with(speciesMap, taxon[species == Species(object)])
    if (!length(taxonomyId))
        e("Unknown species")


    ## dropping this for now, this fails with ftp:// urls.
    ## emailed Hadley, hope he can fix it.
    ##headers <- HEAD(object@Url)$headers
    ##if (headers$status != "200")
    ##    e(sprintf("Can't access URL %s"), Url(object))


    emailRegex <- 
        "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"
    if (!grepl(emailRegex, object@Maintainer, ignore.case=TRUE))
        e("Maintainer must contain an email address")

    
    # Make sure dates are valid


    if (length(rc$name) == 0) TRUE else rc$name

}

setValidity("AnnotationHubMetadata",
        function(object) .AnnotationHubMetadata.validity(object))


#setMethod('show', 'AnnotationHubMetadata',
#
#    function(object) {
#        msg = sprintf('AnnotationHubMetadata')
#        cat(msg, '\n', sep='')
#        })
#
#