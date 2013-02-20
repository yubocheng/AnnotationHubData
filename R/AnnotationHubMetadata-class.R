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
       SourceUrl="character",
       SourceFile="character",
       RDataPath="character",
       SourceMd5="character",
       DerivedMd5="character",
       Description='character',
       Tags='character',
       RDataClass="character",
       RDataVersion="character",
       SourceVersion="character",
       SourceSize="integer",
       RDataSize="integer",
       SourceLastModifiedDate="character",
       RDataLastModifiedDate="character",
       Coordinate_1_based="logical",
       Maintainer="character",
       DataProvider="character",
       BiocVersion="character",
       Notes='character',
       RDataDateAdded="character"
    )
)

### generics, getters and setters

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

# So you can do stuff like this:
# set a to be an AnnotationHubMetadata object, then:
#  metadata(a)[c("AnnotationHubRoot","Maintainer")]
#   <- list("/etc", "foo@bar.com")



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
    l <- fromJSON(pathToJson)
    l$RecipeArgs <- as.list(l$RecipeArgs)
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
    metadata(x)$AnnotationHubRoot <- ahroot
    x
}

constructAnnotationHubMetadataFromSourceFilePath <-
    function(ahroot, RDataVersion, originalFile)
{
    dir <- dirname(file.path(ahroot, originalFile))
    jsonFile <- .getDerivedFileName(originalFile, RDataVersion, "json")
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


postProcessMetadata <- function(ahroot, RDataVersion, originalFile)
{
    x <- constructAnnotationHubMetadataFromSourceFilePath(ahroot, 
        RDataVersion, originalFile)
    metadata(x)$AnnotationHubRoot <- ahroot

    derived <- file.path(ahroot, x@RDataPath)
    metadata(x)$RDataSize <- as.integer(file.info(derived)$size)
    metadata(x)$RDataLastModifiedDate <- .getModificationTime(derived)
    json <- to.json(x)
    resourceDir <- dirname(originalFile[1])
    outfile <- file.path(ahroot, resourceDir, .getDerivedFileName(originalFile, 
        RDataVersion, "json"))
    cat(json, file=outfile)
    x
}

writeJSON <- function(ahroot, metadata, flat=FALSE, filename=NULL)
{
    json <- to.json(metadata)
    sourceFile <- metadata(metadata)$SourceFile[1]
    resourceDir <- dirname(sourceFile)
    if (is.null(filename))
    {
        filename <- .getDerivedFileName(sourceFile,
            metadata(metadata)$RDataVersion, "json")
    }
    if (flat)
        outfile <- file.path(ahroot, filename)
    else
        outfile <- file.path(ahroot, resourceDir, filename)
    cat(json, file=outfile)
    outfile
}

.getDerivedFileName <- function(originalFile, RDataVersion, suffix)
{
    ret <- sub(".gz", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    ret <- sprintf("%s_%s.%s", ret, RDataVersion, suffix)
    ret
}

AnnotationHubMetadata <- function(AnnotationHubRoot, SourceFile, SourceUrl, Title,
    Description,
    Species, Genome, Recipe, RecipeArgs=list(), Tags, RDataClass,
    RDataVersion, SourceVersion, Coordinate_1_based, Maintainer,
    DataProvider,
    Notes="", RDataDateAdded)
{
    ## fixme do better than this
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)

    jsonDir <- dirname(SourceFile[1])
    resourceFile <- .getDerivedFileName(SourceFile,  RDataVersion, "RData")
    jsonFile <- .getDerivedFileName(SourceFile, RDataVersion, "json")
    resourcePath <- file.path(jsonDir, resourceFile)
#    resourcePath <- sub("\\.RData$",
#        sprintf("_%s.RData", RDataVersion), resourcePath)

    x <- new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
        SourceFile=SourceFile,
        SourceUrl=SourceUrl,
        Title=Title,
        Description=Description,
        Species=Species,
        Genome=Genome,
        Recipe=Recipe,
        RecipeArgs=RecipeArgs,
        Tags=Tags,
        RDataClass=RDataClass,
        RDataVersion=RDataVersion,
        SourceVersion=SourceVersion,
        Coordinate_1_based=Coordinate_1_based,
        Maintainer=Maintainer,
        DataProvider=DataProvider,
        Notes=Notes,
        BiocVersion=as.character(biocVersion()),
        SourceLastModifiedDate=unlist(lapply(SourceFile,
            .getModificationTime)),
        TaxonomyId=as.character(with(speciesMap, taxon[species == Species])),
        SourceMd5=unname(tools::md5sum(SourceFile)),
        RDataLastModifiedDate="1970-1-1",
        SourceSize=as.integer(file.info(SourceFile)$size),
        RDataPath=resourcePath,
        RDataDateAdded=RDataDateAdded
    )


    json <- to.json(x)

    cat(json, file=file.path(AnnotationHubRoot, jsonDir, jsonFile))

    x
}

to.json <- function(annotationHubMetadata)
{
    toJSON(to.list(annotationHubMetadata))
}

to.list <- function(annotationHubMetadata)
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
    rapply(l, as.list, "call", how="replace")
}

.AnnotationHubMetadata.validity <- function(object)
{
    rc <- new("MsgClass", name=character(0))
    e <- function(m) {
       rc$name <- c(rc$name, m)
    }

    requiredFields <- c("AnnotationHubRoot", "SourceFile", "SourceUrl", "Title",
        "Species", "Genome", "Recipe", "Tags", "RDataClass",
        "RDataVersion", "SourceVersion",
        "Coordinate_1_based", "Maintainer", "DataProvider", "RDataDateAdded")

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
    setwd(metadata(object)$AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)
    taxonomyId <- with(speciesMap, taxon[species == metadata(object)$Species])
    if (!length(taxonomyId))
        e("Unknown species")


    ## dropping this for now, this fails with ftp:// urls.
    ## emailed Hadley, hope he can fix it.
    ##headers <- HEAD(object@SourceUrl)$headers
    ##if (headers$status != "200")
    ##    e(sprintf("Can't access URL %s"), metadata(object)$SourceUrl)


    emailRegex <- 
        "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"
    if (!grepl(emailRegex, object@Maintainer, ignore.case=TRUE))
        e("Maintainer must contain an email address")

    
    # Make sure dates are valid


    if (length(rc$name) == 0) TRUE else rc$name

}

setValidity("AnnotationHubMetadata",
        function(object) .AnnotationHubMetadata.validity(object))

setMethod(show, "AnnotationHubMetadata",
    function(object)
{
    cat("class: ", class(object), '\n', sep='')
    for (slt in sort(slotNames(object)))
        cat(slt, ": ", slot(object, slt), "\n", sep="")
})
