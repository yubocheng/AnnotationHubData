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
    metadata(x)$AnnotationHubRoot <- ahroot

    derived <- file.path(ahroot, x@ResourcePath)
    metadata(x)$DerivedSize <- as.integer(file.info(derived)$size)
    metadata(x)$DerivedLastModifiedDate <- .getModificationTime(derived)
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
    setwd(metadata(object)$AnnotationHubRoot)

    if (!exists("speciesMap")) data(speciesMap)
    taxonomyId <- with(speciesMap, taxon[species == metadata(object)$Species])
    if (!length(taxonomyId))
        e("Unknown species")


    ## dropping this for now, this fails with ftp:// urls.
    ## emailed Hadley, hope he can fix it.
    ##headers <- HEAD(object@Url)$headers
    ##if (headers$status != "200")
    ##    e(sprintf("Can't access URL %s"), metadata(object)$Url)


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