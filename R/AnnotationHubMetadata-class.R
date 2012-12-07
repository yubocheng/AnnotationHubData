setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)



setClass("AnnotationHubMetadata",
    representation(
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
       SourceLastModifiedDate="POSIXct",
       DerivedLastModifiedDate="POSIXct",
       Coordinate_1_based="logical",
       Maintainer="character",
       DataProvider="character",
       BiocVersion="character",
       Notes='character'
    )
)


AnnotationHubMetadata <- function(localFile, Url, Title,
    Description,
    Species, Genome, Recipe, Tags, ResourceClass,
    Version, SourceVersion, Coordinate_1_based, Maintainer,
    DataProvider,
    Notes="no notes!")
{
    if(missing(localFile)) stop("localFile is required.")

    # UNCOMMENT ME if (!exists("speciesMap")) data(speciesMap)
    x <- new("AnnotationHubMetadata")
    f <- formals()
    print(f)
    m <- match.call()
    print(m)
    for (i in names(f))
    {
        if (i != "localFile")
        {
            print(sprintf("i == %s, m[[i]] == %s", i, m[[i]]))
            if (length(m[[i]]))
                slot(x, i) <- m[[i]]
            else if (length(f[[i]]))
                slot(x, i) <- f[[i]]

        }
    }
    print(x)
    # is there a DRYer way to do all this:
    x@BiocVersion <- as.character(biocVersion())
    x@SourceLastModifiedDate <- as.POSIXct("1/1/1970")
    #     strsplit(as.character(file.info(rdata)$mtime), " ")[[1]][1]
    x@DerivedLastModifiedDate <- as.POSIXct("1/1/1970")


#    if(is.na(dcfFile)) {
#        x@dcfFile = NA_character_
#        return(x)
#    }
#    x@dcfFile <- dcfFile
#    dv <- read.dcf(dcfFile)[1,]
#    x@title <- dv[['Title']]
#    x@dataFile <- dv[['File']]
#
    validObject(x)
    x
}



setValidity("AnnotationHubMetadata", function(object)
{
    print(paste("Url is", object@Url))
    # UNCOMMENT ME AT SOME POINT if (!exists("speciesMap")) data(speciesMap)
    rc <- new("MsgClass", name=character(0))

    e <- function(m) {
       rc$name <- c(rc$name, m)
    }


    requiredFields <- c("Url", "Title",
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