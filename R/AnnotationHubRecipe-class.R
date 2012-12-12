setClass("AnnotationHubRecipe",
    representation(
        metadata="AnnotationHubMetadata",
        annotationHubRoot="character",
        recipeName="character",
        inputFiles="character",
        outputFile="character"
        )
)

#-------------------------------------------------------------------------------
setGeneric("annotationHubRoot", signature="object",
           function(object)
           standardGeneric ("annotationHubRoot"))

setGeneric("recipeName", signature="object",
           function(object)
           standardGeneric ("recipeName"))

setGeneric("inputFiles", signature="object",
           function(object)
           standardGeneric ("inputFiles"))

setGeneric("outputFile", signature="object",
           function(object)
           standardGeneric ("outputFile"))

setGeneric("run", signature="object",
           function(object, functionName=NA, inputFiles=NA)
           standardGeneric ("run"))

#-------------------------------------------------------------------------------
setValidity("AnnotationHubRecipe",

    function(object) {
        msg <- NULL
        #validMetadata <- validObject(object@metadata)
        #if(is.character(validMetadata))
        #    msg <- c(msg, validMetadata)
     
        if(!is.null(msg))
            return(msg)
     
        return(TRUE)
    })
#-------------------------------------------------------------------------------
# ctor
AnnotationHubRecipe <- function(metadata)
{
    x <- new("AnnotationHubRecipe")
    x@metadata <- metadata
    x@recipeName <- metadata@Recipe
    x@annotationHubRoot <- metadata@AnnotationHubRoot
    x@inputFiles <- metadata@OriginalFile
    x@outputFile <- file.path(metadata@AnnotationHubRoot, metadata@ResourcePath)
    x
}
#-------------------------------------------------------------------------------
setMethod("show", "AnnotationHubRecipe",

    function(object) {
       msg = sprintf ("AnnotationHubRecipe object")
       cat (msg, "\n", sep="")
       })
#-------------------------------------------------------------------------------
# function (object, recipe=get(getRecipeName(object), envir=getNamespace("package:AnnotationHubData"))
#                   inputFiles=getInputFiles(object),
#                   args=getRecipeArgs(object))
# do.call(recipe, c(list(inputFiles), args))

setMethod("run", "AnnotationHubRecipe",

    function(object, functionName=NA, inputFiles=NA) {
       #browser('run')
       if(is.na(functionName)) {
           functionName <- recipeName(object)
           inputFiles <- inputFiles(object)
           cmd <- sprintf("%s('%s')", functionName, inputFiles)
         } else {
           cmd <- sprintf("%s('%s')", functionName, inputFiles)
         }
       printf("recipe as function call: %s", cmd)
       eval(parse(text=cmd))
       })

#-------------------------------------------------------------------------------
recipe1 <- function(inputDataFileName)
{
    printf("recipe1, word count in %s: %d", inputDataFileName, 
           length(scan(inputDataFileName, what=character(), quiet=TRUE)))
}
#-------------------------------------------------------------------------------
bedFileRecipe <- function(inputFiles)
{
    browser()
    stopifnot(length(inputFiles) == 1)   # for now.
  
    if(grep("\\.gz$", inputFiles[1]))
        connection <- gzfile(inputFiles[1])
    else
        connection <- file(inputfiles[1])
    tbl <- read.table(connection, sep="\t", as.is=TRUE)
       
}
#-------------------------------------------------------------------------------
setMethod("recipeName", "AnnotationHubRecipe",

    function(object) {
        object@recipeName
        })

#-------------------------------------------------------------------------------
setMethod("annotationHubRoot", "AnnotationHubRecipe",

    function(object) {
        object@annotationHubRoot
        })

#-------------------------------------------------------------------------------
setMethod("inputFiles", "AnnotationHubRecipe",

    function(object) {
        object@inputFiles
        })
#-------------------------------------------------------------------------------
setMethod("outputFile", "AnnotationHubRecipe",

    function(object) {
        object@outputFile
        })
#-------------------------------------------------------------------------------
