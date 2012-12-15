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
           function(object)
           standardGeneric ("run"))

setGeneric("runWild", signature="object",
           function(object, recipe.function=NULL)
           standardGeneric ("runWild"))

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
    x@inputFiles <- file.path(metadata@AnnotationHubRoot, metadata@OriginalFile)
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
# r <- get(recipeName(recipe), envir=getNamespace("AnnotationHubData"))
# do.call(r, list(recipe))

setMethod("run", "AnnotationHubRecipe",

    function(object) {
       recipe.function <- get(recipeName(object), envir=getNamespace("AnnotationHubData"))
       result <- do.call(recipe.function, list(object))
       postProcessMetadata(annotationHubRoot(object), object@metadata@OriginalFile)
       result
       })


setMethod("runWild", "AnnotationHubRecipe",

    function(object, recipe.function=NULL) {
       if(is.null (recipe.function))
         recipe.function <- get(recipeName(object), envir=getNamespace("AnnotationHubData"))
       result <- do.call(recipe.function, list(object))
       postProcessMetadata(annotationHubRoot(object), object@metadata@OriginalFile)
       result
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
# the GRanges that we assemble here need SeqInfo (which is a generalized name for
# what is typically chromosome info:  chromosome name, chromosome length and
# circularity (common among prokaryotic organisms)
constructSeqInfo <- function(species, genome)
{
  stopifnot(species=="Homo sapiens" & genome %in% c("hg18", "hg19"))
  suppressMessages({
         # chroms 1-22, X, Y, M are assumed to be the first 25 rows of the data.frame
     tbl.chromInfo = GenomicFeatures:::.makeUCSCChrominfo (genome,
                                        circ_seqs=character(0)) [1:25,]
                   })

   Seqinfo (as.character(tbl.chromInfo$chrom), 
            seqlengths=tbl.chromInfo$length, 
            isCircular=rep(FALSE, nrow (tbl.chromInfo)), genome=genome)

} # constructSeqInfo
#-------------------------------------------------------------------------------
.sortTableByChromosomalLocation <- function(tbl)
{
  stopifnot (all (c ('seqname', 'start') %in% colnames (tbl)))
  factor.chromNames <- factor (tbl$seqname, levels=paste("chr", c(1:22, "X", "Y", "M"), sep=''))
  tbl$seqname <- factor.chromNames
  tbl <- tbl [order (tbl$seqname, tbl$start), ]
  invisible (tbl)

} # .sortTableByChromsomalLocation 
#------------------------------------------------------------------------------------------------------------------------
