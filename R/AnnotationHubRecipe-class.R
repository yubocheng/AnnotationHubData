setClass("AnnotationHubRecipe",
    representation(
        metadata="AnnotationHubMetadata",
        recipeName="character",
        inputFiles="character",
        outputFile="character"
        )
)
#-------------------------------------------------------------------------------
setGeneric("metadata", signature="object",
           function(object)
           standardGeneric ("metadata"))

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
    function(object, recipeFunction, ...)
        standardGeneric ("run"))




#-------------------------------------------------------------------------------
setValidity("AnnotationHubRecipe",

    function(object) {
        msg <- NULL
        metadata.msg <- validObject(object@metadata)
        if(metadata.msg != TRUE)
            msg <- c(msg, sprintf("%s\n", msg))
        for(file in inputFiles(object)) 
            if(!file.exists(file))
                msg <- c(msg, sprintf("input file '%s' does not exist\n", file))
        if(!is.null(msg))
            return(msg)
        return(TRUE)
    })
#-------------------------------------------------------------------------------
AnnotationHubRecipe <- function(metadata)
{
    x <- new("AnnotationHubRecipe")
    x@metadata <- metadata
    x@recipeName <- Recipe(metadata)

    x@inputFiles <- file.path(AnnotationHubRoot(metadata), OriginalFile(metadata))
    x@outputFile <- file.path(AnnotationHubRoot(metadata), ResourcePath(metadata))
    x
}
#-------------------------------------------------------------------------------
setMethod("show", "AnnotationHubRecipe",

    function(object) {
       cat(sprintf ("| AnnotationHubRecipe: %s\n", recipeName(object)))
       for(file in inputFiles(object))
         cat(sprintf("| inputFile: %s\n", file))
       cat(sprintf("| outputFile: %s\n", outputFile(object)))
       })
#-------------------------------------------------------------------------------
# function (object, recipe=get(getRecipeName(object), envir=getNamespace("package:AnnotationHubData"))
#                   inputFiles=getInputFiles(object),
#                   args=getRecipeArgs(object))
# do.call(recipe, c(list(inputFiles), args))
# r <- get(recipeName(recipe), envir=getNamespace("AnnotationHubData"))
# do.call(r, list(recipe))

#setMethod("run", "AnnotationHubRecipe",
#
#    function(object) {
#       recipe.function <- get(recipeName(object), envir=getNamespace("AnnotationHubData"))
#       result <- do.call(recipe.function, list(object))
#       postProcessMetadata(metadata(object)@AnnotationHubRoot, metadata(object)@OriginalFile)
#       result
#       })


#setMethod("runWild", "AnnotationHubRecipe",
#
#    function(object, recipe.function=NULL) {
#       if(is.null (recipe.function))
#         recipe.function <- get(recipeName(object), envir=getNamespace("AnnotationHubData"))
#       result <- do.call(recipe.function, list(object))
#       postProcessMetadata(metadata(object)@AnnotationHubRoot, metadata(object)@OriginalFile)
#       result
#       })

setMethod("run", "AnnotationHubRecipe",
    function(object, recipeFunction, ...) {
       if (missing(recipeFunction))
           recipeFunction <- get(recipeName(object), envir=getNamespace("AnnotationHubData"))
       result <- recipeFunction(object)
       postProcessMetadata(AnnotationHubRoot(metadata(object)), OriginalFile(metadata(object)))
       result
       })





#-------------------------------------------------------------------------------
setMethod("recipeName", "AnnotationHubRecipe",

    function(object) {
        object@recipeName
        })

#-------------------------------------------------------------------------------
setMethod("metadata", "AnnotationHubRecipe",

    function(object) {
        object@metadata
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
