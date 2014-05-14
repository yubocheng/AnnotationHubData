## setClass("AnnotationHubRecipe",
##     representation(
##         metadata="AnnotationHubMetadata",
##         recipeName="character",
##         inputFiles="character",
##         outputFile="character"
##         )
## )
#------------------------------------------------------------------------------
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




#------------------------------------------------------------------------------
## setValidity("AnnotationHubRecipe",

##     function(object) {
##         msg <- NULL
##         metadata.msg <- validObject(object@metadata)
##         if(metadata.msg != TRUE)
##             msg <- c(msg, sprintf("%s\n", msg))
##         for(file in inputFiles(object)) 
##             if(!file.exists(file))
##                 msg <- c(msg,
##                          sprintf("input file '%s' does not exist\n", file))
##         if(!is.null(msg))
##             return(msg)
##         return(TRUE)
##     })
#------------------------------------------------------------------------------
## AnnotationHubRecipe <- function(metadata)
## {
##     x <- new("AnnotationHubRecipe")
##     x@metadata <- metadata
##     x@recipeName <- metadata(metadata)$Recipe

##     ## x@inputFiles <- file.path(metadata(metadata)$AnnotationHubRoot,
##     ##                           metadata(metadata)$SourceFile)
##     x@inputFiles <- metadata(metadata)$SourceFile
    
##     x@outputFile <- file.path(metadata(metadata)$AnnotationHubRoot,
##                               metadata(metadata)$RDataPath)
##     x
## }
#------------------------------------------------------------------------------
## setMethod("show", "AnnotationHubRecipe",

##     function(object) {
##        cat(sprintf ("| AnnotationHubRecipe: %s\n", recipeName(object)))
##        for(file in inputFiles(object))
##          cat(sprintf("| inputFile: %s\n", file))
##        cat(sprintf("| outputFile: %s\n", outputFile(object)))
##        })
#------------------------------------------------------------------------------
setMethod("run", "AnnotationHubMetadata",
    function(object, recipeFunction, ...) {
       if (missing(recipeFunction))
           recipeFunction <- get(recipeName(object),
                                 envir=getNamespace("AnnotationHubData"))
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
## setMethod("metadata", "AnnotationHubRecipe",

##     function(x, ...) {
##         x@metadata
##         })

#------------------------------------------------------------------------------
setMethod("inputFiles", "AnnotationHubMetadata",
    function(object, useRoot=TRUE) {
        if(useRoot==TRUE){
            res <- file.path(metadata(object)$AnnotationHubRoot,
                             metadata(object)$SourceFile)            
        }else{
            res <- metadata(object)$SourceFile
        }
        res
    })
#------------------------------------------------------------------------------
setMethod("outputFile", "AnnotationHubMetadata",
    function(object) {
        file.path(metadata(object)$AnnotationHubRoot,
                  metadata(object)$RDataPath)
        })





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
