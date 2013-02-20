.makeAuxTable <- function(n, auxFiles, recipe){
    
     colClasses <- metadata(recipe@metadata)$RecipeArgs$auxColClasses[n][[1]]$cols
     auxFile <- auxFiles[n]
     tbl.aux <- read.table(auxFile, sep="\t", colClasses=colClasses)
     colnames(tbl.aux) <- names(colClasses)
     tbl.aux
}

.getMergeArgs <- function(n, recipe){
    metadata(recipe@metadata)$RecipeArgs$auxColClasses[n][[1]]$merge
}

trackWithAuxiliaryTablesToGRanges <- function(recipe)
{
     mainFile <- inputFiles(recipe)[1] ## always the 1st one? - discuss with Dan and Paul
     auxFiles <- inputFiles(recipe)[-1]
     if(!(length(mainFile) == 1)) stop("No files present in input json.") 
     if(!(length(auxFiles) >= 1)) stop("No auxiliary files listed in input json.  Wrong recipe?")

     colClasses <- metadata(recipe@metadata)$RecipeArgs$mainColClasses
     tbl.main <- read.table(gzfile(mainFile), sep="\t", header=FALSE,
                           colClasses=colClasses)
     colnames(tbl.main) <- names(colClasses)

     auxLen <- length(auxFiles)
     ## a couple for loops because we need to know 'n'...
     auxTabs <- list()
     for(i in seq_len(auxLen)){
         auxTabs[[i]] <- .makeAuxTable(i, auxFiles, recipe)
     } 
     mergeArgs <- list()
     for(i in seq_len(auxLen)){
         mergeArgs[[i]] <- .getMergeArgs(i, recipe)
     } 
     
     ## merge together uses for loop again (to concentrate result down to one thing)
     for(i in  seq_len(auxLen)){
         if(i ==1){tbl <- tbl.main}
         ## otherwise recycle
         tbl <- merge(tbl, auxTabs[[i]], by.x=mergeArgs[[i]][["byX"]],
                       by.y=mergeArgs[[i]][["byY"]],
                       all.x=TRUE)  
     }
     
     tbl <- AnnotationHubData:::.sortTableByChromosomalLocation(tbl)
     colnames <- colnames(tbl)
     requiredColnames <- c("seqname", "start", "end")
     stopifnot(all(requiredColnames %in% colnames))
     otherColnames <- setdiff(colnames, requiredColnames)

     gr <- with(tbl, GRanges(seqname, IRanges(start, end)))
     mcols(gr) <- DataFrame(tbl[, otherColnames])

        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(recipe@metadata)$Species,
                                    metadata(recipe@metadata)$Genome)
        # if gr only has a subset of all possible chromosomes,
        # then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(recipe))

    outputFile(recipe)

} # trackWithAuxiliaryTableToGRanges
#-------------------------------------------------------------------------------
