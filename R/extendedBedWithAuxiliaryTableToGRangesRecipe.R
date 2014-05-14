extendedBedWithAuxiliaryTableToGRanges <- function(ahm)
{
     bedFile <- grep(".bed.gz$", inputFiles(ahm), value=TRUE)
     auxFile <- grep(".tab$", inputFiles(ahm), value=TRUE)
     stopifnot(length(bedFile) == 1)
     stopifnot(length(auxFile) == 1)

     colClasses <- metadata(ahm)$RecipeArgs$bedColClasses
     tbl.bed <- read.table(gzfile(bedFile), sep="\t", header=FALSE,
                           colClasses=colClasses)
     colnames(tbl.bed) <- names(colClasses)
     
     colClasses <- metadata(ahm)$RecipeArgs$auxColClasses
     tbl.aux <- read.table(auxFile, sep="\t", colClasses=colClasses)
     colnames(tbl.aux) <- names(colClasses)

     mergeArgs <- metadata(ahm)$RecipeArgs$merge

        # TODO:  special knowledge inserted here, adding a column
        # TODO:  to tbl.aux (rowIndex) so that tables can be linked.
        # TODO:  future data sources using otherwise identical
        # TODO:  treatment may suggest more general approach.
     
     tbl.aux <- cbind(tbl.aux, rowIndex=1:nrow(tbl.aux))
     tbl <- merge(tbl.bed, tbl.aux, by.x=mergeArgs[["byX"]],
                                    by.y=mergeArgs[["byY"]],
                                    all.x=TRUE)
     tbl <- AnnotationHubData:::.sortTableByChromosomalLocation(tbl)
     colnames <- colnames(tbl)
     requiredColnames <- c("seqname", "start", "end")
     stopifnot(all(requiredColnames %in% colnames))
     otherColnames <- setdiff(colnames, requiredColnames)

     gr <- with(tbl, GRanges(seqname, IRanges(start, end)))
     mcols(gr) <- DataFrame(tbl[, otherColnames])

        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(ahm)$Species,
                                    metadata(ahm)$Genome)
        # if gr only has a subset of all possible chromosomes,
        # then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(ahm))

    outputFile(ahm)

} # extendedBedWithAuxiliaryTableToGRanges
#-------------------------------------------------------------------------------
