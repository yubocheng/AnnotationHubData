extendedBedToGRanges <- function(recipe)
{
    colClasses <- recipe@metadata@RecipeArgs$colClasses
    colnames <- names(colClasses)
    unused <- which(colnames == "")
    if(length(unused) > 0)
        colnames <- colnames[-unused]

    requiredColnames <- c("seqnames", "start", "end", "strand")
    stopifnot(all(requiredColnames %in% colnames))
    otherColnames <- setdiff(colnames, requiredColnames)

    tbl <- read.table(inputFiles(recipe)[1], sep="\t", header=FALSE, colClasses=colClasses)
    colnames(tbl) <- colnames

    gr <- with(tbl, GRanges(seqnames, IRanges(start, end), strand))
    mcols(gr) <- DataFrame(tbl[, otherColnames])

        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(recipe@metadata@Species, recipe@metadata@Genome) 
        # if gr only has a subset of all possible chromosomes, then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    postProcessMetadata(annotationHubRoot(recipe), recipe@metadata@OriginalFile)
    save(gr, file=outputFile(recipe))

    outputFile(recipe)

} # extendedBedToGRanges
#-------------------------------------------------------------------------------
