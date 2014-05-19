## define Three kinds of recipes to indicate three different kinds of input.
broadpeakBedToGRanges <- function(ahm){
    colClasses <- list(seqnames="character",
                       start="integer",
                       end="integer",
                       name="character",
                       score="integer",
                       strand="character",
                       signalValue="numeric",
                       pValue="numeric",
                       qValue="numeric")
    extendedBedToGRanges(ahm, colClasses)
}
narrowpeakBedToGRanges <- function(ahm){
    colClasses <- list(seqnames="character",
                       start="integer",
                       end="integer",
                       name="character",
                       score="integer",
                       strand="character",
                       signalValue="numeric",
                       pValue="numeric",
                       qValue="numeric",
                       peak="integer")
    extendedBedToGRanges(ahm, colClasses)
}
bedrnaelementsToGRanges <- function(ahm){
    colClasses <- list(seqnames="character",
                       start="integer",
                       end="integer",
                       name="character",
                       score="integer",
                       strand="character",
                       level="numeric",
                       signif="numeric",
                       score2="integer")
    extendedBedToGRanges(ahm, colClasses)
}

## The above all just need to do what is in here:
extendedBedToGRanges <- function(ahm, colClasses)
{
    ## colClasses <- metadata(ahm)$RecipeArgs$colClasses
    if(colClasses[1] == 'implicit') {
           # TODO: if a strand column can be deduced, it SHOULD be deduced.
           # TODO: pshannon (10 jan 2013)
        tbl <- read.table(inputFiles(ahm)[1], sep="\t", header=FALSE)
        columnCount <- ncol(tbl)
        mandatory.colnames <- c("seqnames", "start", "end")
        if (columnCount > 3) {
            implicitColumnCount <- ncol(tbl) - 3
            implicitColumnNumbers <- 4:(3+implicitColumnCount)
            implicit.colnames = sprintf("col.%02d", implicitColumnNumbers)
            colnames <- c(mandatory.colnames, implicit.colnames)
            colnames(tbl) <- colnames
            gr <- with(tbl, GRanges(seqnames, IRanges(start, end)))
            other.colnames <- setdiff(colnames, mandatory.colnames)
            mcols(gr) <- DataFrame(tbl[, other.colnames])

            }
    } else {
        colnames <- names(colClasses)
        unused <- which(colnames == "")
        if(length(unused) > 0)
            colnames <- colnames[-unused]
        required.colnames <- c("seqnames", "start", "end", "strand")
        stopifnot(all(required.colnames %in% colnames))
        other.colnames <- setdiff(colnames, required.colnames)
        tbl <- read.table(inputFiles(ahm)[1], sep="\t", header=FALSE, colClasses=colClasses)
        colnames(tbl) <- colnames
        new.strand <- sub(".", "*", tbl$strand, fixed=TRUE)
        gr <- with(tbl, GRanges(seqnames, IRanges(start, end), new.strand))
        mcols(gr) <- DataFrame(tbl[, other.colnames])
        }

        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(ahm)$Species,
                                    metadata(ahm)$Genome) 
        # if gr only has a subset of all possible chromosomes, then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(ahm))

    outputFile(ahm)

} # extendedBedToGRanges
#-------------------------------------------------------------------------------
