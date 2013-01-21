rtrackLayerImport <- function (recipe)
{
    gr <- import(inputFiles(recipe)[1], asRangedData=FALSE)
    newSeqInfo <- constructSeqInfo(metadata(recipe@metadata)$Species,
                                    metadata(recipe@metadata)$Genome) 
        # if gr only has a subset of all possible chromosomes, then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(recipe))
    outputFile(recipe)
}
