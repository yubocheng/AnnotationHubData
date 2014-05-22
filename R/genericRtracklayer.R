rtrackLayerImport <- function (ahm)
{
    require('rtracklayer')
    gr <- import(inputFiles(ahm)[1], asRangedData=FALSE)
    newSeqInfo <- constructSeqInfo(metadata(ahm)$Species,
                                    metadata(ahm)$Genome) 
        # if gr only has a subset of all possible chromosomes, then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(ahm))
    outputFile(ahm)
}
