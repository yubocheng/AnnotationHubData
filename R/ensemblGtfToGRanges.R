ensemblGtfToGRanges <- function(ahm)
{
    require("rtracklayer")
    gz.inputFile <- inputFiles(ahm)[1]
    con <- gzfile(gz.inputFile)
    on.exit(close(con))
    gr <- import(con, "gtf", asRangedData=FALSE)
    save(gr, file=outputFile(ahm))
    outputFile(ahm)

} # ensemblGtfToGRanges 
#-------------------------------------------------------------------------------
