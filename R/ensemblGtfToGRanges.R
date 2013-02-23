ensemblGtfToGRanges <- function(recipe)
{
  gz.inputFile <- inputFiles(recipe)[1]
  con <- gzfile(gz.inputFile)
  on.exit(close(con))
  gr <- import(con, "gtf", asRangedData=FALSE)
  save(gr, file=outputFile(recipe))
  outputFile(recipe)

} # ensemblGtfToGRanges 
#-------------------------------------------------------------------------------
