ensemblGtfToGRanges <- function(recipe)
{
  gz.inputFile <- inputFiles(recipe)[1]
  con <- gzfile(gz.inputFile)
  on.exit(close(con))
  writeLines(readLines(con), tmp <- tempfile())
  gr <- import(tmp, "gtf", asRangedData=FALSE)
  save(gr, file=outputFile(recipe))
  outputFile(recipe)

} # ensemblGtfToGRanges 
#-------------------------------------------------------------------------------
