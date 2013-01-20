rtrackLayerImport <- function (recipe)
{
   gr <- import(inputFiles(recipe)[1], asRangedData=FALSE)
   save(gr, file=outputFile(recipe))
   outputFile(recipe)
}
