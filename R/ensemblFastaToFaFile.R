ensemblFastaToFaFile <- function(recipe)
{
    require(Rsamtools)
    faIn <- normalizePath(inputFiles(recipe))
    faOut <- normalizePath(outputFile(recipe))
    if (!identical(faIn, faOut))
        file.copy(faIn, faOut)
    indexFa(faOut)
    faOut
}
