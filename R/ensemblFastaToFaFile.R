ensemblFastaToFaFile <- function(recipe)
{
    require(Rsamtools)
    faIn <- normalizePath(inputFiles(recipe))
    faOut <- normalizePath(outputFile(recipe))

    tmp <- tempfile()
    system2("zcat", sprintf("%s > %s", faIn, tmp))
    razip(tmp, faOut)
    indexFa(faOut)
}
