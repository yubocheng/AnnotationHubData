## ensemblFastaToFaFile <- function(ahm)
## {
##     require(Rsamtools)
##     faIn <- normalizePath(inputFiles(ahm))
##     faOut <- normalizePath(outputFile(ahm))

##     tmp <- tempfile()
##     system2("zcat", sprintf("%s > %s", faIn, tmp))
##     razip(tmp, faOut)
##     indexFa(faOut)
## }
