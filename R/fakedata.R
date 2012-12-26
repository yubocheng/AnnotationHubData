
import_fakeData <- function(ahroot, version)
{
    params <- list()
    params$Species <- "Homo sapiens"
    params$Genome <- "hg19"
    params$Recipe <- "unknown" # FIXME
    params$RecipeArgs <- list() # FIXME
    params$RDataClass <- "GRanges"
    params$RDataVersion <- version
    params$Maintainer <- "Dan Tenenbaum <dtenenba@fhcrc.org>"
    params$DataProvider <- "bioconductor.org"
    params$Coordinate_1_based <- TRUE
    params$AnnotationHubRoot <- ahroot
    params$SourceFile <- "fakedata/data.bed.gz"
    params$Title <- "Fake versioned data"
    params$Description <- "fake versioned data"
    params$SourceUrl <-"http://bioconductor.org/fakedata/data.bed.gz"
    params$SourceVersion <- "dateSubmitted=2011-04-28"
    params$Tags <- "fake"
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot,  metadata(x)$RDataVersion, metadata(x)$SourceFile)
    x
}

import_all_fake_data <- function(ahroot)
{
    import_fakeData(ahroot, "0.0.1")
    import_fakeData(ahroot, "0.0.2")
}