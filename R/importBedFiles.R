#library(AnnotationHubData)


import_wgEncodeRegDnaseClustered <- function(ahroot)
{
    params <- list()
    params$AnnotationHubRoot <- ahroot
    rp <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    files <- file.path(rp, c("wgEncodeRegDnaseClustered.bed.gz",
        "wgEncodeRegDnaseClusteredInputs.tab"))
    params$OriginalFile <- files

    #c("goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab",
    #    "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab")
    params$Species <- "Homo sapiens"
    params$Genome <- "hg19" # they are all hg19, right?
    params$Title <- "wgEncodeRegDnaseClustered"
    params$Recipe <- "wgEncodeRegDnaseClustered" # CHANGEME?
    params$Description <- "999,988 DNaseI hypersensitivity regions, combined from 75 cell types"
    params$Url <- c("http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab.gz",
        "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClustered.bed.gz")
    params$ResourceClass <- "GRanges"
    params$Version <- "0.0.1"
    params$SourceVersion <- "dateSubmitted=2011-04-28"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$DataProvider <- "hgdownload.cse.ucsc.edu/"
    params$Coordinate_1_based <- TRUE
    params$Tags <- "gene regulation"
    do.call(AnnotationHubMetadata, params)
}

