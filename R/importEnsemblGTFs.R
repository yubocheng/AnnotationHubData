#library(AnnotationHubData)

importEnsemblGTFs <- function(ahroot)
{
    ahroot <- normalizePath(ahroot)
    gtf <- dir(ahroot, pattern=".*gtf.gz", recursive=TRUE, full=TRUE)
    #oldwd <- getwd()
    #on.exit(setwd(oldwd))
    #setwd(ahroot)
    gtf <- sub(ahroot, "", gtf)
    gtf <- sub(sprintf("^%s", .Platform$file.sep), "", gtf)
    rda <- sub(".gz$", ".RData", gtf)
    map <- setNames(rda, gtf)

    gtfs <- names(map)
    rdatas <- unname(map)

    for (i in 1:length(gtfs))
    {
        importOneGTF(ahroot, gtfs[i], rdatas[i])
    }

}


importOneGTF <- function(ahroot, gtf, rdata)
{
    print(gtf)
    params <- list()
    params$AnnotationHubRoot <- ahroot
    params$OriginalFile <- gtf
    params$Species <- strsplit(basename(gtf), ".", fixed=TRUE)[[1]][1]
    params$Species <- gsub("_", " ", params$Species, fixed=TRUE)
    params$Genome <- strsplit(basename(gtf), ".", fixed=TRUE)[[1]][2]
    params$Title <- sub(".gz", "", basename(gtf), fixed=FALSE)
    params$Recipe <- "recipe_ensembl_gtf"
    params$Description <- sprintf("Gene Annotation for %s",
        params$Species)
    params$Url <- sprintf("ftp://%s", gtf)
    #OriginalFile <- rdata
    params$ResourceClass <- "GRanges"
    #objName <- load(rdata)
    #gr <- get(objName)
    #params$ResourceDimensions <- 
    #    sprintf("GRanges with %s ranges and %s metadata columns", 
    #        length(ranges(gr)), length(mcols(gr)))
    params$Version <- "0.0.1"
    params$SourceVersion <- "release-69"
    params$Maintainer <- "Martin Morgan <mtmorgan@fhcrc.org>"
    params$DataProvider <- "ftp.ensembl.org"
    params$Coordinate_1_based <- TRUE ## FIXME I really have no idea...
    params$Tags <-
        c("GTF", "ensembl", "Gene", "Transcript", "Annotation")
    do.call("AnnotationHubMetadata", params)
}