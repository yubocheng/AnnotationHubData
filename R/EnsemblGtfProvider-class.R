EnsemblGtfProvider <-
    setClass("EnsemblGtfProvider", contains="DataProvider")

.ensemblGtfSourceUrls <-
    function(url)
    ## retrieve all possible files from Ensembl
{
    ## releases
    lst <- RCurl::getURL(url=url, dirlistonly=TRUE, followlocation=TRUE)
    lst <- strsplit(lst, "\n")[[1]]
    releases <- paste0(url, lst)
    want <- paste0(grep(".*release-(69|7[[:digit:]])", releases, value=TRUE),
                   "/gtf/")
    ## files in release
    sapply(want, function(url) {
        listing <- getURL(url=url, followlocation=TRUE, customrequest="LIST -R")
        listing<- strsplit(listing, "\n")[[1]]
        subdir <- sub(".* ", "", listing[grep("^drwx", listing)])
        gtfGz <- sub(".* ", "", listing[grep(".*gtf.gz$", listing)])
        sprintf("%s%s/%s", url, subdir, gtfGz)
    })
}

.ensemblGtfMetadata <-
    function(baseUrl, sourceUrl)
{
    gtf <- sub(baseUrl, "ensembl/", sourceUrl)
    rdata <- sub(".gz$", ".RData", gtf)
    regex <- "^([[:alpha:]_]+)\\.([[:alpha:]]+).*"
    title <- sub(".gz$", "", basename(gtf))
    species <- gsub("_", " ", sub(regex, "\\1", title), fixed=TRUE)
    genome <- sub(regex, "\\2", title)
    description <- paste("Gene Annotation for", species)

    sourceVersion <- sub(".*(release-[[:digit:]]+).*", "\\1", sourceUrl)
    rDataDateAdded <- 
    
    Map(AnnotationHubMetadata, AnnotationHubRoot=NA_character_,
        Description=description, Genome=genome, SourceFile=gtf,
        SourceUrl=sourceUrl, SourceVersion=sourceVersion,
        Species=species, Title=title,
        MoreArgs=list(
          Coordinate_1_based = TRUE,
          DataProvider = "ftp.ensembl.org",
          Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
          RDataClass = "GRanges",
          RDataDateAdded = format(Sys.time(), "%Y-%m-%d %T"),
          RDataVersion = "0.0.1",
          Recipe = c("ensemblGtfToGRangesRecipe", package="AnnotationHubData"),
          Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
}

setMethod(newResources, "EnsemblGtfProvider",
    function(object, ahmeta)
{
    baseUrl <- "ftp://ftp.ensembl.org/pub/"
    sourceUrls <- .ensemblGtfSourceUrls(baseUrl)

    ## filter known
    knownUrls <- sapply(ahmeta, function(elt) metadata(ahmeta)$SourceUrl)
    sourceUrls <- sourceUrls[!sourceUrls %in% knownUrls]

    ## AnnotationHubMetadata
    .ensemblGtfMetadata(baseUrl, sourceUrls)
})
