#library(AnnotationHubData)

importEnsemblGTFs <-
    function(ahroot)
{
    ahroot <- normalizePath(ahroot)
    gtf <- dir(ahroot, pattern=".*gtf.gz", recursive=TRUE, full.names=TRUE)
    #oldwd <- getwd()
    #on.exit(setwd(oldwd))
    #setwd(ahroot)
    gtf <- sub(ahroot, "", gtf, fixed=TRUE)
    gtf <- sub(sprintf("^%s", .Platform$file.sep), "", gtf)
    rdata <- sub(".gz$", ".RData", gtf)

    regex <- "^([[:alpha:]_]+)\\.([[:alpha:]]+).*"
    title <- sub(".gz$", "", basename(gtf))
    species <- gsub("_", " ", sub(regex, "\\1", title), fixed=TRUE)
    genome <- sub(regex, "\\2", title)

    description <- paste("Gene Annotation for", species)
    sourceUrl <- paste0("ftp://ftp.ensembl.org/", gtf)
    sourceVersion <- sub(".*(release-[[:digit:]]+).*", "\\1", gtf)
    rDataDateAdded <- format(Sys.time(), "%Y-%m-%d %T")

    Map(importOneGTF, gtf=gtf, rdata=rdata, title=title,
        species=species, genome=genome, description=description,
        sourceUrl=sourceUrl, sourceVersion=sourceVersion,
        MoreArgs=list(ahroot=ahroot, rDataDateAdded=rDataDateAdded))
}

importOneGTF <-
    function(ahroot, gtf, rdata, species, genome, title, description,
             sourceUrl, sourceVersion, rDataDateAdded)
{
    message(gtf)
    x = AnnotationHubMetadata(
      AnnotationHubRoot = ahroot,
      SourceFile = gtf,
      Species = species,
      Genome = genome,
      Title = title,
      Recipe = "recipe_ensembl_gtf",
      Description = description,
      SourceUrl = sourceUrl,
      RDataClass = "GRanges",
      RDataVersion = "0.0.1",
      SourceVersion = sourceVersion,
      Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
      DataProvider = "ftp.ensembl.org",
      Coordinate_1_based = TRUE,
      RDataDateAdded = rDataDateAdded,
      Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation"))
    postProcessMetadata(ahroot, metadata(x)$RDataVersion,
                        metadata(x)$SourceFile)
}
