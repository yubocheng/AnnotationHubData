.ucscChainBase <- "http://hgdownload.cse.ucsc.edu/"

.get1ChainResource <- function(url, verbose=FALSE) {
    require(XML)
    tryCatch({
        if (verbose)
            message(basename(dirname(url)))
        html <- htmlParse(url)
        fls <- sapply(html["//pre[2]/a/text()"], xmlValue)
        url <- sprintf("%s/%s", url, fls)
        keep <- grepl("chain.gz", fls)
        stamps <- sapply(html["//pre[2]/text()"], xmlValue)
        regex <- " *([[:alnum:]-]+) *([:[:alnum:]]+)*.*"
        version <- sub(regex, "\\1_\\2", stamps)
        data.frame(url, version, stringsAsFactors=FALSE)[keep,,drop=FALSE]
    }, error=function(err) {
        warning(basename(dirname(url)), ": ", conditionMessage(err))
        data.frame(url=character(), version=character(), stringsAsFactors=FALSE)
    })
}

.getUCSCChainResources <-
    function(verbose=FALSE)
{
    .chainBase <- sprintf("%sgoldenPath", .ucscChainBase)
    genomes <- rtracklayer::ucscGenomes()$db
    urls <- sprintf("%s/%s/liftOver", .chainBase, genomes)
    rsrc <- do.call(rbind, lapply(urls, .get1ChainResource, verbose=verbose))
    rownames(rsrc) <- basename(rsrc$url)
    rsrc
}

.parseFileName <- function(table)
{
    table$from <-
        sub("^([a-z][[:alnum:]]+)To.*.over.chain.gz", "\\1",
            rownames(table))
    to <- sub(".*To([A-Z][[:alnum:]]+)\\.over.chain.gz", "\\1", rownames(table))
    table$to <- sub("^([A-Z])", "\\L\\1", to, perl=TRUE)
    table
}

.getTaxGenome <- function(rsrc)
{
    ga <- genomeAssemblies()
    idx <- match(rsrc$from, ga$UCSC_assembly_ID)
    rsrc$taxid <- as.character(ga[idx, "Taxon_ID"])
    rsrc$species <- ga[idx, "Scientific_Name"]
    rsrc
}

makeUCSCChain <- function(currentMetadata) {
    rsrc <- .getUCSCChainResources()
    rsrc <- .parseFileName(rsrc)
    rsrc <- .getTaxGenome(rsrc)
    
    description <- sprintf("UCSC liftOver chain file from %s to %s",
                           rsrc$from, rsrc$to)
    genome <- rsrc$from
    sourceFile <- rownames(rsrc)
    sourceUrls <- sub(.ucscChainBase, "", rsrc$url)
    sourceVersion <- rsrc$version
    species <- rsrc$species            
    taxonomyId <- rsrc$taxid           
    title <- rownames(rsrc)

    Map(AnnotationHubMetadata,
        Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title,
        MoreArgs=list(
          Coordinate_1_based = FALSE,
          DataProvider = "hgdownload.cse.ucsc.edu",
          Location_Prefix = .ucscChainBase,
          Maintainer = "Sonali Arora <sarora@fhcrc.org>",
          RDataClass = "ChainFileResource",
          RDataDateAdded = Sys.time(),
          RDataVersion = "0.0.1",
          RDataPath = NA_character_,
          Recipe = NA_character_,
          Tags = c("liftOver", "chain", "UCSC", "genome", "homology")))
}

makeAnnotationHubResource("UCSCChainPreparer", makeUCSCChain)
