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

.organismToTaxid <- function(organism=character())
{
    ## query NCBI for taxonomy ID
    .eutils <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils"
    
    ## 1. ids
    uorganism <- unique(organism[!is.na(organism)])
    query <- paste(uorganism, collapse=" OR ")
    url <- sprintf("%s/esearch.fcgi?db=taxonomy&term=%s&retmax=%d",
                   .eutils, query, length(uorganism))
    xml <- XML::xmlParse(url)
    
    ## 2. records
    id <- as.character(sapply(xml["//Id/text()"], XML::xmlValue))
    scin <- taxid <- character()
    if (length(id)) {
        query2 <- paste(id, collapse=",")
        url <- sprintf("%s/efetch.fcgi?db=taxonomy&id=%s&retmax=%d",
                       .eutils, query2, length(uorganism))
        xml <- XML::xmlParse(url)
        scin <- sapply(xml["/TaxaSet/Taxon/ScientificName"], XML::xmlValue)
        taxid <- sapply(xml["/TaxaSet/Taxon/TaxId/text()"], XML::xmlValue)
    }
    
    ## there are 3 special cases:
    #a) query ="Pongo pygmaeus abelii", scin="Pongo abelii", taxid="9601"
    #b) query ="Xenopus tropicalis", scin="Xenopus (Silurana) tropicalis", taxid="8364"
    #c) query ="Spermophilus tridecemlineatus", scin="Ictidomys tridecemlineatus", taxid="43179"
    
    tax_ind <- match(c("9601","8364","43179"), taxid)
    scin_ind <- match(c("Pongo abelii","Xenopus (Silurana) tropicalis",
             "Ictidomys tridecemlineatus"), scin)
    if(identical(tax_ind, scin_ind))
        scin[scin_ind] <- c("Pongo pygmaeus abelii","Xenopus tropicalis",
                            "Spermophilus tridecemlineatus")
    
    ## 3. Results
    as.integer(taxid)[match(organism, scin)]
}

.getTaxGenome <- function(rsrc)
{
    ga <- rtracklayer::ucscGenomes()
    idx <- match(rsrc$from, ga$db)
    rsrc$organism <- as.character(ga[idx, "organism"])
    rsrc$organism[which(is.na(rsrc$organism))] <- NA_character_
    rsrc$taxid <- .organismToTaxid(rsrc$organism)
    rsrc$taxid[which(is.na(rsrc$taxid))]<- NA_character_
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
    species <- rsrc$organism            
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
