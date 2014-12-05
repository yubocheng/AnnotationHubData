.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.get1Resource <- function(url, fileName, genome, verbose=FALSE) {
    require(XML)
    tryCatch({
        if (verbose)
            message(basename(dirname(url)))

        html <- htmlParse(url)
        fls <- sapply(html["//pre[2]/a/text()"], xmlValue)
        if(length(fls)==0)
            fls <- sapply(html["//pre[1]/a/text()"], xmlValue)
        
        ## extract the file name
        url <- sprintf("%s/%s", url, fls)
        keep <- grepl(paste0(fileName, "$"), fls)
        url <- url[keep]

        result <- lapply(url, function(f) {
            h <- basicTextGatherer()
            ok <- curlPerform(url=f,
                              nobody=TRUE, headerfunction=h$update)
            yy <- h$value()
            list(date=sub(".*Last-Modified: ([[:print:]]+) GMT.*", "\\1", yy),
                 size=sub(".*Content-Length: ([[:digit:]]+).*","\\1", yy))
        })
        date <- strptime(sapply(result, "[[", "date"),
                         "%a, %d %b %Y %H:%M:%S", tz="GMT")
        size <- as.integer(sapply(result, "[[", "size"))
                
        data.frame(url, date, size, stringsAsFactors=FALSE)
    }, error=function(err) {
            warning(basename(dirname(url)), ": ", conditionMessage(err))
            data.frame(url=character(), version=character(), 
                stringsAsFactors=FALSE)
    })
}

.organismToTaxid <- function(organism=character()) {
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
    
    table <- c("Pongo abelii","Xenopus (Silurana) tropicalis",
               "Ictidomys tridecemlineatus")
    idx <- match(scin, table)
    updt <- !is.na(idx)
    if (any(updt))
        scin[updt] <- table[idx[updt]]
    
    ## 3. Results
    as.integer(taxid[match(organism, scin)])
}

.getUCSCResources <- function(fileType, dirName, fileName, verbose=FALSE) {
    ## get resource from UCSC
    .fileBase <- sprintf("%sgoldenPath", .ucscBase)
    genome_tbl <- rtracklayer::ucscGenomes(organism=TRUE)
    genomes <- genome_tbl$db
    ## remove faulty genome. 
    rm <- c("cb1", "eboVir3", "dp2", "strPur1", "ci1", "calMil1","monDom1", 
            "balAcu1" ,"musFur1")
    genomes <- setdiff(genomes, rm)
        
    urls <- sprintf("%s/%s/%s", .fileBase, genomes, dirName)
    rsrc <- do.call(rbind, lapply(urls, .get1Resource,  
        fileName=fileName, verbose=verbose))
    rownames(rsrc) <- basename(rsrc$url)

    
    ## parse the filename for each file type.
    switch(fileType, chain={
        rsrc$from <- sub("^([[:alnum:]]+)To[A-Z].*", "\\1", rownames(rsrc))
        rsrc$to <- sub(".*To([A-Z])([[:alnum:]]+).*", "\\L\\1\\E\\2",
                       rownames(rsrc), perl=TRUE)
    }, "2bit"={
        rsrc$from <- sub(".2bit","", rownames(rsrc))
    }, {
        stop("unknown fileType ", sQuote(fileType))
    })
    
    ## add the organism 
    idx <- match(rsrc$from, genome_tbl$db)
    rsrc$organism <- rep(NA_character_, length(idx))
    rsrc$organism[!is.na(idx)] <- genome_tbl[idx[!is.na(idx)], "organism"]
    
    ## add the taxonmy Id. 
    rsrc$taxid <- rep(NA_character_, length(idx))
    rsrc$taxid[!is.na(idx)] <- .organismToTaxid(rsrc$organism[!is.na(idx)])
    
    rsrc
}

makeUCSCChain <- function(currentMetadata) {
    rsrc <- .getUCSCResources(fileType="chain", dirName="liftOver", 
        fileName="chain.gz", verbose=FALSE)
    description <- sprintf("UCSC liftOver chain file from %s to %s",
        rsrc$from, rsrc$to)
    genome <- rsrc$from
    sourceFile <- rownames(rsrc)
    sourceUrls <- sub(.ucscBase, "", rsrc$url)
    sourceVersion <- sapply(rsrc$date, function(y) gsub(" ","_",y)) 
    species <- rsrc$organism            
    taxonomyId <- as.integer(rsrc$taxid)           
    title <- rownames(rsrc)
    SourceLastModifiedDate <- rsrc$date
    SourceSize <- as.numeric(rsrc$size)
        
    Map(AnnotationHubMetadata,
        Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title,
        MoreArgs=list(
            Coordinate_1_based = FALSE,
            DataProvider = "hgdownload.cse.ucsc.edu",
            Location_Prefix = .ucscBase,
            Maintainer = "Sonali Arora <sarora@fhcrc.org>",
            RDataClass = "ChainFile",
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.1",
            Recipe = NA_character_,
            Tags = c("liftOver", "chain", "UCSC", "genome", "homology")))
}

makeAnnotationHubResource("UCSCChainPreparer", makeUCSCChain)
