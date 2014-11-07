.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.fileSize <- function(s) {
    s1 <- character(0)
    if(s=="")
        return(NA_character_)
    
    let <- gsub("[0-9]", "",s)
    no <- gsub("[:A-Z:]", "",s)
    s1 <- ifelse(let=="M", as.numeric(no)*1000000, as.numeric(no)*1000)
    s1
}  

.get1Resource <- function(url, fileName, verbose=FALSE) {
    require(XML)
    tryCatch({
        if (verbose)
            message(basename(dirname(url)))
        html <- htmlParse(url)
        fls <- sapply(html["//pre[2]/a/text()"], xmlValue)
        
        ## extract the file name
        url <- sprintf("%s/%s", url, fls)
        keep <- grepl(paste0(fileName,"$"), fls)
        
        ## extract the date time stamp of file.  
        stamps <- sapply(html["//pre[2]/text()"], xmlValue)
        regex <- " *([[:alnum:]-]+) *([:[:alnum:]]+)*.*"
        version <- sub(regex, "\\1_\\2", stamps)
        
        ## extract the file size
        regex2 <- " *([[:alnum:]-]+) *([:[:alnum:]]+)"
        size <- gsub(" ","",gsub("\\n","",sub(regex2, "", stamps)), fixed=TRUE)
        size  <- gsub("-","", size)
        newSize <- sapply(size, .fileSize)
        
        data.frame(url, version, size, newSize, 
            stringsAsFactors=FALSE)[keep,,drop=FALSE]
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
    
    tax_ind <- match(c("9601","8364","43179"), taxid)
    scin_ind <- match(c("Pongo abelii","Xenopus (Silurana) tropicalis",
        "Ictidomys tridecemlineatus"), scin)
    if(identical(tax_ind, scin_ind))
        scin[scin_ind] <- c("Pongo pygmaeus abelii","Xenopus tropicalis",
            "Spermophilus tridecemlineatus")
    
    ## 3. Results
    as.integer(taxid)[match(organism, scin)]
}

.getUCSCResources <- function(fileType, dirName, fileName, verbose=FALSE) {
    ## get resource from UCSC
    .fileBase <- sprintf("%sgoldenPath", .ucscBase)
    genome_tbl <- rtracklayer::ucscGenomes(organism=TRUE)
    genomes <- genome_tbl$db
    urls <- sprintf(paste0("%s/%s/", dirName), .fileBase, genomes)
    rsrc <- do.call(rbind, lapply(urls, .get1Resource,  
        fileName= fileName, verbose=verbose))
    rownames(rsrc) <- basename(rsrc$url)
    
    ## parse the filename for each file type. 
    if(fileType=="chain") {
        rsrc$from <- sub("^([a-z][[:alnum:]]+)To.*.over.chain.gz", "\\1",
            rownames(rsrc))
        to <- sub(".*To([A-Z][[:alnum:]]+)\\.over.chain.gz", "\\1", 
            rownames(rsrc))
        rsrc$to <- sub("^([A-Z])", "\\L\\1", to, perl=TRUE)
    }
    
    if(fileType=="2bit")
        rsrc$from <- sub(".2bit","",rownames(rsrc))
    
    ## add the organism 
    idx <- match(rsrc$from, genome_tbl$db)
    rsrc$organism <- as.character(genome_tbl[idx, "organism"])
    rsrc$organism[which(is.na(rsrc$organism))] <- NA_character_
    
    ## add the taxonmy Id. 
    rsrc$taxid <- .organismToTaxid(rsrc$organism)
    rsrc$taxid[which(is.na(rsrc$taxid))]<- NA_character_
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
    sourceVersion <- rsrc$version
    species <- rsrc$organism            
    taxonomyId <- as.integer(rsrc$taxid)           
    title <- rownames(rsrc)
    SourceLastModifiedDate <- as.POSIXct(strptime(rsrc$version,
        "%d-%b-%Y_%H:%M"))
    SourceSize <- as.numeric(rsrc$newSize)
        
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