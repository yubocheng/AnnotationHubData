.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.getchainFiles <- function(url, fileName=NA_character_, verbose=TRUE) {
    result <- .httrRead(url, xpathString="//pre/a/text()",
                        fileName=fileName, getmd5sum=TRUE)
    if(length(result)) {
        files <-  paste0(url, "/", result$files)
        df <- .httrFileInfo(files, verbose=TRUE)
        if(identical(names(result), c("files","md5sum")))
            cbind(df, md5sum=result$md5sum, stringsAsFactors=FALSE)
    } else 
        data.frame(fileurl=NA_character_, date=NA, 
            size=NA, md5sum=NA_character_, stringsAsFactors=FALSE)
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
    
    
    scin[which(scin %in% "Pongo abelii")] <- "Pongo pygmaeus abelii"
    scin[which(scin %in% "Xenopus (Silurana) tropicalis")]="Xenopus tropicalis"
    scin[which(scin %in% "Ictidomys tridecemlineatus")]="Spermophilus tridecemlineatus"
    
    ## there are 3 special cases: WE provide query, ncbi returns scin
    #a) query ="Pongo pygmaeus abelii", scin="Pongo abelii", taxid="9601"
    #b) query ="Xenopus tropicalis", scin="Xenopus (Silurana) tropicalis", taxid="8364"
    #c) query ="Spermophilus tridecemlineatus", scin="Ictidomys tridecemlineatus", taxid="43179"
        
    ## 3. Results
    as.integer(taxid[match(organism, scin)])
}

.getUCSCResources <- 
    function(fileType, dirName, fileName, verbose=FALSE, justRunUnitTest=FALSE)
{
    ## get resource from UCSC
    .fileBase <- sprintf("%sgoldenPath", .ucscBase)
    genome_tbl <- rtracklayer::ucscGenomes(organism=TRUE)
    genomes <- genome_tbl$db
    ## remove faulty genome. 
    rm <- c("cb1", "eboVir3", "dp2", "strPur1", "ci1", "calMil1","monDom1", 
            "balAcu1" ,"musFur1")
    genomes <- setdiff(genomes, rm)
    
    urls <- sprintf("%s/%s/%s", .fileBase, genomes, dirName)
    
    if(justRunUnitTest)
       	urls <- urls[1]
       
    rsrc <- do.call(rbind, lapply(urls, .getchainFiles, 
        fileName=fileName, verbose=verbose))
    rsrc <- rsrc[complete.cases(rsrc),]
    title <- basename(rsrc$fileurl)

    ## parse the filename for each file type.
    switch(fileType, chain={
        rsrc$from <- sub("^([[:alnum:]]+)To[A-Z].*", "\\1", title)
        rsrc$to <- sub(".*To([A-Z])([[:alnum:]]+).*", "\\L\\1\\E\\2",
                       title, perl=TRUE)
    }, "2bit"={
        rsrc$from <- sub(".2bit","", title)
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

makeUCSCChain <- function(currentMetadata, justRunUnitTest=FALSE, 
                          BiocVersion=biocVersion()) {
    rsrc <- .getUCSCResources(fileType="chain", dirName="liftOver", 
        fileName="chain.gz", verbose=TRUE, justRunUnitTest)
    
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    species <- rsrc$organism   
    genome <- rsrc$from
    taxonomyId <- as.integer(rsrc$taxid)           
    title <- basename(rsrc$fileurl) 
    description <- sprintf("UCSC liftOver chain file from %s to %s",
                           rsrc$from, rsrc$to)
    rdatapaths <-gsub(.ucscBase, "",sourceUrls)
    md5sum <- rsrc$md5sum
    
    Map(AnnotationHubMetadata,
        
        SourceSize=sourceSize,
        SourceUrl=sourceUrls,
        SourceVersion=sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        SourceMd5 =md5sum, 
        
        Description=description,
        Title=title,
        Genome=genome,
        Species=species, 
        TaxonomyId=taxonomyId,
        
        RDataPath= rdatapaths,
        
        MoreArgs=list(
            BiocVersion=BiocVersion,
            # input sources 
            SourceType= "Chain",
            
            # resources
            DataProvider = "UCSC",
            Maintainer =  "Bioconductor Maintainer <maintainer@bioconductor.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .ucscBase,
            RDataDateAdded = Sys.time(),
                        
            #rdata table
            DispatchClass= "ChainFile" ,
            RDataClass = "GRanges",
            
            Recipe = NA_character_, 
            Tags = c("liftOver", "chain", "UCSC", "genome", "homology")))
}

makeAnnotationHubResource("UCSCChainPreparer", makeUCSCChain)
