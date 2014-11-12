.EpigenomeRoadMap <- "http://www.broadinstitute.org/~anshul/projects/roadmap/"

trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
trim2 <- function(x) {
    x <- gsub("\n","", x)
    gsub("^ *|(?<= ) | *$","", x, perl=TRUE)
}

.getFileInfo <- function(url, broad=TRUE) {
    names(url) <- basename(url)
    result <- lapply(url, function(f) {
        h <- basicTextGatherer()
        ok <- curlPerform(url=f,
                          nobody=TRUE, headerfunction=h$update)
        yy <- h$value()
        list(date=sub(".*Last-Modified: ([[:print:]]+) GMT.*", "\\1", yy),
             size=sub(".*Content-Length: ([[:digit:]]+).*","\\1", yy),
             src_url=gsub(.EpigenomeRoadMap,"",f))
    })
    date <- strptime(sapply(result, "[[", "date"),
                     "%a, %d %b %Y %H:%M:%S", tz="GMT")
    size <- as.integer(sapply(result, "[[", "size"))
    sourceUrl <- sapply(result, "[[", "src_url")
    
    description <- vector(length=length(url))
    if(broad) {
        ## broad peak description
        keep_dna_b1  <- grepl("hotspot.fdr0.01.broad.bed.gz", url)
        keep_dna_b2  <- grepl("hotspot.broad.bed.gz", url)  
        description[keep_dna_b1] <- "Broad DNasePeaks at FDR 1%"
        description[keep_dna_b2] <- "Broad DNasePeaks at no FDR thresholding"
        keep_chip_b1  <- grepl("broadPeak.gz", url)  
        keep_chip_b2 <- grepl("gappedPeak.gz", url) 
        description[keep_chip_b1] <- trim2("Broad ChIP peaks passing the p-value 
        0.1 threshold")
        description[keep_chip_b2] <- trim2("Broad ChIP peaks (passing p-value 
        0.1) that contain atleast one narrow peak passing a pvalue of 0.01.")
    } else {
        ## narrow peaks
        keep_dna_n1 <-  grepl("hotspot.fdr0.01.peaks.bed.gz",url)   
        keep_dna_n2 <- grepl("hotspot.all.peaks.bed.gz",url)
        keep_dna_n3 <- grepl(".macs2.narrowPeak.gz",url)
        description[keep_dna_n1] <- trim2("Narrow DNasePeaks in FDR 1% hotspots
            called using HotSpot")
        description[keep_dna_n2] <- trim2("Genome-wide tag density peak calls 
        (not restricted to hotspots, thresholded or unthresholded) called 
        using HotSpot peak caller")
        description[keep_dna_n3] <- trim2("Narrow DNasePeaks called using MACS2 
        with a p-value threshold of 0.01")
        keep_chip_n1  <- grepl("narrowPeak.gz",url)
        description[keep_chip_n1] <- trim2("Narrow ChIP peaks were called using 
        MACSv2 with a p-value threshold of 0.01. ")
    }
    
    data.frame(date=date, size=size, sourceUrl=sourceUrl, 
               description=description, stringsAsFactors =FALSE)
}

.getPeakCalls <- function(url,verbose=FALSE, broad=TRUE) {
    require(XML)
    require(RCurl)
    tryCatch ({
        if (verbose)
            message(basename(url))
        html <- htmlParse(url)
        
        if(length(class(html))!=4)
            stop("Cant connect to Broad Institute to get Epigenome files")
        
        fls <- sapply(html["//li/a/text()"], xmlValue)
        
        fls <- sapply(fls, trim, USE.NAMES=FALSE)
        fls <- fls[!grepl("Parent Directory" , fls)]
        fls <- fls[!grepl("README.txt" , fls)]
        
        ## get file information for each file. 
        .getFileInfo(url=paste0(url,fls), broad=broad) ## step takes time!!
        
    },error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
        data.frame(url=character(), version=character(), 
                   stringsAsFactors=FALSE)
    })
}

.getEpigenomeRoadMapPeaks <- function() {
    broadBase <- "peaks/stdnames30M/broadPeak/"
    narrowBase <- "peaks/stdnames30M/combrep/"
    
    .broadUrl <- sprintf(paste0("%s",broadBase), .EpigenomeRoadMap)
    broad_df <- .getPeakCalls(url =.broadUrl, broad=TRUE,verbose=TRUE)
    
    .narrowUrl <- sprintf(paste0("%s",narrowBase), .EpigenomeRoadMap)
    narrow_df <- .getPeakCalls(url =.narrowUrl, broad=FALSE)
    
    rbind(broad_df, narrow_df)
}


makeEpigenomeRoadmap <- function(currentMetadata) {
    rsrc <- .getEpigenomeRoadMapPeaks()
    
    description <- rsrc$description
    genome <- rep("hg19",nrow(rsrc))
    sourceFile <- rownames(rsrc)
    title <- rownames(rsrc)
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- sapply(rsrc$date, function(y) gsub(" ","_",y)) 
        ## should be character
    species <- rep("Homo sapiens",nrow(rsrc))           
    taxonomyId <- as.integer(rep(9606,nrow(rsrc)))
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
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
            DataProvider = "broadinstitute.org",
            Location_Prefix = .EpigenomeRoadMap,
            Maintainer = "Sonali Arora <sarora@fhcrc.org>",
            RDataClass = "EpigenomeRoadmapFile",
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.1",
            Recipe = NA_character_,
            Tags = c("DNAseq","ChIPseq","genome" )))
}

makeAnnotationHubResource("EpigenomeRoadMapPreparer", makeEpigenomeRoadmap)

