.EpigenomeRoadMap <- "http://www.broadinstitute.org/~anshul/projects/roadmap/"

.trim <- function(x)
    gsub("[[:space:]]{2,}"," ", x)

.getFileInfo <- function(urls) {
    names(urls) <- basename(urls)
    result <- lapply(urls, function(url) {
        message(".getFileInfo ", sQuote(basename(url)))
        ok <- GET(url, config=config(nobody=TRUE))
        stop_for_status(ok)
        headers(ok)[c("last-modified", "content-length")]
    })
    date <- parse_http_date(vapply(result, "[[", character(1), "last-modified"))
    size <- as.integer(vapply(result, "[[", character(1), "content-length"))
    sourceUrl <- sub(.EpigenomeRoadMap, "", urls)

    map <- c(
        ## broad peaks
        hotspot.fdr0.01.broad.bed.gz="Broad DNasePeaks at FDR 1%",
        hotspot.broad.bed.gz="Broad DNasePeaks at no FDR thresholding",
        broadPeak.gz="Broad ChIP peaks passing the p-value 0.1 threshold",
        gappedPeak.gz=.trim("Broad ChIP peaks (passing p-value 0.1) that
            contain at least one narrow peak passing a pvalue of 0.01."),
        ## narrow peaks
        hotspot.fdr0.01.peaks.bed.gz=.trim("Narrow DNasePeaks in FDR 1%
            hotspots called using HotSpot"),
        hotspot.all.peaks.bed.gz=.trim("Genome-wide tag density peak calls,
            (not restricted to hotspots, thresholded or unthresholded) called
            using HotSpot peak caller"),
        .macs2.narrowPeak.gz=.trim("Narrow DNasePeaks called using MACS2
            with a p-value threshold of 0.01"),
        narrowPeak.gz=.trim("Narrow ChIP peaks were called using MACSv2
            with a p-value threshold of 0.01."))
    description <- character(length(urls))
    for (i in seq_along(map))
        description[grep(names(map)[i], urls)] <- map[[i]]
    
    data.frame(date=date, size=size, sourceUrl=urls, 
               description=description, stringsAsFactors =FALSE)
}

.getPeakCalls <- function(url, tag) {
    ans <- tryCatch ({
        message(basename(url))
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- sapply(html["//li/a/@href"], as.character)
        fls <- fls[!grepl(".*(/|README.txt).*" , fls)]
        urls <- paste0(url, fls)
        ## get file information for each file. 
        .getFileInfo(urls) ## step takes time!!
        
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err), immediate.=TRUE)
        data.frame(url=character(), version=character(),
                   stringsAsFactors=FALSE)
    })
    cbind(ans, tag=tag)
}

.getEpigenomeRoadMapPeaks <- function() {
    paths <- c(broadPeaks="peaks/stdnames30M/broadPeak/",
               narrowPeaks="peaks/stdnames30M/combrep/")
    urls <- setNames(paste0(.EpigenomeRoadMap, paths), names(paths))

    do.call(rbind, Map(.getPeakCalls, urls, names(urls)))
}

makeEpigenomeRoadmap <- function(currentMetadata, justRunUnitTest=FALSE) {
    rsrc <- .getEpigenomeRoadMapPeaks()
    
    description <- rsrc$description
    sourceFile <- rownames(rsrc)
    title <- rownames(rsrc)
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- gsub(" ","_",rsrc$date) 
        ## should be character
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- lapply(rsrc$tag, function(tag) {
        c("EpigenomeRoadmap", as.character(tag), "DNAseq","ChIPseq","genome")
    })
    
    Map(AnnotationHubMetadata,
        Description=description, 
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion, 
        Title=title, Tags=Tags,
        MoreArgs=list(
            Genome="hg19",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Coordinate_1_based = FALSE,
            DataProvider = "broadinstitute.org",
            Location_Prefix = .EpigenomeRoadMap,
            Maintainer = "Sonali Arora <sarora@fhcrc.org>",
            RDataClass = "EpigenomeRoadmapFile",
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.1",
            Recipe = NA_character_))
}

makeAnnotationHubResource("EpigenomeRoadMapPreparer", makeEpigenomeRoadmap)

