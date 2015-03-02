.EpigenomeRoadMap <- "http://www.broadinstitute.org/~anshul/projects/roadmap/"

.getEpiFileInfo <- function(urls) {
    df <- .httrFileInfo(urls, verbose=TRUE) 
    rdatapath <- sub(.EpigenomeRoadMap, "", urls)

    map <- c(
        ## broad peaks
        hotspot.fdr0.01.broad.bed.gz="Broad DNasePeaks at FDR 1%",
        hotspot.broad.bed.gz="Broad DNasePeaks at no FDR thresholding",
        broadPeak.gz="Broad ChIP peaks passing the p-value 0.1 threshold",
        gappedPeak.gz=.expandLine("Broad ChIP peaks (passing p-value 0.1) that
            contain at least one narrow peak passing a pvalue of 0.01."),
        ## narrow peaks
        hotspot.fdr0.01.peaks.bed.gz=.expandLine("Narrow DNasePeaks in FDR 1%
            hotspots called using HotSpot"),
        hotspot.all.peaks.bed.gz=.expandLine("Genome-wide tag density peak calls,
            (not restricted to hotspots, thresholded or unthresholded) called
            using HotSpot peak caller"),
        .macs2.narrowPeak.gz=.expandLine("Narrow DNasePeaks called using MACS2
            with a p-value threshold of 0.01"),
        narrowPeak.gz=.expandLine("Narrow ChIP peaks were called using MACSv2
            with a p-value threshold of 0.01."))
    description <- character(length(urls))
    for (i in seq_along(map))
        description[grep(names(map)[i], urls)] <- map[[i]]
    
    cbind(df, rdatapath=rdatapath, description=description, 
        stringsAsFactors =FALSE)
}

.getPeakCalls <- function(url, tag, justRunUnitTest) {
    ans <- tryCatch ({
        message(basename(url))
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- sapply(html["//li/a/@href"], as.character)
        fls <- fls[!grepl(".*(/|README.txt).*" , fls)]
        urls <- paste0(url, fls)
        
        if(justRunUnitTest)
            urls <- urls[1:5]
        
        ## get file information for each file. 
        .getEpiFileInfo(urls) ## step takes time!!
        
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err), immediate.=TRUE)
        data.frame(url=character(), version=character(),
                   stringsAsFactors=FALSE)
    })
    cbind(ans, tag=tag)
}

.getEpigenomeRoadMapPeaks <- function(justRunUnitTest) {
    paths <- c(broadPeaks="peaks/consolidated/broadPeak/",
               narrowPeaks="peaks/consolidated/narrowPeak/")
    urls <- setNames(paste0(.EpigenomeRoadMap, paths), names(paths))

    do.call(rbind, Map(.getPeakCalls, urls, names(urls), justRunUnitTest))
}

makeEpigenomeRoadmap <- function(currentMetadata, justRunUnitTest=FALSE) {
    rsrc <- .getEpigenomeRoadMapPeaks(justRunUnitTest)
    
    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ","_",rsrc$date) 
        ## should be character
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- lapply(rsrc$tag, function(tag) {
        c("EpigenomeRoadmap", as.character(tag), "DNAseq","ChIPseq","genome")
    })
    rdatapath <- rsrc$rdatapath
    
    Map(AnnotationHubMetadata,
        Description=description, 
        SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=rdatapath,
        SourceVersion=sourceVersion, 
        Title=title, Tags=Tags,
        MoreArgs=list(
            Genome="hg19",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Coordinate_1_based = FALSE,
            SourceType ="BED",
            DataProvider = "BroadInstitute",
            Location_Prefix = .EpigenomeRoadMap,
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",
            RDataClass = "GRanges",
            DispatchClass = "EpigenomeRoadmapFile",
            RDataDateAdded = Sys.time(),
            Recipe = NA_character_))
}

makeAnnotationHubResource("EpigenomeRoadMapPreparer", makeEpigenomeRoadmap)

