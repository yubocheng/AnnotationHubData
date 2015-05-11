.EpigenomeRoadMap <- "http://egg2.wustl.edu/roadmap/data/byFileType/"

.readEpiFilesFromWebUrl <- function(url, fileType, justRunUnitTest) {
    tryCatch ({
       table <- XML::readHTMLTable(url)[[1]]
       fnames <- grep(fileType, as.character(table$Name), value=TRUE) 
       fileurl <- paste0(url, fnames)
       
       if(justRunUnitTest)
           fileurl <- fileurl[1:2]
       fileurl
       
    }, error=function(err) {
        warning(url, ": ", conditionMessage(err), immediate.=TRUE)
        data.frame(url=character(), stringsAsFactors=FALSE)
    }) 
    
}

.EpiFilesMetadata <- function() {
    metafname <-"metadata/EID_metadata.tab"
    read.delim(paste0(.EpigenomeRoadMap, metafname))
}

.EpiFilesTags <- function(url) {
    ## get columns F,O,D from metadata
    metadata <- .EpiFilesMetadata()
    filename <- basename(url)
    celltype <- gsub("-.*","", filename)
    idx <- match(celltype, metadata[,"EID"])
    metadata <- metadata[,c("EID", "GROUP","MNEMONIC", "STD_NAME")]
    fod <- vapply(idx, function(x) 
        paste0(as.character(unlist(metadata[x,])), collapse=", ")
        , character(1))
    fod <- gsub("[\x01-\x1f\x7f-\xff]", "", fod) # remove \xa0
    ## parse the url to get which kind of  files
    tempurl <- gsub(.EpigenomeRoadMap, "", url)
    out <- strsplit(tempurl, "/")
    mat <- do.call(rbind, out)
    matTags <- apply(mat, 1, function(x) paste0(x[1:3], collapse=", "))
    
    paste("EpigenomeRoadMap", matTags, fod, sep=", ")
}


.MiscEpiFiles <- function(fileurls) {
    ## get the file size and  the file info 
    df <- .httrFileInfo(fileurls, verbose=TRUE) 
    
    ## these files are downloaded from the web 
    rdatapath <- sub(.EpigenomeRoadMap, "", fileurls)
  
    ## add tags
    tags <- .EpiFilesTags(fileurls)
    
    ## description 
    description <- .EpiFilesDescription(fileurls)
    
    cbind(df, rdatapath, tags, description, stringsAsFactors=FALSE)
}

.EpiFilesDescription <- function(fileurls) {  
    ## description 
    ## refer : http://egg2.wustl.edu/roadmap/web_portal/processed_data.html    
    map <- c(
        ## broad peaks- DNase-seq
        hotspot.fdr0.01.broad.bed.gz=.expandLine("Broad domains on enrichment for 
             DNase-seq for consolidated epigenomes from EpigenomeRoadMap 
             Project"),
        hotspot.broad.bed.gz=.expandLine("Broad domains on enrichment for 
            DNase-seq for consolidated epigenomes from EpigenomeRoadMap 
            Project"),
        
        broadPeak.gz=.expandLine("Broad ChIP-seq peaks for consolidated 
            epigenomes from EpigenomeRoadMap Project"),
        gappedPeak.gz=.expandLine("Gapped ChIP-seq peaks for consolidated 
            epigenomes from EpigenomeRoadMap Project"),
        narrowPeak.gz=.expandLine("Narrow ChIP-seq peaks for consolidated 
            epigenomes from EpigenomeRoadMap Project"),
    
        ## narrow peaks
        hotspot.fdr0.01.peaks.bed.gz=.expandLine("Narrow DNasePeaks for 
            consolidated epigenomes from EpigenomeRoadMap Project"),
        hotspot.all.peaks.bed.gz=.expandLine("Narrow DNasePeaks for 
            consolidated epigenomes from EpigenomeRoadMap Project"),
        .macs2.narrowPeak.gz=.expandLine("Narrow DNasePeaks for 
            consolidated epigenomes from EpigenomeRoadMap Project"), 
        
        ## Big Wig Files
         fc.signal.bigwig=.expandLine("Bigwig File containing  fold enrichment 
            signal tracks from EpigenomeRoadMap Project"),
        pval.signal.bigwig=.expandLine("Bigwig File containing -log10(p-value) 
            signal tracks from EpigenomeRoadMap Project"))        
        
    description <- character(length(fileurls))
    for (i in seq_along(map))
        description[grep(names(map)[i], fileurls)] <- map[[i]]
    
    description
}

## PEAK CONSOLIDATED FILES
## these files are to be re-inserted 
.peakConsolidatedEpiFiles <- function(justRunUnitTest) {
    paths <- c(broadPeaks="peaks/consolidated/broadPeak/",
               narrowPeaks="peaks/consolidated/narrowPeak/", 
               gappedPeaks="peaks/consolidated/gappedPeak/" )
    fileurls <- lapply(paste0(.EpigenomeRoadMap, paths), function(x) {
        .readEpiFilesFromWebUrl(x, "gz", justRunUnitTest)
    })
    .MiscEpiFiles(unlist(fileurls))
}

.signalEpiFiles <- function(justRunUnitTest) {   
    
    ## consolidated BigWifg Files
    paths <- 
        c(consolidated_foldChange="signal/consolidated/macs2signal/foldChange/", 
        consolidated_pval="signal/consolidated/macs2signal/pval/",
        un_consolidated_foldChange="signal/unconsolidated/foldChange/", 
        un_consolidated_pval="signal/unconsolidated/pval/")
    dirurl <- paste0(.EpigenomeRoadMap, paths)
    
    ## consolidated Imputed Files
    conImpUrl <- paste0(.EpigenomeRoadMap, "signal/consolidatedImputed/")
    table <- XML::readHTMLTable(conImpUrl)[[1]]
    baseDirs<- levels(table$Name)
    idx <- match(c("Parent Directory", "RNAseq/", "DNase/", "DNAMethylSBS/"),
       baseDirs)
    baseDirs <- baseDirs[-idx]      
    baseDirs <- paste0(conImpUrl, baseDirs)
    dirurl <- c(dirurl, baseDirs)
    
    fileurls <- lapply(dirurl, function(x) {
        .readEpiFilesFromWebUrl(x, "bigwig", justRunUnitTest)
    })
    fileurls <- unlist(fileurls)
    df <- .MiscEpiFiles(fileurls)
    # # add dispatch class, sourcetype and Rdataclass
    dispatchClass <- rep("BigWigFile" , length(fileurls))
    rdataclass <- rep("GRanges" , length(fileurls))
    sourcetype <- rep("BigWig" , length(fileurls))
    
    cbind(df, dispatchClass, sourcetype, rdataclass, stringsAsFactors=FALSE)
}

.EpiMetadataFile <- function() {
    metafname <-"metadata/EID_metadata.tab"
    fileurl <- paste0(.EpigenomeRoadMap, metafname)
    description <- "Metadata for EpigenomeRoadMap Project"
    df <-.httrFileInfo(fileurl)
    tag <- paste("EpigenomeRoadMap", "Metadata", sep=", ")
    rdataclass <- "data.frame"
    sourcetype <- "tab"
    dispatchClass <- "data.frame"
    rdatapath <- sub(.EpigenomeRoadMap, "", fileurl)
    cbind(df, description, tag,  dispatchClass, sourcetype, rdataclass, 
        rdatapath, stringsAsFactors=FALSE)
}

makeEpigenomeRoadmap <- function(currentMetadata, justRunUnitTest=FALSE) {
    rsrc <- .peakConsolidatedEpiFiles(justRunUnitTest)
    #rsrc <- .signalEpiFiles(justRunUnitTest)
    #rsrc <-  .EpiMetadataFile()
    
    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ","_",rsrc$date) 
        ## should be character
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- strsplit(rsrc$tag, ", ") 
    rdatapath <- rsrc$rdatapath
    SourceType = rsrc$sourcetype
    DispatchClass = rsrc$dispatchClass
    RDataClass = rsrc$rdataclass    

    Map(AnnotationHubMetadata,
        Description=description, 
        SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=rdatapath,
        SourceVersion=sourceVersion, 
        Title=title, Tags=Tags,
        SourceType =SourceType,
        DispatchClass = DispatchClass,
        RDataClass = RDataClass,
        MoreArgs=list(
            Genome="hg19",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Coordinate_1_based = FALSE,
            DataProvider = "BroadInstitute",
            Location_Prefix = .EpigenomeRoadMap,
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",
            RDataDateAdded = Sys.time(),
            Recipe = NA_character_))
}

makeAnnotationHubResource("EpigenomeRoadMapPreparer", makeEpigenomeRoadmap)

