.EpigenomeRoadMap <- "http://egg2.wustl.edu/roadmap/data/byFileType/"
.EpigenomeRoadMapMain <- "http://egg2.wustl.edu/roadmap/data/"

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
    metafname <-"byFileType/metadata/EID_metadata.tab"
    read.delim(paste0(.EpigenomeRoadMapMain, metafname))
}

.EpiFilesTags <- function(url, pattern="-") {
    ## get columns F,O,D from metadata
    metadata <- .EpiFilesMetadata()
    filename <- basename(url)
     celltype <- gsub(paste0(pattern,".*"),"", filename)
    idx <- match(celltype, metadata[,"EID"])
    
    ## parse the url to get which kind of  files
    tempurl <- gsub(.EpigenomeRoadMapMain, "", url)
    tempurl <- sub("byDataType/","", tempurl)  
    tempurl <- sub("byFileType/","", tempurl)
    out <- strsplit(tempurl, "/")
    mat <- do.call(rbind, out)
    matTags <- apply(mat, 1, function(x) paste0(x[1:3], collapse=", "))
    
    tags <- paste("EpigenomeRoadMap", matTags, sep=", ")
    
    if(any(!is.na(unique(idx)))) {
        metadata <- metadata[,c("EID", "GROUP","MNEMONIC", "STD_NAME")]
        fod <- vapply(idx, function(x) 
            paste0(as.character(unlist(metadata[x,])), collapse=", ")
            , character(1))
        fod <- gsub("[\x01-\x1f\x7f-\xff]", "", fod) # remove \xa0
        tags <- paste(tags, fod, sep=", ")
    }
    tags
}

.MiscEpiFiles <- function(fileurls, pattern="-") {
    ## get the file size and  the file info 
    df <- .httrFileInfo(fileurls, verbose=TRUE) 

    ## these files are downloaded from the web 
    rdatapath <- sub(.EpigenomeRoadMapMain, "", fileurls)
  
    ## add tags
    tags <- .EpiFilesTags(fileurls, pattern)
    
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
            signal tracks from EpigenomeRoadMap Project"),
   
        ## chmm Models
        coreMarks_mnemonics.bed.gz=.expandLine("15 state chromatin 
        segmentations from EpigenomeRoadMap Project"),


        ## dna methylation files
        mCRF_FractionalMethylation.bigwig=.expandLine("MeDIP/MRE(mCRF) 
        fractional methylation calls from EpigenomeRoadMap Project"),
        RRBS_FractionalMethylation.bigwig=.expandLine("RRBS
        fractional methylation calls from EpigenomeRoadMap Project "),
        WGBS_FractionalMethylation.bigwig=.expandLine("Whole genome bisulphite
        fractional methylation calls from EpigenomeRoadMap Project") )        
        
    description <- character(length(fileurls))
    for (i in seq_along(map))
        description[grep(names(map)[i], fileurls)] <- map[[i]]
    
    description
}

.peakEpiFiles <- function(justRunUnitTest) {
   paths <- c(broadPeaks="byFileType/peaks/consolidated/broadPeak/",
               narrowPeaks="byFileType/peaks/consolidated/narrowPeak/", 
               gappedPeaks="byFileType/peaks/consolidated/gappedPeak/" ,
               unbroadPeaks="byFileType/peaks/unconsolidated/broadPeak/",
               unnarrowPeaks="byFileType/peaks/unconsolidated/narrowPeak/",
               ungappedPeaks="byFileType/peaks/unconsolidated/gappedPeak/" )
    fileurls <- sapply(paste0(.EpigenomeRoadMapMain, paths), function(x) {
        .readEpiFilesFromWebUrl(x, "gz", justRunUnitTest)
    })
    if(justRunUnitTest)
         fileurls <- fileurls[1:2]
    df <- .MiscEpiFiles(fileurls)
    dispatchClass <- rep("EpigenomeRoadmapFile" , length(fileurls))
    sourcetype <- rep("BED" , length(fileurls))
    rdataclass <- rep("GRanges" , length(fileurls))
    
    cbind(df, dispatchClass, sourcetype, rdataclass, stringsAsFactors=FALSE)
}

.signalEpiFiles <- function(justRunUnitTest) {   
    ## consolidated & unconsolidated BigWig Files
    paths <- 
        c(consolidated_foldChange="byFileType/signal/consolidated/macs2signal/foldChange/", 
        consolidated_pval="byFileType/signal/consolidated/macs2signal/pval/",
        un_consolidated_foldChange="byFileType/signal/unconsolidated/foldChange/", 
        un_consolidated_pval="byFileType/signal/unconsolidated/pval/")
    dirurl <- paste0(.EpigenomeRoadMapMain, paths)
    
    ## consolidated Imputed Files
    conImpUrl <- paste0(.EpigenomeRoadMapMain, "byFileType/signal/consolidatedImputed/")
    table <- XML::readHTMLTable(conImpUrl)[[1]]
    baseDirs<- levels(table$Name)
    idx <- match(c("Parent Directory", "RNAseq/", "DNase/", "DNAMethylSBS/"),
       baseDirs)
    baseDirs <- baseDirs[-idx]      
    baseDirs <- paste0(conImpUrl, baseDirs)
    dirurl <- c(dirurl, baseDirs)
    
    fileurls <- sapply(dirurl, function(x) {
        .readEpiFilesFromWebUrl(x, "bigwig", justRunUnitTest)
    })
    
    if(justRunUnitTest)
        fileurls <- fileurls[1:2]    

    df <- .MiscEpiFiles(fileurls)
    # # add dispatch class, sourcetype and Rdataclass
    dispatchClass <- rep("BigWigFile" , length(fileurls))
    rdataclass <- rep("BigWigFile" , length(fileurls))
    sourcetype <- rep("BigWig" , length(fileurls))
    
    cbind(df, dispatchClass, sourcetype, rdataclass, stringsAsFactors=FALSE)
}

.EpiMetadataFile <- function() {
    metafname <-"byFileType/metadata/EID_metadata.tab"
    fileurl <- paste0(.EpigenomeRoadMapMain, metafname)
    description <- "Metadata for EpigenomeRoadMap Project"
    df <-.httrFileInfo(fileurl)
    tags <- paste("EpigenomeRoadMap", "Metadata", sep=", ")
    rdataclass <- "data.frame"
    sourcetype <- "tab"
    dispatchClass <- "data.frame"
    rdatapath <- sub(.EpigenomeRoadMapMain, "", fileurl)
    cbind(df, rdatapath, tags, description, dispatchClass, sourcetype, 
        rdataclass, stringsAsFactors=FALSE)
}

.chmmModels <- function(justRunUnitTest) {
    dirurl <- paste0(.EpigenomeRoadMapMain, 
        "byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/")
    fileurls <- .readEpiFilesFromWebUrl(dirurl, "mnemonics.bed.gz", 
                                        justRunUnitTest=FALSE)
    if (justRunUnitTest)
         fileurls <- fileurls[1:2]
    df <- .MiscEpiFiles(fileurls, pattern="_")
    
    dispatchClass <- rep("EpichmmModels" , length(fileurls))
    rdataclass <- rep("GRanges" , length(fileurls))
    sourcetype <- rep("BED" , length(fileurls))
    cbind(df, dispatchClass, sourcetype, rdataclass, stringsAsFactors=FALSE)
}

.expressionTextFiles <- function(justRunUnitTest=FALSE) {
   dirurl <- paste0(.EpigenomeRoadMapMain,"byDataType/rna/expression/")

   ## collect file name from page
   fileurls <- .readEpiFilesFromWebUrl(dirurl,"gz", FALSE)
    
   ## get the file size and  the file info
   df <- .httrFileInfo(fileurls, verbose=TRUE)

   ## these files are downloaded from the web
   rdatapath <- sub(.EpigenomeRoadMapMain, "", fileurls)

    ## add description.
    map <- c(
       "57epigenomes.RPKM.pc" = "RPKM expression matrix for protein coding genes",
       "57epigenomes.N.pc"= "RNA-seq read counts matrix for protein coding genes",
       "57epigenomes.exon.RPKM.pc"= "RPKM expression matrix for protein coding exons",
       "57epigenomes.RPKM.intronic.pc"="RNA-seq read count matrix for intronic protein-coding RNA elements",
       "57epigenomes.exon.N.pc"="RNA-seq read counts matrix for protein coding exons",
       "57epigenomes.N.intronic.pc"="RNA-seq read count matrix for intronic protein-coding RNA elements",
       "57epigenomes.RPKM.nc"="RPKM expression matrix for non-coding RNAs",
       "57epigenomes.N.nc"="RNA-seq read counts matrix for non-coding RNAs",
       "57epigenomes.N.rb"="RNA-seq read counts matrix for ribosomal genes",
       "57epigenomes.RPKM.rb"="RPKM expression matrix for ribosomal RNAs",
       "57epigenomes.exn.RPKM.rb"="RPKM expression matrix for ribosomal gene exons",
       "57epigenomes.exn.N.rb"="RNA-seq read counts matrix for ribosomal gene exons",
       "57epigenomes.exon.N.nc.gz"="RNA-seq read counts matrix for ribosomal gene exons", 
       "57epigenomes.exon.RPKM.nc.gz"="RPKM expression matrix for ribosomal gene exons")
    description <- character(length(fileurls))
    for (i in seq_along(map))
       description[grep(names(map)[i], basename(fileurls))] <- map[[i]]

    ## add tags
    tags <- rep("EpigenomeRoadMap, expression, quantification, hg19",
        length(fileurls))

    dispatchClass <- rep("EpiExpressionText" , length(fileurls))
    rdataclass <- rep("data.frame", length(fileurls))
    sourcetype <- rep("Zip", length(fileurls))
    cbind(df, rdatapath, description, tags, dispatchClass,
        sourcetype, rdataclass, stringsAsFactors=FALSE)

}

.expressionAnnotationGtf <- function(justRunUnitTest=FALSE) {
    dirurl <- paste0(.EpigenomeRoadMapMain,"byDataType/rna/annotations/")
    
    ## collect file name from page
    fileurls <- .readEpiFilesFromWebUrl(dirurl,"gtf.gz", FALSE)
    
    ## get the file size and  the file info
    df <- .httrFileInfo(fileurls, verbose=TRUE)

    ## these files are downloaded from the web
    rdatapath <- sub(.EpigenomeRoadMapMain, "", fileurls)
    
    ## add description. 
    description <- rep(.expandLine("GencodeV10 gene/transcript coordinates
    and annotations corresponding to hg19 version of the human genome"),
        length(fileurls))
    ## add tags
    tags <- rep("EpigenomeRoadMap, annotations, genecodeV10, hg19", 
        length(fileurls))
    dispatchClass <- rep("GTFFile" , length(fileurls))
    rdataclass <- rep("GRanges", length(fileurls))
    sourcetype <- rep("GTF", length(fileurls))
    cbind(df, rdatapath, description, tags, dispatchClass, 
        sourcetype, rdataclass, stringsAsFactors=FALSE)
}


.dnaMethylation <- function(justRunUnitTest=FALSE) {
    dirurl <- paste0(.EpigenomeRoadMapMain,"byDataType/dnamethylation/")
    dirurl2 <- paste0(dirurl, c("RRBS/FractionalMethylation_bigwig/", 
                    "WGBS/FractionalMethylation_bigwig/",
                    "mCRF/FractionalMethylation_bigwig/"))
    fileurls <- unlist(sapply(dirurl2, function(x) 
       .readEpiFilesFromWebUrl(x,"bigwig", FALSE), USE.NAMES=FALSE))
    ## add decription, tags, rdatapath, date, size
    df <- .MiscEpiFiles(fileurls, pattern="_")
    ## add dispatch class, sourcetype and Rdataclass
    dispatchClass <- rep("BigWigFile" , length(fileurls))
    rdataclass <- rep("BigWigFile" , length(fileurls))
    sourcetype <- rep("BigWig" , length(fileurls))
    cbind(df, dispatchClass, sourcetype, rdataclass, stringsAsFactors=FALSE)
}


makeEpigenomeRoadmap <- function(currentMetadata, justRunUnitTest=FALSE) {
    #peak <- .peakEpiFiles(justRunUnitTest)
    #signal <- .signalEpiFiles(justRunUnitTest)
    #metadata <-  .EpiMetadataFile()
    #seg <- .chmmModels(justRunUnitTest)
    #expr_gtf <- .expressionAnnotationGtf(justRunUnitTest)
    #expr_text <- .expressionTextFiles(justRunUnitTest)
    #dnamethyl <- .dnaMethylation(justRunUnitTest)
    #rsrc <- rbind(peak[, !names(peak)%in% "date"], 
    #              signal[, !names(signal)%in% "date"], 
    #              metadata[, !names(metadata)%in% "date"], 
    #              seg[, !names(seg)%in% "date"], 
    #              expr_gtf[, !names(expr_gtf)%in% "date"],  
    #              expr_text[, !names(expr_text)%in% "date"],
    #              dnamethyl[, !names(dnamethyl)%in% "date"]  )
    #date <- c(peak$date, signal$date, metadata$date, seg$date,
    #         expr_gtf$date, expr_text$date, dnamethyl$date)    
        
    rsrc <- .dnaMethylation(justRunUnitTest)
    date <- rsrc$date

    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- as.character(date) #gsub(" ","_",date) 
        ## should be character
    SourceLastModifiedDate <- date  ## should be "POSIXct" "POSIXt"
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
            Location_Prefix = .EpigenomeRoadMapMain,
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",
            RDataDateAdded = Sys.time(),
            Recipe = NA_character_))
}

makeAnnotationHubResource("EpigenomeRoadMapPreparer", makeEpigenomeRoadmap)

