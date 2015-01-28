# This is a new recipe for EncodeImportPreparer-class.R 
.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.readHtmlPage <- function(url, isSubDir=FALSE) {
    tryCatch({
        #html <- htmlParse(url)
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- sapply(html["//pre/a/text()"], xmlValue)
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt")
        fls <- fls[-which(fls %in% remove )]
        
        if(isSubDir)
            fls <- fls[grepl(basename(url), fls)]
        
        fls
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
            url=character()
    })
}

.httrFileInfo <- function(files, verbose=TRUE) {
    tryCatch({
        result <- lapply(files, function(f){
            if(verbose)
                message(basename(f))
            
            h = GET(f, config=config(nobody=TRUE, filetime=TRUE))
            stop_for_status(h)
            headers(h)[c("last-modified", "content-length")] 
        })
        size <- as.numeric(sapply(result, "[[", "content-length"))
        date <- strptime(sapply(result, "[[", "last-modified"),
                         "%a, %d %b %Y %H:%M:%S", tz="GMT")
        data.frame(fileurl=files, date, size, stringsAsFactors=FALSE)
    }, error=function(err) {
        warning(basename(files), ": ", conditionMessage(err))
            url=character()
    })    
}


.subDir <- function(url, verbose=TRUE) {
    contents <- .readHtmlPage(url, isSubDir=TRUE)
    supported.formats <- c("narrowPeak", "broadPeak", "bedRnaElements", 
                           "gtf")
    type <- sapply(strsplit(contents,".",fixed = TRUE),"[[",2)
    idx <- type %in% supported.formats
    contents <- contents[idx]
    type <- type[idx]        
    if(length(contents)!=0) {
        files <-  sprintf("%s%s", url, contents)
        if(length(files)>5){
            files<- files[1:5]
            type<- type[1:5]
        }
            
        df <- .httrFileInfo(files ,verbose)
        cbind(df, type=type, stringsAsFactors=FALSE)
    } else 
        data.frame(fileurl=character(), date=character(), size=numeric(),
                   type= character(), stringsAsFactors=FALSE)
}

.encodeFiles <- function(){
    encode_url <- paste0(.ucscBase, "goldenpath/hg19/encodeDCC/")
    subdirs <- .readHtmlPage(encode_url, isSubDir=FALSE)
    urls <- setNames(paste0(encode_url, subdirs), subdirs)
    
    df <- do.call(rbind, Map(.subDir, urls, verbose=TRUE))
        
    title <- basename(df$fileurl)
    sourceUrl <- sub(.ucscBase , "", df$fileurl)
        
    cbind(df, title, sourceUrl, stringsAsFactors = FALSE)
}

makeEncodeImporter <- function(currentMetadata) {
    rsrc <- .encodeFiles()
    
    # previously the description field just contained the name of the file
    
    description <- rsrc$title
    genome <- rep("hg19", nrow(rsrc))
    sourceFile <- rsrc$title
    title <- rsrc$title
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- sapply(rsrc$date, function(y) gsub(" ","_",y)) 
    ## should be character
    species <- rep("Homo sapiens", nrow(rsrc))
    taxonomyId <- rep(9606L, nrow(rsrc))
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- mapply(function(x, y) {
        c("EnodeDCC", x, y, "hg19")
    }, rsrc$title, rsrc$type)
    
    Map(AnnotationHubMetadata,
        Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title, Tags=Tags,
        MoreArgs=list(
            Coordinate_1_based = FALSE,
            DataProvider = "hgdownload.cse.ucsc.edu",
            Location_Prefix = .ucscBase,
            Maintainer = "Sonali Arora <sarora@fhcrc.org>",
            RDataClass = "EncodeDCC", 
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.2",
            Recipe = "AnnotationHubData:::extendedBedToGRanges"))
}


extendedBedToGRanges <- function(ahm) {
    faIn <- normalizePath(inputFiles(ahm))
    faOut <- normalizePath(outputFile(ahm))
    
    Tags <- metadata(ahm)$Tags
    
    if(grepl("broadPeak", Tags))
        colClasses <- list(seqnames="character",
                           start="integer",
                           end="integer",
                           name="character",
                           score="integer",
                           strand="character",
                           signalValue="numeric",
                           pValue="numeric",
                           qValue="numeric")
    
    if(grepl("narrowPeak", Tags))
        colClasses <- list(seqnames="character",
                           start="integer",
                           end="integer",
                           name="character",
                           score="integer",
                           strand="character",
                           signalValue="numeric",
                           pValue="numeric",
                           qValue="numeric",
                           peak="integer")
       
    if(grepl("bedRnaElements", Tags))
        colClasses <- list(seqnames="character",
                           start="integer",
                           end="integer",
                           name="character",
                           score="integer",
                           strand="character",
                           level="numeric",
                           signif="numeric",
                           score2="integer")
    
    if(colClasses[1] == 'implicit') {
        # TODO: if a strand column can be deduced, it SHOULD be deduced.
        # TODO: pshannon (10 jan 2013)
        tbl <- read.table(inputFiles(ahm)[1], sep="\t", header=FALSE)
        columnCount <- ncol(tbl)
        mandatory.colnames <- c("seqnames", "start", "end")
        if (columnCount > 3) {
            implicitColumnCount <- ncol(tbl) - 3
            implicitColumnNumbers <- 4:(3+implicitColumnCount)
            implicit.colnames = sprintf("col.%02d", implicitColumnNumbers)
            colnames <- c(mandatory.colnames, implicit.colnames)
            colnames(tbl) <- colnames
            gr <- with(tbl, GRanges(seqnames, IRanges(start, end)))
            other.colnames <- setdiff(colnames, mandatory.colnames)
            mcols(gr) <- DataFrame(tbl[, other.colnames])
            
        }
    } else {
        colnames <- names(colClasses)
        unused <- which(colnames == "")
        if(length(unused) > 0)
            colnames <- colnames[-unused]
        required.colnames <- c("seqnames", "start", "end", "strand")
        stopifnot(all(required.colnames %in% colnames))
        other.colnames <- setdiff(colnames, required.colnames)
        tbl <- read.table(inputFiles(ahm)[1], sep="\t", header=FALSE, colClasses=colClasses)
        colnames(tbl) <- colnames
        new.strand <- sub(".", "*", tbl$strand, fixed=TRUE)
        gr <- with(tbl, GRanges(seqnames, IRanges(start, end), new.strand))
        mcols(gr) <- DataFrame(tbl[, other.colnames])
    }
    
    # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(ahm)$Species,
                                   metadata(ahm)$Genome)
    # if gr only has a subset of all possible chromosomes, then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]
    
    save(gr, file=faOut)
    
    faOut
    
} 


makeAnnotationHubResource("EncodeImportPreparer", makeEncodeImporter)


