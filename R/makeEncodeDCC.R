# This is a new recipe for EncodeImportPreparer-class.R 
.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.httrRead <- function(url) {
    tryCatch({
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- sapply(html["//pre/a/text()"], xmlValue)
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt")
        fls[-which(fls %in% remove )]
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
        url=character()
    })
}


.cleanFiles <- function(url, isSubDir=FALSE) {
    fls <- .httrRead(url)
        
    if(length(fls) != 0) {
        faultyfolders <-c( "wgEncodeAwgTfbsUniform", 
                           "wgEncodeAwgDnaseUniform", "wgEncodeGencodeV4")
        if(isSubDir){
            if(basename(url) %in% faultyfolders){
                if(basename(url)=="wgEncodeAwgTfbsUniform")
                    fls <- fls[grepl("wgEncodeAwgTfbs", fls)]
                
                if(basename(url)=="wgEncodeAwgDnaseUniform")
                    fls <- fls[grepl("wgEncodeAwgDnase", fls)]
                
                if(basename(url)=="wgEncodeGencodeV4")
                    fls <- fls[grepl("wgEncodeGencode", fls)]
            } else { # for main dir
                fls <- fls[grepl(basename(url), fls)]
            }
        }
    } else { # no files returned 
        fls <- character(0)
    }    
    fls
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
    contents <- .cleanFiles(url, isSubDir=TRUE)
    supported.formats <- c("narrowPeak", "broadPeak", "bedRnaElements", 
                           "gtf")
    type <- sapply(strsplit(contents,".",fixed = TRUE),"[[",2)
    idx <- type %in% supported.formats
    contents <- contents[idx]
    type <- type[idx]        
    if(length(contents)!=0) {
        files <-  sprintf("%s%s", url, contents)
#         if(length(files)>5){
#             files<- files[1:5]
#             type<- type[1:5]
#         }
            
        df <- .httrFileInfo(files ,verbose)
        cbind(df, type=type, stringsAsFactors=FALSE)
    } else 
        data.frame(fileurl=character(), date=character(), size=numeric(),
                   type= character(), stringsAsFactors=FALSE)
}

.encodeFiles <- function(){
    encode_url <- paste0(.ucscBase, "goldenpath/hg19/encodeDCC/")
    subdirs <- .cleanFiles(encode_url, isSubDir=FALSE)
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
    Tags <- lapply(rsrc$type, function(tag) {
        c(as.character(tag), "EnodeDCC","hg19")
    }) 
        
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
            RDataClass = "EpigenomeRoadmapFile", 
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.2",
	    Recipe = NA_character_))
}

makeAnnotationHubResource("EncodeImportPreparer", makeEncodeImporter)

