# This is a new recipe for EncodeImportPreparer-class.R 
.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.httrRead <- function(url) {
    tryCatch({
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- vapply(html["//pre/a/text()"], xmlValue, character(1))
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt")
        fls[!fls %in% remove ]
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
        url=character()
    })
}


.cleanFiles <- function(url, isSubDir=FALSE) {
    fls <- .httrRead(url)
        
    if(length(fls) != 0) {
        if(isSubDir){
             subst <- switch( basename(url),
               wgEncodeAwgTfbsUniform="wgEncodeAwgTfbs",
               wgEncodeAwgDnaseUniform="wgEncodeAwgDnase",
               wgEncodeGencodeV4="wgEncodeGencode",
               basename(url))                  
             fls <- fls[grepl(subst,fls)]    
	}
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
    subdirs <- .cleanFiles(encode_url, isSubDir=FALSE)
    urls <- setNames(paste0(encode_url, subdirs), subdirs)
    
    df <- do.call(rbind, Map(.subDir, urls, verbose=TRUE))
        
    title <- basename(df$fileurl)
    sourceUrl <- sub(.ucscBase , "", df$fileurl)
        
    cbind(df, title, sourceUrl, stringsAsFactors = FALSE)
}

makeEncodeImporter <- function(currentMetadata) {
    rsrc <- .encodeFiles()

    description <- paste0(rsrc$type, " file from ENCODE")
    sourceFile <- rsrc$title
    title <- rsrc$title
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- lapply(rsrc$type, c, "EncodeDCC", "hg19")
            
    Map(AnnotationHubMetadata,
        Description=description, 
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion,
        Title=title, Tags=Tags,
        MoreArgs=list(
            Genome= "hg19",
            Species="Homo sapiens",
            TaxonomyId=9606L,
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

