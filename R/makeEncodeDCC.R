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
                    "md5sum.txt")
        fls[!fls %in% remove ]
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
        url=character()
    })
}

.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

.getTags <- function(url) {
    tagurl <- paste0(url, "files.txt")
    result <- GET(tagurl)
    stop_for_status(result)
    html <- content(result)
    
    html <- unlist(strsplit(html, "\n")) # split to get tags for each file
    lapply(html, function(t) {
        ta <- unlist(strsplit(t, "\t"))
        temp <- unlist(strsplit(ta[2],";"))
        temp <- .trim(temp)
        
        ## extract the md5sum if present
        md <- grep("md5sum=", temp, value=TRUE)
        md <- ifelse(length(md), gsub(".*=","", md), NA_character_)    
        
        ## change "cell=8988T" to "8988T cell"
        n <- grep("cell=", temp, value=TRUE)
        n <- ifelse(length(n)!=0, paste0(gsub(".*=", "", n)," cell"), 
                    NA_character_)

        ## change "grant=Gingeras" to "Gingeras grant"
        g <- grep("grant=", temp, value=TRUE)
        g <- ifelse(length(g)!=0, paste0(gsub(".*=", "", g)," grant"), 
                    NA_character_)
    
        ## get only important fields
        toMatch <- "dataVersion|dataType|lorigAssembly|type"
        temp <- temp[grepl(toMatch, temp)]
        
        ## remove everything before "="
        temp <- gsub(".*=","", temp)
        
        ## add
        if(!is.na(n))
            temp <- c(temp, n)
        
        if(!is.na(g))
            temp <- c(temp, g)
    
        temp <- c("wgEncode", temp)
        temp <- temp[!grepl("None",temp)]
        
        list(tags=paste0(temp, collapse=", "), md5sum = md) 
   })
}

.cleanFiles <- function(url, isSubDir=FALSE) {
    fls <- .httrRead(url)
        
    if(length(fls) != 0) {
        if(isSubDir){
            tags <- character(0)
            sourcemd5sum <- character(0)
            
            if(grep("files.txt",fls)){
                result <- .getTags(url)
                tags <- sapply(result, "[[", "tags")
                sourcemd5sum <- sapply(result, "[[", "md5sum")
            }               
            
            subst <- switch( basename(url),
                wgEncodeAwgTfbsUniform="wgEncodeAwgTfbs",
                wgEncodeAwgDnaseUniform="wgEncodeAwgDnase",
                wgEncodeGencodeV4="wgEncodeGencode",
                basename(url))                  
            
            fls <- fls[grepl(subst,fls)]
            fls <- fls[!grepl("files.txt", fls)]
            if(length(tags)!=0)
                fls <- list(filename=fls, tags=tags, sourcemd5sum=sourcemd5sum)
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
    tags <- contents$tags
    sourcemd5sum <- contents$sourcemd5sum
    files <- contents$filename
    
    type <- sapply(strsplit(files, ".", fixed = TRUE), "[[", 2)
    idx <- type %in% supported.formats
    files <- files[idx]
    tags <- tags[idx]
    sourcemd5sum <- sourcemd5sum[idx]
    type <- type[idx]
    
    if(length(files)!=0) {
        files <-  sprintf("%s%s", url, files)
         if(length(files)>5){
             files<- files[1:5]
             tags<- tags[1:5]
             sourcemd5sum <- sourcemd5sum[1:5]
             type <- type[1:5]
         }
            
        df <- .httrFileInfo(files, verbose)
        cbind(df, type, tags, sourcemd5sum, stringsAsFactors=FALSE)
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
    sourceMd5sum <- rsrc$sourcemd5sum
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- strsplit(rsrc$tags, ", ")
            
    Map(AnnotationHubMetadata,
        Description=description, 
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion,
        Title=title, Tags=Tags,
        SourceMd5= sourceMd5sum,
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

