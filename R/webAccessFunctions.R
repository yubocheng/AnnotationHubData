
## Using this function, once can read all the filenames or filenames ending with a 
## cetrain extension on an http page, it also reads the 
## md5sum which is present in "md5sum.txt" on the same http page
.httrRead <- function(url, fileName, getmd5sum=FALSE) {
    tryCatch({
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- vapply(html["//pre/a/text()"], xmlValue, character(1))
        
        md5exists <- length(grep("md5sum.txt", fls))!=0
        
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/", 
                    "files.txt", "md5sum.txt")
        fls <- fls[!fls %in% remove ]
                
        ## we want to read in only files with a specific extension here. 
        if(!is.na(fileName)){
            fls <- fls[grepl(paste0(fileName, "$"), fls)]
        }
        
        ## the chain files and the ucsc 2bit files have a file called md5sum.txt
        ## col1=md5sum, col2=filename
        ## note : not all chain files have md5sum on UCSC website!
        if(getmd5sum & md5exists & length(fls!=0)) {
            df <- read.table(paste0(url, "/", "md5sum.txt"), header=FALSE, 
                             stringsAsFactors=FALSE)
            md5sum <- df[match(fls, df[,2]),1]
            df <- data.frame(files=fls, md5sum=md5sum,  stringsAsFactors=FALSE)
        } else 
            df <- data.frame(files=fls, stringsAsFactors=FALSE)
        df
        
    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
        url=character()
    })
}

## Using this function, once can read all the filenames or filenames ending with a 
## cetrain extension on an FTP page, it also gets the date the file was last 
## modified and the file size. 
.ftpFileInfo <- function(url, filename, tag, verbose=TRUE) {
    txt <- getURL(url, dirlistonly=TRUE, curl=handle_find(url))
    df2 <- strsplit(txt, "\n")[[1]]
    df2 <- df2[grep(paste0(filename, "$"), df2)]
    drop <-  grepl("latest", df2) | grepl("00-", df2)
    df2 <- df2[!drop]
    df2 <- paste0(url, df2)
    
    result <- lapply(df2, function(x){
        if(verbose)
            message(basename(x))
        tryCatch({
            h = suppressWarnings(
                GET(x, config=config(nobody=TRUE, filetime=TRUE)))
            nams <- names(headers(h))
            if("last-modified" %in% nams)
                headers(h)[c("last-modified", "content-length")] 
            else
                c("last-modified"=NA, "content-length"=NA)	
        }, error=function(err) {
        warning(basename(x), ": ", conditionMessage(err))
            list("last-modified"=character(), "content-length"=character())
        })
    }) 
      
    size <- as.numeric(sapply(result, "[[", "content-length"))
    date <- strptime(sapply(result, "[[", "last-modified"),
                     "%a, %d %b %Y %H:%M:%S", tz="GMT")
        
    data.frame(fileurl=url, date, size, genome=tag, stringsAsFactors=FALSE)
}

## remove leading and trailing white spaces
.trim <- function (x) gsub("^\\s+|\\s+$", "", x)


## while inserting long text(more than 80 chars), we want to remove "\n"
.expandLine <- function(x)
    gsub("[[:space:]]{2,}"," ", x)


## for files on http sites, get the file size and file's date last modified. 
.httrFileInfo <- function(files, verbose=TRUE) {
    result <- lapply(files, function(f){
        if(verbose)
            message(basename(f))
        tryCatch({    
            h = GET(f, config=config(nobody=TRUE, filetime=TRUE))
            stop_for_status(h)
            nams <- names(headers(h))
            if("last-modified" %in% nams)
                 headers(h)[c("last-modified", "content-length")] 
	    else
                c("last-modified"=NA, "content-length"=NA)	
        }, error=function(err) {
        warning(basename(f), ": ", conditionMessage(err))
        list("last-modified"=character(), "content-length"=character())
        }) 
    })
    
    
    size <- as.numeric(sapply(result, "[[", "content-length")) 
    date <- strptime(sapply(result, "[[", "last-modified"),
             "%a, %d %b %Y %H:%M:%S", tz="GMT")
    
    data.frame(fileurl=files, date, size, stringsAsFactors=FALSE)	
}    

## check if file exists online. 
.fileExistsOnline  <- function(file) {
   sapply(file, function(z) {
       try(
           z <- getBinaryURL(file, failonerror = TRUE)
       )  
       if (length(z) > 1) 
           TRUE
       else
           FALSE
    }) 
}

## currently not in use - used to parse a http page and get the file size 
## and file's date last modified. 
.fileInfoRCurl <- function(url, verbose=FALSE) {
    require(XML)
    tryCatch({
        result <- lapply(url, function(f) {
            message(basename(f))
            h <- basicTextGatherer()
            ok <- curlPerform(url=f,
                              nobody=TRUE, headerfunction=h$update)
            yy <- h$value()
            list(date=sub(".*Last-Modified: ([[:print:]]+) GMT.*", "\\1", yy),
                 size=sub(".*Content-Length: ([[:digit:]]+).*","\\1", yy))
        })
        date <- strptime(sapply(result, "[[", "date"),
                         "%a, %d %b %Y %H:%M:%S", tz="GMT")
        size <- as.integer(sapply(result, "[[", "size"))
        
        data.frame(url, date, size, stringsAsFactors=FALSE)
    }, error=function(err) {
        warning(basename(dirname(url)), ": ", conditionMessage(err))
        data.frame(url=character(), date=character(), size=character(),
                   stringsAsFactors=FALSE)
    })
}
