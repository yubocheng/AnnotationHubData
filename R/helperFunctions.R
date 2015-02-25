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
        ## lying in the same folder - here we read it in. 
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

.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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


.get1Resource <- function(url, fileName, genome, verbose=FALSE) {
    require(XML)
    tryCatch({
        if (verbose)
            message(basename(dirname(url)))
        
        html <- htmlParse(url)
        fls <- sapply(html["//pre[2]/a/text()"], xmlValue)
        if(length(fls)==0)
            fls <- sapply(html["//pre[1]/a/text()"], xmlValue)
        
        ## extract the file name
        url <- sprintf("%s/%s", url, fls)
        keep <- grepl(paste0(fileName, "$"), fls)
        url <- url[keep]
        
        result <- lapply(url, function(f) {
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
        data.frame(url=character(), version=character(), 
                   stringsAsFactors=FALSE)
    })
}