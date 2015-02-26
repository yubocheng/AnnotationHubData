
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
.ftpFileInfo <- function(url, filename, tag) {
    tryCatch({
        df2 <- strsplit(getURL(url, dirlistonly=TRUE), "\r\n")[[1]]
        
        df2 <- df2[grep(paste0(filename, "$"), df2)]
        drop <-  grepl("latest", df2) | grepl("00-", df2)
        df2 <- df2[!drop]
        df2 <- paste0(url, df2)
        
        result <- lapply(df2, function(x){
            h = suppressWarnings(
                GET(x, config=config(nobody=TRUE, filetime=TRUE)))
            headers(h)[c("last-modified", "content-length")] 
        })
        
        size <- as.numeric(sapply(result, "[[", "content-length"))
        date <- strptime(sapply(result, "[[", "last-modified"),
                         "%a, %d %b %Y %H:%M:%S", tz="GMT")
        
        data.frame(fileurl=df2, date, size, genome=tag, stringsAsFactors=FALSE)
    }, error=function(err) {
        warning(basename(dirname(url)), ": ", conditionMessage(err))
        data.frame(url=character(), version=character(), 
                   stringsAsFactors=FALSE)
    })
}

## remove leading and trailing white spaces
.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## for files on http sites, get the file size and file's date last modified. 
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