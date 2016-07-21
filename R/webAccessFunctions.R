### =========================================================================
### HTTP and FTP helpers
### -------------------------------------------------------------------------
###

.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Remove remove "\n" when inserting long text (> 80 chars)
.expandLine <- function(x)
    gsub("[[:space:]]{2,}"," ", x)

## -----------------------------------------------------------------------------
## HTTP
## 

## Parses xml from a http page into a data.frame of filenames, 
## List filenames or filenames ending with an extension on http page.
## Also reads the md5sum in "md5sum.txt" on the same http page.
.httrRead <- function(url, xpathString="//pre/a/text()", 
                      extension=NA_character_, getmd5sum=FALSE) {
    tryCatch({
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)

        ## httr >=1.1.0 uses xml2 instead of XML
        fls <- as.character(xml_find_all(html, xpathString)) 

        md5exists <- length(grep("md5sum.txt", fls))!=0
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt", "supplemental/")
        fls <- fls[!fls %in% remove ]

        ## filter by extension
        if(!is.na(extension)){
            fls <- fls[grepl(paste0(extension, "$"), fls)]
        }

        ## UCSC chain and 2bit files have a file called md5sum.txt
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


## Returns data.frame with fileurl, last modified date and file size
.httrFileInfo <- function(urls, verbose=TRUE) {
    result <- lapply(urls, function(f){
        if(verbose)
            message(paste0("getting file info: ", basename(f)))
        tryCatch({
            h = suppressWarnings(
              GET(f, config=config(nobody=TRUE, filetime=TRUE)))

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

    data.frame(fileurl=urls, date, size, stringsAsFactors=FALSE)
}

## -----------------------------------------------------------------------------
## FTP 
## 

## Returns a data.frame with fileurl, last modified date and file size.
## 'extension' can be a single file name with extension or just the extension.
.ftpFileInfo <- function(url, extension, verbose=FALSE) {

    curlHandle <- curl::new_handle()
    curl::handle_setopt(curlHandle, dirlistonly=TRUE)
    if (verbose)
        message(paste0("creating urls ..."))

    result <- lapply(url, function(ul) {
        con <- curl::curl(ul, "r")
        txt <- read.table(con, stringsAsFactors=FALSE, fill=TRUE)
        close(con)

        files <- txt[[9]]
        if (verbose)
            message(basename(ul))

        pattern <- paste(paste0(extension, "$"), collapse="|")
        keep <- !grepl("00-", files) & grepl(pattern, files)
        txt <- txt[keep, ]
        if (nrow(txt) == 0L)
            return(data.frame(fileurl=character(), 
                              date=as.POSIXct(character()),
                              size=numeric()))

        # last modified date and size
        dateraw <- apply(txt, 1, function(xx) paste(xx[6], xx[7], xx[8]))
        datestring <- lapply(dateraw, function(xx) {
            as.POSIXct(strptime(xx, format="%b %e %H:%M", tz="GMT"))
        })
        if (any(is.na(datestring))) {
            datestring <- lapply(dateraw, function(xx) {
                as.POSIXct(strptime(xx, format="%b %e %Y", tz="GMT"))
            })
        }

        data.frame(fileurl=paste0(ul, txt[[9]]), date=do.call(c, datestring), 
                   size=as.numeric(txt[[5]]), stringsAsFactors=FALSE)
    })

    do.call(rbind, result)
}

# Return unparsed directory listing as character vector
.ftpDirectoryInfo <- function(someUrl, filesOnly=FALSE) {
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest="LIST -R")
  con <- curl::curl(someUrl, handle=curlHandle)
  retVal <- readLines(con)
  close(con)
  curl::handle_reset(curlHandle)
  # Return listing as rows
  return (retVal);
}

## Return just the names of the files in an FTP directory
## Note, this will not do any cleaning of symlinks
.listRemoteFiles <- function(someUrl){
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, dirlistonly=TRUE)
  con <- curl::curl(someUrl, handle=curlHandle)
  retVal <- readLines(con)
  close(con)
  curl::handle_reset(curlHandle)

  return (retVal);
}
