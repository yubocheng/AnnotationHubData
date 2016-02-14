
## Using this function, one can read all the filenames or filenames ending with a
## cetrain extension on an http page, it also reads the
## md5sum which is present in "md5sum.txt" on the same http page
.httrRead <- function(url, xpathString, fileName=NA_character_, getmd5sum=FALSE) {
    tryCatch({
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)

        ## httr >=1.1.0 uses xml2 instead of XML
        #fls <- as.character(xml_find_all(html, "//pre/a/text()")) 
        fls <- as.character(xml_find_all(html, xpathString)) 

        md5exists <- length(grep("md5sum.txt", fls))!=0
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt", "supplemental/")
        fls <- fls[!fls %in% remove ]

        ## we want to read in only files with a specific extension here.
        if(!is.na(fileName)){
            fls <- fls[grepl(paste0(fileName, "$"), fls)]
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

## Using this function, once can read all the filenames or filenames ending with a
## cetrain extension on an FTP page, it also gets the date the file was last
## modified and the file size.
.ftpFileInfo <- function(url, filename, tag) {
  firsturl <- ifelse(length(url)>1, url[1], url)
    # Establish the curl handle
    curlHandle <- curl::new_handle()
    curl::handle_setopt(curlHandle, dirlistonly=TRUE)

  ## make a list of filenames from each url
  allurls <- lapply(url, function(ul){
    # Open the connecting in "r" (read text) mode.
    con <- curl::curl(ul, "r")
    txt <- read.table(con, stringsAsFactors=FALSE, fill=TRUE)
    close(con)

    df2 <- txt[[9]]
    df2 <- df2[grep(paste0(filename, "$"), df2)]
    drop <- grepl("00-", df2)
    df2 <- df2[!drop]

    paste0(ul, df2)
  })
  allurls <- unlist(allurls)

  curl::handle_reset(curlHandle)

  ## use httr to get date and size for each file.
  df <- .httrFileInfo(allurls, verbose=TRUE)
  base::cbind(df, genome=tag, stringsAsFactors=FALSE)
}

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

    data.frame(fileurl=files, date, size, stringsAsFactors=FALSE)
}

## FIXME: AFAICT this isn't used in AHD; do we need this?
## Check if a given file exists online.
.fileExistsOnline  <- function(url) {
    sapply(url, function(innerUrl) {
        tryCatch({
            urlHeaders <- httr::HEAD(innerUrl)
            ## VO: fail based on status instead of 'headers.content-length'
            if (urlHeaders$status_code >= 300)
                FALSE
            else 
                TRUE
        }, error=function(err){
            message(paste0("Error retrieving URL '", url, "': \n\t", err))
            FALSE
        })
    })
}

# Return a full directory listing (with size, permissions, last modified)
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
