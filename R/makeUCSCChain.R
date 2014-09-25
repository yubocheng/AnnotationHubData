.get1ChainResource <- function(url, verbose=FALSE) {
    require(XML)
    tryCatch({
        if (verbose)
            message(basename(dirname(url)))
        html <- htmlParse(url)
        fls <- sapply(html["//pre[2]/a/text()"], xmlValue)
        url <- sprintf("%s/%s", url, fls)
        keep <- grepl("chain.gz", fls)
        stamps <- sapply(html["//pre[2]/text()"], xmlValue)
        regex <- " *([[:alnum:]-]+) *([:[:alnum:]]+)*.*"
        version <- sub(regex, "\\1_\\2", stamps)
        data.frame(url, version, stringsAsFactors=FALSE)[keep,,drop=FALSE]
    }, error=function(err) {
        warning(basename(dirname(url)), ": ", conditionMessage(err))
        data.frame(url=character(), version=character(), stringsAsFactors=FALSE)
    })
}

.getUCSCChainResources <-
    function(verbose=FALSE)
{
    .chainBase <- "http://hgdownload.cse.ucsc.edu/goldenPath"
    genomes <- rtracklayer::ucscGenomes()$db
    urls <- sprintf("%s/%s/liftOver", .chainBase, genomes)
    do.call(rbind, lapply(urls, .get1ChainResource, verbose=verbose))
}

.parseFileName <- function(table)
{
    FileNames <- sapply(table[,1], function(x) 
        unlist(strsplit(basename(x),".over.chain.gz")))
    FromToNames <- lapply(FileNames, function(x){
        fromto <- unlist(strsplit(x,"To"))
        from <- fromto[1]
        to <- fromto[2]
        to <- paste0(tolower(substring(to,1,1)), substring(to,2,nchar(to)))
        c(from, to)
    })
    names(FileNames) <- NULL
    names(FromToNames) <- NULL
    df <- cbind(FileNames, t(as.data.frame(FromToNames)))
    row.names(df) <- NULL
    df
}
