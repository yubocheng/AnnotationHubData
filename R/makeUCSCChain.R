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
