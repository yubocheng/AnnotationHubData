### =========================================================================
### Utility functions
### -------------------------------------------------------------------------
###

constructSeqInfo <- function(species, genome)
{
  recognized.human <- species=="Homo sapiens" & genome %in% c("hg18", "hg19")
  recognized.mouse <- species=="Mus musculus" & genome %in% c("mm10")
  recognized <- recognized.human | recognized.mouse
  stopifnot(recognized)
 
  suppressMessages({
       # chroms 1-22, X, Y, M are assumed to be the first 25 rows of the
       # data.frame
     if(recognized.human)
        tbl.chromInfo =
            GenomicFeatures:::.makeUCSCChrominfo (genome,
                                                  circ_seqs="chrM") [1:25,]
     if(recognized.mouse)
        tbl.chromInfo =
            GenomicFeatures:::.makeUCSCChrominfo (genome,
                                                  circ_seqs="chrM") [1:22,]
 
     })

   Seqinfo(as.character(tbl.chromInfo$chrom), 
           seqlengths=tbl.chromInfo$length, 
           isCircular=tbl.chromInfo$is_circular,
           genome=genome)
}

.sortTableByChromosomalLocation <- function(tbl)
{
  stopifnot (all (c ('seqname', 'start') %in% colnames (tbl)))
  factor.chromNames <- factor (tbl$seqname,
                               levels=paste("chr", c(1:22, "X", "Y", "M"),
                                            sep=''))
  tbl$seqname <- factor.chromNames
  tbl <- tbl [order (tbl$seqname, tbl$start), ]
  invisible (tbl)

} 

.printf <- function(...) print(noquote(sprintf(...)))

## from ?grep, by Luke Tierney
URL_parts <- function(x)
{
    m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
    parts <- do.call(rbind,
       lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
}

url.exists <- function(url)
{
   HEAD(url)$headers$status == "200"
}

## log messages to console AND to a file
flog <- function(level, ...)
{
    loggerFunction <- switch(level,
        "flog.fatal",
        "flog.error",
        "noop",
        "flog.warn",
        "noop",
        "flog.info",
        "noop",
        "flog.debug",
        "flog.trace"
        )
    dots <- list(...)
    do.call(loggerFunction, dots)
    dots$name <- "file"
    do.call(loggerFunction, dots)
}

## Uploading to S3 usually happens in AnnotationHubServer, but 
## when running the track*Recipe recipes, it happens in
## AnnotationHubData. There is an RAmazonS3 R package but 
## it does not work well for uploading files. Therefore this
## function expects the AWS CLI to be installed. 
## See: https://aws.amazon.com/cli/
## It should be configured with a user who can write to 
## the appropriate bucket. 
upload_to_S3 <- 
    function(file, remotename,
             bucket = getOption("ANNOTATION_HUB_BUCKET_NAME", "annotationhub"),
             profile, acl="public-read")
{
    remotename <- sub("^\\/", "", remotename)
    #aws --profile ahs_content_uploader s3 cp --acl public-read test s3://annotationhub/loquat/vato/manichean/test
    profileStr <- " "
    if (!missing(profile))
        profileStr <- paste("--profile ", profile)

    cmd <- "aws"
    if (length(file) != length(remotename))
        stop("Length of file does not match length of remotename!")

    for (i in 1:length(file)) {
        thisFile <- file[i]
        thisRemoteName <- remotename[i]
        quotes = getOption("useFancyQuotes")
        on.exit(options(useFancyQuotes=quotes))
        options(useFancyQuotes=FALSE)
        args <- sprintf("%s s3 cp --region us-east-1 --acl %s %s s3://%s/%s",
            profileStr, acl, dQuote(thisFile), bucket, dQuote(thisRemoteName))
        res <- system2(cmd, args)
        if (res != 0)
            stop(sprintf("Failed to upload %s to S3! Result was %s.", file, res))
    }
    
    TRUE
}


globalVariables(c("futile.logger"))

.onLoad <-
    function(libname, pkgname)
{
   logDir <- file.path(Sys.getenv("HOME"),
        sprintf(".%s", pkgname))
    if (!file.exists(logDir))
    {
        .printf("Creating log directory %s", logDir)
        dir.create(logDir)
    }
    l <- library
    l(futile.logger)
    flog.threshold(TRACE)
    flog.appender(appender.file(file.path(logDir,
        sprintf("%s.log", pkgname))), name="file")
}
