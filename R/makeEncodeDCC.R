# This is a new recipe for EncodeImportPreparer-class.R 
.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

.readHtmlPage <- function(url, isSubDir=FALSE) {
    tryCatch({
        #html <- htmlParse(url)
        result <- GET(url)
        stop_for_status(result)
        html <- content(result)
        
        fls <- sapply(html["//pre/a/text()"], xmlValue)
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt")
        fls <- fls[-which(fls %in% remove )]
        
        if(isSubDir)
            fls <- fls[grepl(basename(url), fls)]
        
        fls
    }, error=function(err) {
        browser()
        warning(basename(url), ": ", conditionMessage(err))
        url=character()
    })
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
        browser()
        warning(basename(files), ": ", conditionMessage(err))
        url=character()
    })    
}


.subDir <- function(url, verbose=TRUE) {
    contents <- .readHtmlPage(url, isSubDir=TRUE)
    supported.formats <- c("narrowPeak", "broadPeak", "bedRnaElements", 
                           "gtf")
    type <- sapply(strsplit(contents,".",fixed = TRUE),"[[",2)
    contents <- contents[type %in% supported.formats]
            
    if(length(contents)!=0) {
        files <-  sprintf("%s%s", url, contents)
        .httrFileInfo(files ,verbose)
    } else 
        data.frame(fileurl=character(), date=character(), size=numeric(),
                   stringsAsFactors=FALSE)
}

.encodeFiles <- function(){
    encode_url <- paste0(.ucscBase, "goldenpath/hg19/encodeDCC/")
    subdirs <- .readHtmlPage(encode_url, isSubDir=FALSE)
    urls <- setNames(paste0(encode_url, subdirs), subdirs)
    
    df <- do.call(rbind, Map(.subDir, urls, verbose=TRUE))
        
    title <- basename(df$fileurl)
    sourceUrl <- sub(.ucscBase , "", df$fileurl)
        
    cbind(df, title, sourceUrl, stringsAsFactors = FALSE)
}

makeEncodeImporter <- function(currentMetadata) {
    rsrc <- .encodeFiles()
    
    # previously the description field just contained the name of the file
    
    description <- rsrc$title
    genome <- rep("hg19", nrow(rsrc))
    sourceFile <- rsrc$title
    title <- rsrc$title
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- sapply(rsrc$date, function(y) gsub(" ","_",y)) 
    ## should be character
    species <- rep("Homo sapiens", nrow(rsrc))
    taxonomyId <- rep(9606L, nrow(rsrc))
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- lapply(rsrc$title, function(tag) {
        c("EnodeDCC", as.character(tag), "hg19")
    })
    
    Map(AnnotationHubMetadata,
        Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title, Tags=Tags,
        MoreArgs=list(
            Coordinate_1_based = FALSE,
            DataProvider = "hgdownload.cse.ucsc.edu",
            Location_Prefix = .ucscBase,
            Maintainer = "Sonali Arora <sarora@fhcrc.org>",
            RDataClass = "EncodeImportPreparer", # paul is returning GRanges!
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.2",
            Recipe = NA_character_))
}

makeAnnotationHubResource("EncodeImportPreparer", makeEncodeImporter)


