# This is a new recipe for EncodeImportPreparer-class.R 
.ucscBase <- "http://hgdownload.cse.ucsc.edu/"

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
        
        dv <- grep("dataVersion=", temp, value=TRUE)
        dv <- ifelse(length(dv)!=0, gsub(".*=", "", dv), NA_character_)
        
        ## get only important fields
        toMatch <- "dataType|lorigAssembly|type"
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
        
        list(tags=paste0(temp, collapse=", "), md5sum = md, 
             sourceVersion=dv) 
   })
}

.cleanFiles <- function(url, isSubDir=FALSE) {
    fls <- .httrRead(url, xpathString="//pre/a/text()")$files
        
    if(length(fls) != 0) {
        if(isSubDir){
            
            result <- .getTags(url)
            tags <- sapply(result, "[[", "tags")
            sourcemd5sum <- vapply(result, "[[",character(1),  "md5sum")
            sourceVersion <- vapply(result, "[[", "", "sourceVersion") 
            
            subst <- switch( basename(url),
                wgEncodeAwgTfbsUniform="wgEncodeAwgTfbs",
                wgEncodeAwgDnaseUniform="wgEncodeAwgDnase",
                wgEncodeGencodeV4="wgEncodeGencode",
                basename(url))                  
            
            fls <- fls[grepl(subst,fls)]
            fls <- fls[!grepl("files.txt", fls)]
            if(length(tags)!=0)
                fls <- list(filename=fls, tags=tags, sourcemd5sum=sourcemd5sum,
                            sourceVersion=sourceVersion)
	    }
    }    
    fls
}

.subDir <- function(url, verbose=TRUE) {
    contents <- .cleanFiles(url, isSubDir=TRUE)
    supported.formats <- c("narrowPeak", "broadPeak", "bedRnaElements", 
                           "gtf")
    tags <- contents$tags
    sourcemd5sum <- contents$sourcemd5sum
    files <- contents$filename
    sourceVersion <- contents$sourceVersion
    
    type <- sapply(strsplit(files, ".", fixed = TRUE), "[[", 2)
    idx <- type %in% supported.formats
    files <- files[idx]
    tags <- tags[idx]
    sourcemd5sum <- sourcemd5sum[idx]
    type <- type[idx]
    sourceVersion <- sourceVersion[idx]
 

    if(length(files)!=0) {
        files <-  sprintf("%s%s", url, files)
         if(length(files)>5){
             files<- files[1:5]
             tags<- tags[1:5]
             sourcemd5sum <- sourcemd5sum[1:5]
             type <- type[1:5]
             sourceVersion <- sourceVersion[1:5]
         }
            
        df <- .httrFileInfo(files, verbose)
        
        cbind(df, type, tags, sourcemd5sum, sourceVersion, 
              stringsAsFactors=FALSE)
    } else 
        data.frame(fileurl=character(), date=character(), size=numeric(),
                   type= character(), stringsAsFactors=FALSE)
}

.encodeFiles <- function(justRunUnitTest=FALSE) {
    encode_url <- paste0(.ucscBase, "goldenpath/hg19/encodeDCC/")
    subdirs <- .cleanFiles(encode_url, isSubDir=FALSE)
    urls <- setNames(paste0(encode_url, subdirs), subdirs)
    
    if(justRunUnitTest)
        urls <- urls[c(1,2,3,10,14)]
    
    do.call(rbind, Map(.subDir, urls, verbose=TRUE))
}

makeEncodeImporter <- function(currentMetadata, justRunUnitTest=FALSE,
                               BiocVersion=biocVersion()) {
    rsrc <- .encodeFiles(justRunUnitTest)
    
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- rsrc$sourceVersion # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    sourceType <- sapply(rsrc$type, function(x) 
        switch(x, 
               broadPeak="BED", 
               narrowPeak="BED",
               gtf="GTF", 
               bedRnaElements="BED"), 
        USE.NAMES =FALSE)
    
    dispatchclass <- sapply(rsrc$type, function(x)
       switch(x,
               broadPeak="UCSCBroadPeak",
               narrowPeak="UCSCNarrowPeak",
               gtf="GTFFile",
               bedRnaElements="UCSCBEDRnaElements"),
       USE.NAMES =FALSE)

    
    ## resources table
    title <- basename(rsrc$fileurl)
    description <- rsrc$description
    sourceMd5sum <- rsrc$sourcemd5sum
    
    rdatapath <- gsub(.ucscBase, "", sourceUrls) 
    
    tags <- strsplit(rsrc$tags, ", ")
    
    Map(AnnotationHubMetadata,
        
        SourceSize=sourceSize,
        SourceUrl=sourceUrls,
        SourceVersion=sourceVersion,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceType = sourceType,
	        
        Description= paste0(rsrc$type, " file from ENCODE"), 
        Title=title, 
        
        RDataPath=rdatapath,
        DispatchClass = dispatchclass,  
      
        Tags=tags,
        
        MoreArgs=list(
            BiocVersion=BiocVersion,
            # resources
            DataProvider = "UCSC",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Genome= "hg19",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",            
            Coordinate_1_based = FALSE,
            ##status_id =2L, 
            Location_Prefix = .ucscBase,
            RDataDateAdded = Sys.time(),
            ##PreparerClass = "EncodeImportPreparer",
            
            #rdata table
            RDataClass = "GRanges",
            
            Recipe = NA_character_))
}

makeAnnotationHubResource("EncodeImportPreparer", makeEncodeImporter)

