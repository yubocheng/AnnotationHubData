.nihBaseUrl <- "ftp://ftp1.nci.nih.gov/"

.getBioPax <- function(url) {
    files <- .ftpFileInfo(url, filename="owl.gz", tag="hg19")
}    

.getBioPaxFilesNIH <- function() {
    paths <- c(bioPax="BioPAX/", 
               bioPaxLevel2="BioPAX_Level_2/",
               biocPaxLevel3="BioPAX_Level_3/")
    
    baseUrl <- paste0(.nihBaseUrl, "pub/PID/")
    
    urls <- setNames(paste0(baseUrl, paths), names(paths))
    
    df <- do.call(rbind, Map(.getBioPax, urls))
    
    title <- basename(df$fileurl)
    
    tempTags <- strsplit(gsub(".owl.gz", "", title),"\\.")
    fileSource <- sapply(tempTags, function(x)  x[1])
    bpLevel <- sapply(tempTags, function(x)  x[2])
    bpLevel[is.na(bpLevel)] <- "bp"
    
    sourceType <- sapply(bpLevel, function(x) 
        switch(x, 
               bp3="BioPaxLevel3 file", 
               bp2="BioPaxLevel2 file",
               bp="BioPax file"), 
        USE.NAMES =FALSE)
    
    tags <- unlist(mapply(function(x,y) {
        paste(x, y, "Pathway Interaction Database", sep=", ")
    }, fileSource, sourceType, USE.NAMES=FALSE))
    
    description <- paste0(fileSource, 
        " BioPax file from NCI Pathway Interaction Database")
    
    cbind(df, title,tags, sourceType, description, stringsAsFactors=FALSE)
}

makeBioPaxImporter <- function(currentMetadata) {
    rsrc <- .getBioPaxFilesNIH()
    
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    sourceType <- rsrc$sourceType
    
    ## resources table
    title <- rsrc$title
    description <- rsrc$description
    
    rdatapath <- sub(.nihBaseUrl, "", sourceUrls)
        
    tags <- strsplit(rsrc$tags, ", ")
    
    Map(AnnotationHubMetadata,
        
        SourceSize=sourceSize,
        SourceUrl=sourceUrls,
        SourceVersion=sourceVersion,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceType = sourceType,
        
        Description=description, 
        Title=title, 
        
        RDataPath=rdatapath,
            
        Tags=tags,
        
        MoreArgs=list(
            DataProvider = "NIH PID",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Genome= "hg19",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",            
            Coordinate_1_based = FALSE,
            rdataclass <- "data.frame"   
            dispatchclass <- "importBioPax"
            Location_Prefix = .nihBaseUrl,
            RDataDateAdded = Sys.time(),
            Recipe = NA_character_)
    )
}

makeAnnotationHubResource("BioPaxImportPreparer", makeBioPaxImporter)
