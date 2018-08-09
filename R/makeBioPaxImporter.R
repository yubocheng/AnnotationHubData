## This recipe is no longer used. If reinstated, add unit test back
## to test_recipes.R.

test_BioPaxImportPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
      preparerClasses = "BioPaxImportPreparer",
      insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

.nihBaseUrl <- "ftp://ftp1.nci.nih.gov/"
.amazonBaseUrl <- "http://s3.amazonaws.com/annotationhub/"

.getBioPax <- function(url) {
    files <- .ftpFileInfo(url, extension="owl.gz")
}    

.getBioPaxFilesNIH <- function(justRunUnitTest=FALSE) {
    paths <- c(bioPax="BioPAX/", 
               bioPaxLevel2="BioPAX_Level_2/")
               #biocPaxLevel3="BioPAX_Level_3/")
    
    baseUrl <- paste0(.nihBaseUrl, "pub/PID/")
    
    urls <- setNames(paste0(baseUrl, paths), names(paths))
     
    df <- do.call(rbind, Map(.getBioPax, urls))
    
    if(justRunUnitTest)
        df <- df[1:2, ]  ## just first 2 files from first url.
       
    title <- basename(df$fileurl)
    
    tempTags <- strsplit(sub(".owl.gz", "", title),"\\.")
    fileSource <- sapply(tempTags, function(x)  x[1])
    bpLevel <- sapply(tempTags, function(x)  x[2])
    bpLevel[is.na(bpLevel)] <- "bp"
    
    sourceType <- sapply(bpLevel, function(x) 
        switch(x, 
               bp3="BioPaxLevel3", 
               bp2="BioPaxLevel2",
               bp="BioPax"), 
        USE.NAMES =FALSE)
    
    tags <- unlist(mapply(function(x,y) {
        paste(x, y, "Pathway Interaction Database", sep=", ")
    }, fileSource, sourceType, USE.NAMES=FALSE))
    
    description <- paste0(fileSource, 
        " BioPax file from NCI Pathway Interaction Database")
    
    cbind(df, title,tags, sourceType, description, stringsAsFactors=FALSE)
}

makeBioPaxImporter <- function(currentMetadata, justRunUnitTest=FALSE,
                               BiocVersion=BiocManager::version()) {
    rsrc <- .getBioPaxFilesNIH(justRunUnitTest)
    
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
    rdatapath <- paste0(rdatapath, ".Rda")

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
            BiocVersion=BiocVersion,
            DataProvider = "NIH Pathway Interaction Database",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Genome= "hg19",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",            
            Coordinate_1_based = FALSE,
            RDataClass = "biopax",   
            DispatchClass = "BioPax",
            Location_Prefix = .amazonBaseUrl,
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::makeBioPaxRdata")
	    #Recipe =c("makeBioPaxRdata", package="AnnotationHubData"))
    )
}

## recipe
makeBioPaxRdata <- function(ahm)
{
    ahmroot <- metadata(ahm)$AnnotationHubRoot  # path on localDir
    
    ## contains truncated path to .Rda file on localDir
    rdatapath <- metadata(ahm)$RDataPath # contains truncated path to .Rda file on localDir
    
    ## path to original Biopax file on the web
    originalFile <- basename(metadata(ahm)$SourceUrl)  
    
    ## contains truncated path to original file on localDir
    originalPath <- gsub(basename(rdatapath), originalFile, rdatapath)   
  
    faIn <- file.path(ahmroot, originalPath)
    faOut <- file.path(ahmroot, rdatapath)
   
    if(!file.exists(faOut)) {   
        rb <- rBiopaxParser::readBiopax(faIn)
        save(rb, file=faOut)
    } 
    faOut
}

makeAnnotationHubResource("BioPaxImportPreparer", makeBioPaxImporter)
