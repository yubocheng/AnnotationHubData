## This is an example of how this new helper method can make things
## simpler and also provides a test case for how we can parse ensembl
## GTF files into GRanges objects.

## Helper to retrieve GTF file urls from Ensembl 
.ensemblGtfSourceUrls <-
    function(baseUrl, justRunUnitTest)
{
    rel <- seq(69,81,1) 
    want <- paste0(baseUrl, "release-", rel, "/gtf/")
     
    if(justRunUnitTest)
        want <- want[1]
    
    ## files in release
    
    urls <- unlist(lapply(want, function(url) {
        listing <- .ftpDirectoryInfo(url)
        subdir <- sub(".* ", "", listing[grep("^drwx", listing)])
        paste0(url, subdir, "/")
    }), use.names=FALSE)

    if(justRunUnitTest)
        urls <- urls[1:5] 
 
    df <- .ftpFileInfo(url=urls, filename="gtf.gz", tag=basename(urls))  
    rownames(df) <- NULL
    df
}

## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeEnsemblGTFsToAHMs <- function(currentMetadata, 
                                  baseUrl = "ftp://ftp.ensembl.org/pub/",
                                  justRunUnitTest = FALSE, 
                                  BiocVersion = biocVersion()){
    ## get possible sources
    df <- .ensemblGtfSourceUrls(baseUrl, justRunUnitTest)
    sourceUrls <- df$fileurl
    
    ## construct datapath for the saved GRanges
    rd <- gsub(baseUrl, "", sourceUrls)
    rdata <- sub(".gz$", ".RData", rd)
    rdata <- paste0("ensembl/", rdata)
    
    meta <- .ensemblMetadataFromUrl(sourceUrls)
    description <- paste("Gene Annotation for", meta$species)

    Map(AnnotationHubMetadata,
        Description=description, Genome=meta$genome,
        SourceUrl=sourceUrls,
	SourceSize=as.numeric(df$size),
	SourceLastModifiedDate=df$date,
        SourceVersion=meta$sourceVersion, 
        Species=meta$species,
        RDataPath=rdata,
        TaxonomyId=meta$taxonomyId, Title=meta$title,
        MoreArgs=list(
          BiocVersion=BiocVersion,
          Coordinate_1_based = TRUE,
          DataProvider = "Ensembl",
          Maintainer = "Martin Morgan <mtmorgan@fredhutch.org>",
          RDataClass = "GRanges",
	  DispatchClass="GRanges",
          SourceType="GTF",  
          Location_Prefix=.amazonBaseUrl, 
          RDataDateAdded = Sys.time(),
          Recipe = "AnnotationHubData:::ensemblGTFToGRangesRecipe", 
          Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
}



## STEP 2: Make a recipe function that takes an AnnotationHubMetadata
## object.
ensemblGTFToGRangesRecipe <- function(ahm){
    
    message()
    message("###############################################################") 
    
    ahmroot <- metadata(ahm)$AnnotationHubRoot  # path on localDir
    
    ## contains truncated path to .Rda file on localDir
    rdatapath <- metadata(ahm)$RDataPath

    ## path to original Biopax file on the web
    originalFile <- basename(metadata(ahm)$SourceUrl)  

    ## contains truncated path to original file on localDir
    originalPath <- gsub(basename(rdatapath), originalFile, rdatapath) 

    faIn <- file.path(ahmroot, originalPath)
    faOut <- file.path(ahmroot, rdatapath)
   
    message("faIn:", faIn)
    message("faOut:", faOut)

    if(!file.exists(faOut)) {   
        gr <- import(faIn, "gtf")
        save(gr, file=faOut)
    } 
    faOut
}

## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("EnsemblGtfImportPreparer",
                          makeEnsemblGTFsToAHMs)
