## old file - RefNetImportPreparer-class.R 
##changes - title should have full file name, species is "Homo sapiens" not 9606
## what is RefNet Genome?
## tags looks like "interactions, interactions from gerstein-2012"

.amazonBaseUrl <- "hhttps://bioconductorhubs.blob.core.windows.net/annotationhub/"

.getRefNetFileURIs <- function() { 
    # everything is embedded in the second line of xml
    .refNetbase.url  <- paste0(.amazonBaseUrl, "refnet/")
    filenames <- c("gerstein-2012.tsv_0.0.1.RData" ,
                "hypoxiaSignaling-2006.tsv_0.0.1.RData", 
                "stamlabTFs-2012.tsv_0.0.1.RData", 
                "recon202.tsv_0.0.1.RData")      
    paste0(.refNetbase.url, filenames)
} 


.refnetFiles <- function() {
    files <- .getRefNetFileURIs()
    df <- .httrFileInfo(files, verbose=FALSE)
    title <- basename(files)
    
    filename.stem <- sub(".tsv_0.0.1.RData", "", title)
    description <- sprintf("Interactions from %s", filename.stem)
    cbind(df, title,  description, stringsAsFactors=FALSE)
}


makeRefNetImporter <- function(currentMetadata, justRunUnitTest=FALSE,
                               BiocVersion=BiocManager::version()) {
    rsrc <- .refnetFiles()
    
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- gsub("_0.0.1.RData", "", rsrc$fileurl)
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    
    title <- rsrc$title
    description <- rsrc$description
    
    Tags <- lapply(rsrc$description, function(x) {
        c("refNet","interactions", x)
    })
    
    Map(AnnotationHubMetadata,
        
        SourceSize = sourceSize,
        SourceUrl = sourceUrls,
        SourceVersion = sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        
        Description = description,
        Title = title,
            
        RDataPath = gsub(.amazonBaseUrl, "",sourceUrls),
        
        MoreArgs=list(
            BiocVersion=BiocVersion,
            # input sources 
            SourceType = "RData",
            
            # resources
            Species = "Homo sapiens", 
            TaxonomyId = 9606L,
            Genome = "RefNet Genome",
            DataProvider = "RefNet",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .amazonBaseUrl,
            RDataDateAdded = Sys.time(),
            
            
            #rdata table
            DispatchClass = "data.frame" ,
            RDataClass = "data.frame",
            
            Tags = c("refnet", "interactions"),
            Recipe= NA_character_))
}

makeAnnotationHubResource("RefNetImportPreparer", makeRefNetImporter)
