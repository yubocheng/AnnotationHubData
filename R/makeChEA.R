.mssmBaseUrl <- "http://amp.pharm.mssm.edu/"

makeChEAImporter <- function(currentMetadata, justRunUnitTest=FALSE) {
    files <- "result/kea/chea-background.zip"

    files <- paste0(.mssmBaseUrl, files)
    rsrc <- AnnotationHubData:::.httrFileInfo(files, verbose=FALSE)
    title <- basename(rsrc$fileurl)
        
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    description <- .expandLine("ChEA database file -  Each line of the 
        background data file contains 3 entries. The first is a transcription
        factor and pubmed id, the second a reported transcription factor 
        target, third th pubmed id from the publication the interaction was 
        reported.")
    
    Map(AnnotationHubMetadata,
        SourceSize = sourceSize,
        SourceUrl = sourceUrls,
        SourceVersion = sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        
        Description = description,
        Title = title,
        
        RDataPath = gsub(.mssmBaseUrl, "", sourceUrls),
        
        MoreArgs=list(
            # input sources 
            SourceType = "Zip",
            
            # resources
            Species = NA_character_, 
            TaxonomyId = NA_integer_,
            Genome = NA_character_,
            DataProvider = "ChEA",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .mssmBaseUrl,
            RDataDateAdded = Sys.time(),
                        
            #rdata table
            DispatchClass = "zip" ,
            RDataClass = "data.frame",
            
            Tags = c("ChEA","Transcription Factors"),
            
            Recipe = NA_character_ ))
}

makeAnnotationHubResource("ChEAImportPreparer", makeChEAImporter)

