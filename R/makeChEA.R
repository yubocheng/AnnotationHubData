.mssmBaseUrl <- "http://amp.pharm.mssm.edu/"

makeChEAImporter <- function(currentMetadata, justRunUnitTest=FALSE,
                             BiocVersion=biocVersion()) {
    files <- "result/kea/chea-background.zip"

    files <- paste0(.mssmBaseUrl, files)
    rsrc <- .httrFileInfo(files, verbose=FALSE)
    title <- basename(rsrc$fileurl)
        
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    description <- .expandLine("ChEA background file, containing  
        transcription factor data to run ChEA") 
    
    Map(AnnotationHubMetadata,
        SourceSize = sourceSize,
        SourceUrl = sourceUrls,
        SourceVersion = sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        
        Description = description,
        Title = title,
        
        RDataPath = gsub(.mssmBaseUrl, "", sourceUrls),
        
        MoreArgs=list(
            BiocVersion=BiocVersion,
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
            DispatchClass = "ChEA",
            RDataClass = "data.frame",
            
            Tags = c("ChEA","Transcription Factors"),
            
            Recipe = NA_character_ ))
}

makeAnnotationHubResource("ChEAImportPreparer", makeChEAImporter)

