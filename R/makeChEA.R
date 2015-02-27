.amazonBaseUrl <- "http://s3.amazonaws.com/annotationhub/"

makeChEAImporter <- function(currentMetadata) {
    files <- "chea/chea-background.zip"
    files <- paste0(.amazonBaseUrl, files)
    rsrc <- .httrFileInfo(files, verbose=FALSE)
    title <- basename(rsrc$fileurl)
        
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    description <- "ChEA database file -  Each line of the background data file contains 3 entries. The first is a transcription factor and pubmed id, the second a reported transcription factor target, third th pubmed id from the publication the interaction was reported."
    
    Map(AnnotationHubMetadata,
        SourceSize = sourceSize,
        SourceUrl = sourceUrls,
        SourceVersion = sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        
        Description = description,
        Title = title,
        
        RDataPath = gsub(.amazonBaseUrl, "", sourceUrls),
        
        MoreArgs=list(
            # input sources 
            SourceType = "CSV file",
            
            # resources
            Species = "Homo sapiens", 
            TaxonomyId = 9606L,
            Genome = "hg19",
            DataProvider = "ChEA",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .amazonBaseUrl,
            RDataDateAdded = Sys.time(),
                        
            #rdata table
            DispatchClass = "data.frame" ,
            RDataClass = "data.frame",
            
            Tags = c("ChEA","Transcription Factors"),
            
            Recipe = NA_character_ ))
}

makeAnnotationHubResource("ChEAImportPreparer", makeChEAImporter)

