make2bit <- function(currentMetadata) {
    rsrc <- .getUCSCResources(fileType="2bit", dirName="bigZips", 
                              fileName=".2bit", verbose=FALSE)
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$url
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    species <- rsrc$organism   
    genome <- rsrc$from
    taxonomyId <- as.integer(rsrc$taxid)           
    title <- rownames(rsrc) 
    description <- sprintf("UCSC 2 bit file for %s ", rsrc$from)
    
    Map(AnnotationHubMetadata,
        
        SourceSize = sourceSize,
        SourceUrl = sourceUrls,
        SourceVersion = sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        
        Description = description,
        Title = title,
        Genome = genome,
        Species = species, 
        TaxonomyId = taxonomyId,
        
        RDataPath = sourceUrls,
        
        MoreArgs=list(
            # input sources 
            SourceType = "TowBitFile",
            
            # resources
            DataProvider = "UCSC",
            Maintainer =  "Sonali Arora <sarora@fredhutch.org>",         
            Coordinate_1_based = FALSE,
            status_id = 2L, 
            Location_Prefix = .ucscBase,
            RDataDateAdded = Sys.time(),
            PreparerClass = "UCSC2BitPreparer",
            
            #rdata table
            DispatchClass= "TwoBitFile" ,
            RDataClass = "TwoBitFile",
            
            Recipe = NA_character_, 
            Tags = c("2bit", "UCSC", "genome" )))
}

makeAnnotationHubResource("UCSC2BitPreparer", make2bit)
