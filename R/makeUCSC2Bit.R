make2bit <- function(currentMetadata, justRunUnitTest=FALSE, 
                     BiocVersion=BiocManager::version()) {
    rsrc <- .getUCSCResources(fileType="2bit", dirName="bigZips", 
                              fileName=".2bit", verbose=TRUE, justRunUnitTest)
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    rdatapaths <- gsub(.ucscBase, "",sourceUrls)
    md5sum <- rsrc$md5sum
    
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
        SourceMd5 =md5sum,
        
        Description = description,
        Title = title,
        Genome = genome,
        Species = species, 
        TaxonomyId = taxonomyId,
        
        RDataPath = rdatapaths,
        
        MoreArgs=list(
            BiocVersion=BiocVersion,
            # input sources 
            SourceType = "TwoBit",
            
            # resources
            DataProvider = "UCSC",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .ucscBase,
            RDataDateAdded = Sys.time(),
                        
            #rdata table
            DispatchClass= "TwoBitFile" ,
            RDataClass = "TwoBitFile",
            
            Recipe = NA_character_, 
            Tags = c("2bit", "UCSC", "genome" )))
}

makeAnnotationHubResource("UCSC2BitPreparer", make2bit)
