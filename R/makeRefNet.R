## old file - RefNetImportPreparer-class.R 
##changes - title should have full file name, species is "Homo sapiens" not 9606
## what is RefNet Genome?
## tags looks like "interactions, interactions from gerstein-2012"

.refNetbase.url <- "http://s3.amazonaws.com/refnet-networks/"

.getRefNetFileURIs <- function() { 
    # everything is embedded in the second line of xml
    raw.text <- scan(.refNetbase.url, what=character(0), sep="\n", quiet=TRUE)[2]
    raw.tokens <- strsplit(raw.text, "<")[[1]]
    filenames.raw <- raw.tokens[grep("^Key>", raw.tokens)]
    stopifnot(length(filenames.raw) > 0)
    filenames <- sub("Key>", "", filenames.raw)
    paste0(.refNetbase.url, filenames)
} 


.refnetFiles <- function() {
    files <- .getRefNetFileURIs()
    df <- .httrFileInfo(files, verbose=FALSE)
    title <- basename(files)
    
    filename.stem <- sub(".tsv", "", title)
    description <- sprintf("interactions from %s", filename.stem)
    cbind(df, title,  description, stringsAsFactors=FALSE)
}


makeRefNetImporter <- function(currentMetadata) {
    rsrc <- .refnetFiles()
    
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
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
            
        RDataPath = sourceUrls,
        
        MoreArgs=list(
            # input sources 
            SourceType = "CSV File",
            
            # resources
            Species = "species", 
            TaxonomyId = "taxonomyId",
            Genome = "RefNet Genome",
            DataProvider = "RefNet",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",         
            Coordinate_1_based = FALSE,
            status_id = 2L, 
            Location_Prefix = .refNetbase.url,
            RDataDateAdded = Sys.time(),
            PreparerClass = "RefNetImportPreparer",
            
            #rdata table
            DispatchClass = "data.frame" ,
            RDataClass = "data.frame",
            
            Tags = Tags,
            
            Recipe = NA_character_ ))
}

makeAnnotationHubResource("RefNetImportPreparer", makeRefNetImporter)
