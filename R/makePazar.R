.pazarBaseUrl <- "http://www.pazar.info/"

## Note: These 3 files are present in the zip, but not on the website?
## "pazar_Foxa2_Robertson_20120522.csv" "pazar_jaspar_core_20120522.csv"    
## "pazar_Pax6_Aniridia_20120522.csv" 

.getPazarFiles <- function(justRunUnitTest) {
    
    ## read the filenames from the url 
    theurl <- paste0(.pazarBaseUrl, "cgi-bin/downloads_csv.pl")
    result <- GET(theurl)
    stop_for_status(result)
    html <- content(result)
    tab <- sapply(html["//table"], xmlValue)
    files <- strsplit(tab, "\n\t\t\t")[[1]]
    filenames <- grep(".csv", files, value=TRUE)
    
    if(justRunUnitTest)
	filenames  <- filenames[1:5]
        
    ## get the fileSize and 
    actualUrl <- paste0( .pazarBaseUrl, "tftargets/")
    sourceUrl <- paste0(actualUrl, filenames)
    
    df <- .httrFileInfo(sourceUrl, verbose=TRUE)
    title <- basename(df$fileurl)
    
    filename.stem <- sub(".csv", "", title)
    description <- sprintf("TF - Target Gene file from %s", filename.stem)
    
    tags <- sapply(filename.stem, function(x) {
        paste0("Pazar", "TF-Target Gene file", x)
    })
    
    cbind(df, title, description, tags, sourceUrl, stringsAsFactors=FALSE)
}

makePazarImporter <- function(currentMetadata, justRunUnitTest=FALSE) {
    rsrc <- .getPazarFiles(justRunUnitTest)
    
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    
    Map(AnnotationHubMetadata,
        SourceSize = sourceSize,
        SourceUrl = sourceUrls,
        SourceVersion = sourceVersion,
        SourceLastModifiedDate = sourceLastModifiedDate,
        
        Description = description,
        Title = title,
        
        RDataPath = gsub(.pazarBaseUrl, "", sourceUrls),
        
        MoreArgs=list(
            # input sources 
            SourceType = "CSV",
            
            # resources
            Species = NA_character_, 
            TaxonomyId = NA_integer_,
            Genome = NA_character_,
            DataProvider = "Pazar",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .pazarBaseUrl,
            RDataDateAdded = Sys.time(),
            
            #rdata table
            DispatchClass = "data.frame" ,
            RDataClass = "data.frame",
            
            Tags = c("ChEA","Transcription Factors"),
            
            Recipe = NA_character_ ))
}

makeAnnotationHubResource("PazarImportPreparer", makePazarImporter)
