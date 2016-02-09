.pazarBaseUrl <- "http://www.pazar.info/"

## Note: These 3 files are present in the zip, but not on the website?
## "pazar_Foxa2_Robertson_20120522.csv" "pazar_jaspar_core_20120522.csv"    
## "pazar_Pax6_Aniridia_20120522.csv" 

.getPazarFiles <- function(justRunUnitTest) {
    
    url <- paste0(.pazarBaseUrl, "cgi-bin/downloads_csv.pl")
    result <- GET(url)
    stop_for_status(result)
    html <- content(result)
    filenames <- as.character(xml_find_all(html, "//table//tr/td/a/text()")) 
    sourceUrl <- as.character(
        xml_contents(xml_find_all(html, "//table//tr/td/a/@href")))
    sourceUrl <- grep(".csv$", sourceUrl, value=TRUE)
    if(!identical(basename(sourceUrl), filenames))
         stop("Inconsistent FileNames and sourceUrls")
    
    if(justRunUnitTest) {
	filenames  <- filenames[1:5]
	sourceUrl  <- sourceUrl[1:5]
    }
        
    ## get the fileSize and 
    df <- .httrFileInfo(sourceUrl, verbose=TRUE)
    title <- basename(df$fileurl)
    
    filename.stem <- sub(".csv", "", title)
    description <- sprintf("TF - Target Gene file from %s", filename.stem)
    
    cbind(df, title, description, sourceUrl, stringsAsFactors=FALSE)
}

makePazarImporter <- function(currentMetadata, justRunUnitTest=FALSE, 
                              BiocVersion=biocVersion()) {
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
            BiocVersion=BiocVersion,
            # input sources 
            SourceType = "CSV",
            
            # resources
            Species = NA_character_, 
            TaxonomyId = NA_integer_,
            Genome = NA_character_,
            DataProvider = "Pazar",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",         
            Coordinate_1_based = FALSE,
            Location_Prefix = .pazarBaseUrl,
            RDataDateAdded = Sys.time(),
            
            #rdata table
            DispatchClass = "Pazar" ,
            RDataClass = "GRanges",
            
            Tags = c("Pazar","Transcription Factors"),
            
            Recipe = NA_character_ ))
}

makeAnnotationHubResource("PazarImportPreparer", makePazarImporter)
