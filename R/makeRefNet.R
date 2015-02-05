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
    sourceUrl <- sub("http://s3.amazonaws.com/" , "", df$fileurl)
        
    cbind(df, title,  description, sourceUrl, stringsAsFactors=FALSE)
}


makeRefNetImporter <- function(currentMetadata) {
    rsrc <- .refnetFiles()
    
    description <- rsrc$description
    sourceFile <- rsrc$title
    title <- rsrc$title
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- rsrc$description
    
    Map(AnnotationHubMetadata,
        Description=description, 
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion,
        Title=title, Tags=Tags,
        MoreArgs=list(
            Genome= "RefNet Genome",
            Species="Homo sapiens",
            TaxonomyId=9606L,
            Coordinate_1_based = FALSE,
            DataProvider = "RefNet",
            Location_Prefix = .refNetbase.url,
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",
            RDataClass = "data.frame", 
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.2",
            Recipe = NA_character_))
}

makeAnnotationHubResource("RefNetImportPreparer", makeRefNetImporter)