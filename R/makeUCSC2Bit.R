make2bit <- function(currentMetadata) {
    rsrc <- .getUCSCResources(fileType="2bit", dirName="bigZips", 
        fileName=".2bit", verbose=FALSE)
    description <- sprintf("UCSC 2 bit file for %s ", rsrc$from)
    genome <- rsrc$from
    sourceFile <- rownames(rsrc)
    sourceUrls <- sub(.ucscBase, "", rsrc$url)
    sourceVersion <- rsrc$version
    species <- rsrc$organism            
    taxonomyId <- as.integer(rsrc$taxid)           
    title <- rownames(rsrc)
    SourceLastModifiedDate <- as.POSIXct(strptime(rsrc$version, 
        "%d-%b-%Y_%H:%M"))
    SourceSize <- as.numeric(rsrc$newSize)
    Map(AnnotationHubMetadata,
        Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceFile,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title,
        MoreArgs=list(
            Coordinate_1_based = FALSE,
            DataProvider = "hgdownload.cse.ucsc.edu",
            Location_Prefix = .ucscBase,
            Maintainer = "Sonali Arora <sarora@fhcrc.org>",
            RDataClass = "TwoBitFile",
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.1",
            Recipe = NA_character_,
            Tags = c("2bit", "UCSC", "genome" )))
}

makeAnnotationHubResource("UCSC2BitPreparer", make2bit)