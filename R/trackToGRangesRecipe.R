## -----------------------------------------------------------------------------
## Import UCSC track to GRanges
## 

trackToGRangesRecipe <- function(ahm)
{
    outputFile <- outputFile(ahm)
    if (!file.exists(outputFile)) {
        trackName <- basename(metadata(ahm)$SourceFile)
        session <- browserSession()
        genome(session) <- metadata(ahm)$Genome
        query <- ucscTableQuery(session, trackName)
        gr <- track(query, asRangedData = FALSE)
        save(gr, file=outputFile)
        if (!getOption("AnnotationHub_Use_Disk", FALSE)) {
            upload_to_S3(outputFile, metadata(ahm)$RDataPath)
        }
    }
    outputFile
}
