## -----------------------------------------------------------------------------
## Import UCSC track to GRanges
## 

trackToGRangesRecipe <- function(recipe)
{
    outputFile <- outputFile(recipe)
    if (!file.exists(outputFile)) {
        trackName <- basename(metadata(metadata(recipe))$SourceFile)
        session <- browserSession()
        genome(session) <- metadata(metadata(recipe))$Genome
        query <- ucscTableQuery(session, trackName)
        gr <- track(query, asRangedData = FALSE)
        save(gr, file=outputFile)
        if (!getOption("AnnotationHub_Use_Disk", FALSE)) {
            upload_to_S3(outputFile, metadata(recipe)@RDataPath)
        }
    }
    outputFile
}
