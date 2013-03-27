## -----------------------------------------------------------------------------
## Import UCSC track to GRanges
## 

trackToGRangesRecipe <- function(recipe)
{
    outputFile <- outputFile(recipe)
    if (!file.exists(outputFile)) {
        range <- .GRangesForUSCSGenome(genome)

        trackName <- basename(metadata(metadata(recipe))$SourceFile)
        genome <- metadata(metadata(recipe))$Genome
        query <- .ucscTableQuery(genome, trackName)
        gr <- track(query, asRangedData = FALSE)
        save(gr, file=outputFile)
    }
    outputFile
}
