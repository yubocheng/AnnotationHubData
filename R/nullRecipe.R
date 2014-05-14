nullRecipe <- function(ahm)
{
    inputFile <- inputFiles(ahm)[1]
    outputFile <- outputFile(ahm)
    file.copy(inputFile, outputFile)
    outputFile
}
