nullRecipe <- function(recipe)
{
    inputFile <- inputFiles(recipe)[1]
    outputFile <- outputFile(recipe)
    file.copy(inputFile, outputFile)
    outputFile
}
