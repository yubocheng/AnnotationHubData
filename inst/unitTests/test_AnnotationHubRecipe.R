library(AnnotationHubData)
library(RJSONIO)
library(RUnit)
#-------------------------------------------------------------------------------
runTests <- function()
{
    test_createWorkingDirectory()
    test_simpleConstructor()
    test_nullRecipe()
    test_adhocRecipe()

    test_constructSeqInfo()
    test_extendedBedFileRecipe()
    
    #test_temporaryMethods()
    #test_recipeWithSuppliedArguments()
    #test_recipeWithImpliedArguments()
    #test_bedFileRecipe ()
}
#-------------------------------------------------------------------------------
test_createWorkingDirectory <- function()
{
    print ("--- test_createWorkingDirectory")
    subdirectory <- 'goldenpath'
    sourceDirectory <- system.file('extdata', subdirectory,
                                    package='AnnotationHubData')
    
    originalFiles <- sort(list.files(sourceDirectory, recursive=TRUE))
  
       # recursive list.files ends up at the bottom of the extdata/goldenpath/hg19/...
       # path, returning only the 3 (or more) files found there
       # PKG-ROOT/extdata/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/
    checkTrue(length(originalFiles) >= 3)
    
    newDirectory <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    newDirectoryComplete <- file.path(newDirectory, 'goldenpath')
    movedFiles <- sort(list.files(newDirectoryComplete, recursive=TRUE))
    checkEquals(originalFiles, movedFiles)

} # test_createWorkingDirectory
#-------------------------------------------------------------------------------
test_simpleConstructor <- function()
{
    print ("--- test_simpleConstructor")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructAnnotationHubMetadataFromJsonPath(annotationHubRoot, jsonPath)

    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "extendedBedToGRanges")
    checkEquals(recipeName(recipe), "extendedBedToGRanges")
    checkEquals(annotationHubRoot(recipe), md@AnnotationHubRoot)
    checkTrue(file.exists(inputFiles(recipe)))

       # the output file has the same path and name as the 'main' (and often only) input file, with '.RData' added to it
       # remove that suffix, then compare it to the full path to that input file, aka 'ResourcePath'
    checkEquals(file.path(md@AnnotationHubRoot, md@ResourcePath), outputFile(recipe))
    TRUE

} # test_simpleConstructor
#-------------------------------------------------------------------------------
# the 'nullRecipe' simply copies the specified input file to the specified output file
# with no transformations on the contents.
test_nullRecipe <- function()
{
    print ("--- test_nullRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructAnnotationHubMetadataFromJsonPath(annotationHubRoot, jsonPath)
    md@Recipe <- "nullRecipe"
    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "nullRecipe")
    checkEquals(recipeName(recipe), "nullRecipe")
    checkTrue(file.exists(inputFiles(recipe)[1]))
    checkTrue(!file.exists(outputFile(recipe)))
    run(recipe)
    runWild(recipe)
    checkTrue(file.exists(outputFile(recipe)))

} # test_nullRecipe
#-------------------------------------------------------------------------------
# the 'nullRecipe' simply copies the specified input file to the specified output file
# with no transformations on the contents.
test_adhocRecipe <- function()
{
    print ("--- test_adhocRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructAnnotationHubMetadataFromJsonPath(annotationHubRoot, jsonPath)
    adhoc <- function(recipe) {
        nchar(inputFiles(recipe)[1])
        }
    
    md@Recipe <- "adhoc"
    
    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "adhoc")
    checkEquals(recipeName(recipe), "adhoc")
    checkEquals(runWild(recipe, adhoc), 154)

} # test_nullRecipe
#-------------------------------------------------------------------------------
test_constructSeqInfo <- function()
{
    species <- "Homo sapiens"
    genome <- "hg19"
  
    si.hg19 <- constructSeqInfo(species, genome)
    expected.names <- paste("chr", c(1:22,"X","Y","M"), sep="")
    checkEquals(names(si.hg19), expected.names)
    checkEquals(unique(genome(si.hg19)), genome)
    min.max <- fivenum(as.integer(seqlengths(si.hg19)))[c(1,5)]
    checkTrue(min.max[1] < 17000)     # chrM
    checkTrue(min.max[2] > 24000000)  # chr1
  
    genome <- "hg18"
    si.hg18 <- constructSeqInfo(species, genome)

        # chr17 grew in hg19
    checkTrue(seqlengths(si.hg18['chr17']) < seqlengths(si.hg19['chr17']))

} # test_constructSeqInfo
#-------------------------------------------------------------------------------
test_extendedBedFileRecipe <- function ()
{
    print ("--- test_extendedBedFileRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage"
    jsonPath <- file.path(resourcePath, jsonFile)

        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")
    workingDirectory <- AnnotationHubData:::createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # call a metadata file reader/object constructor
    md <- constructAnnotationHubMetadataFromJsonPath(annotationHubRoot, jsonPath)
    recipe <- AnnotationHubRecipe(md)

    checkEquals(recipeName(recipe), "extendedBedToGRanges")

        # create GRanges from the extended bed file, save as RData where
        # instructed by the recipe
    pathToRDataFile <- run(recipe)

        # check the result
    load(pathToRDataFile)

    checkEquals(length(gr), 50)
    checkEquals(names(mcols(gr)), c("level", "significance"))
    checkEquals(start(gr[9]), 54704676)
    checkEquals(end(gr[9]),   54704735)
    checkEquals(width(gr[9]), 60)

} # test_extendedBedFileRecipe
#-------------------------------------------------------------------------------
#test_recipeWithImpliedArguments <- function()
#{
#    print ("--- test_recipeWithImpliedArguments")
#    x <- AnnotationHubRecipe()
#    x <- setRecipeName(x,"bedFileRecipe")
#    x <- setInputFiles(x,"wgEncodeRikenCageCd20CellPapTssHmm.top50.bedRnaElements")
#    x <- setOutputDirectory(x,"/tmp")
#    run(x)
#    
#}
##-------------------------------------------------------------------------------
#test_recipeWithSuppliedArguments <- function()
#{
#    print ("--- test_recipeWithSuppliedArguments")
#    x <- AnnotationHubRecipe()
#    func <- function(filename) {system(sprintf("wc -l "%s"", filename))}
#    run(x, "func", "jabberwocky.txt")
#    
#}
##-------------------------------------------------------------------------------
#test_bedFileRecipe <- function()
#{
#    print ("--- test_bedFileRecipe")
#    x <- AnnotationHubRecipe()
#    x <- setRecipeName(x, "bedFileRecipe")
#    filename <- system.file("extdata",
#                            "wgEncodeRikenCageCd20CellPapTssHmm.top50.bedRnaElements.gz",
#                             package="AnnotationHubData")
#    x <- setInputFiles(x, filename)
#    x <- setOutputDirectory(x, "/tmp")
#    run(x)
#}
##-------------------------------------------------------------------------------
#test_temporaryMethods <- function()
#{
#  print ("--- test_temporaryMethods")
#  x <- AnnotationHubRecipe()
#
#  x <- setRecipeName(x, "foo")
#  checkEquals(getRecipeName(x), "foo")
#
#  x <- setInputFiles(x, c("foo", "bar"))
#  checkEquals(getInputFiles(x), c("foo", "bar"))
#
#  x <- setOutputDirectory(x,"/tmp")
#  checkEquals(getOutputDirectory(x),"/tmp")
#  
#}
##-------------------------------------------------------------------------------
#runRec <- function (functionName=NA, arg=NA) {
#  cmd <- sprintf("%s()", functionName)
#  printf("cmd: %s", cmd)
#  eval(parse(text=cmd))
#  }
#
##-------------------------------------------------------------------------------
#runRec1 <- function (functionName=NA, inputFileName=NA) {
#  if(is.na(functionName))
#    cmd <- sprintf("system ("date")")
#  else {
#    if(is.na(inputFileName))
#      cmd <- sprintf ("%s()", functionName)
#    else
#      cmd <- sprintf ("%s("%s")", functionName, inputFileName)
#    }
#  printf("cmd: %s", cmd)
#  eval(parse(text=cmd))
#  }
##-------------------------------------------------------------------------------
#
#func <- function(inputFileName=NA)
#{
#  if(!is.na(inputFileName)) {
#    if(file.exists(inputFileName))
#       printf("word count in %s: %d", inputFileName, 
#          length(scan(inputFileName, what=character(), quiet=TRUE)))
#    }
#  else
#      printf("no inputFileName, calling date from within func: %s", date())
#}
##-------------------------------------------------------------------------------
