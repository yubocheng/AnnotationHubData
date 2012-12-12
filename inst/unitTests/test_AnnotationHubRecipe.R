library(AnnotationHubData)
library(RUnit)
#-------------------------------------------------------------------------------
runTests <- function()
{
    test_createWorkingDirectory()
    test_simpleConstructor()
    #test_temporaryMethods()
    #test_recipeWithSuppliedArguments()
    #test_recipeWithImpliedArguments()
    #test_bedFileRecipe ()
}
#-------------------------------------------------------------------------------
# in these unit tests we want to actually create data resources, just as they
# are in the full running version of the hub.  this requires a writable directory:
# we don't want to try to write into the extdata directory of an installed package!
# so we need a directory which can be reliably created, written to, and checked, on any computer
# these tests will run on.  
# a recursive copy of the (possibly deeply-nested) source directory (below pkg/extdata)
# into a temporary and necessarily writable directory provides the solution
createWorkingDirectory <- function(sourceDirectory)
{
    newDirectory <- tempdir()

    suppressWarnings(  # .svn directories do not copy
        file.copy(sourceDirectory, newDirectory, recursive=TRUE)
        )

    newDirectory

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
  
  newDirectory <- createWorkingDirectory(sourceDirectory)
  newDirectoryComplete <- file.path(newDirectory, 'goldenpath')
  movedFiles <- sort(list.files(newDirectoryComplete, recursive=TRUE))
  checkEquals(originalFiles, movedFiles)

}
#-------------------------------------------------------------------------------
test_simpleConstructor <- function()
{
    print ("--- test_simpleConstructor")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructAnnotationHubMetadataFromJsonPath(annotationHubRoot, jsonPath)

    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "extendedBedToGranges")
    checkEquals(recipeName(recipe), "extendedBedToGranges")
    checkEquals(annotationHubRoot(recipe), md@AnnotationHubRoot)
    checkEquals(inputFiles(recipe), "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements")

       # the output file has the same path and name as the 'main' (and often only) input file, with '.RData' added to it
       # remove that suffix, then compare it to the full path to that input file, aka 'ResourcePath'
    checkEquals(file.path(md@AnnotationHubRoot, md@ResourcePath), outputFile(recipe))
                  
    bedFile <- file.path(annotationHubRoot(recipe), inputFiles(recipe))
    checkTrue(file.exists(bedFile))

}
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
#    func <- function(filename) {system(sprintf("wc -l '%s'", filename))}
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
#    cmd <- sprintf("system ('date')")
#  else {
#    if(is.na(inputFileName))
#      cmd <- sprintf ("%s()", functionName)
#    else
#      cmd <- sprintf ("%s('%s')", functionName, inputFileName)
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
