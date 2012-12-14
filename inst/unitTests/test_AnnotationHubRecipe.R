library(AnnotationHubData)
library(RJSONIO)
library(RUnit)
#-------------------------------------------------------------------------------
runTests <- function()
{
    test_.createWorkingDirectory()
    test_simpleConstructor()
    test_nullRecipe()
    test_adhocRecipe()

    test_constructSeqInfo()

    test_extendedBedFileRecipe()
    #test_extendedBedWithAuxiliaryTableRecipe ()

} # runTests
#-------------------------------------------------------------------------------
test_.createWorkingDirectory <- function()
{
    print ("--- test_.createWorkingDirectory")
    sourceDirectory <- system.file('extdata',
                                    package='AnnotationHubData')
    
    originalFiles <- sort(list.files(sourceDirectory, recursive=TRUE))
  
       # recursive list.files ends up at the bottom of the extdata/goldenpath/hg19/...
       # path, returning only the 3 (or more) files found there
       # PKG-ROOT/extdata/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/
    checkTrue(length(originalFiles) >= 3)
    
    newDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    movedFiles <- sort(list.files(newDirectory, recursive=TRUE))
    checkEquals(originalFiles, movedFiles)

} # test_.createWorkingDirectory
#-------------------------------------------------------------------------------
test_simpleConstructor <- function()
{
    print ("--- test_simpleConstructor")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <-
        AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)

    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "extendedBedToGRanges")
    checkEquals(recipeName(recipe), "extendedBedToGRanges")
    checkEquals(annotationHubRoot(recipe), md@AnnotationHubRoot)
    checkTrue(file.exists(inputFiles(recipe)))

       # the output file has the same path and name as the 'main' (and often only)
       # input file, with '.RData' added to it remove that suffix, then compare it
       # to the full path to that input file, aka 'ResourcePath'
    checkEquals(file.path(md@AnnotationHubRoot, md@ResourcePath),
                outputFile(recipe))
    TRUE

} # test_simpleConstructor
#-------------------------------------------------------------------------------
# the 'nullRecipe' simply copies the specified input file to the specified
# output file with no transformations on the contents.
test_nullRecipe <- function()
{
    print ("--- test_nullRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    md@Recipe <- "nullRecipe"
    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "nullRecipe")
    checkEquals(recipeName(recipe), "nullRecipe")
    checkTrue(file.exists(inputFiles(recipe)[1]))
    run(recipe)
    checkTrue(file.exists(outputFile(recipe)))
    runWild(recipe)
    checkTrue(file.exists(outputFile(recipe)))

} # test_nullRecipe
#-------------------------------------------------------------------------------
# demonstrate and test the use of a locally defined function, one which does
# no transformation of data, just reports the number of characters in the
# name of the recipe's input data file
test_adhocRecipe <- function()
{
    print ("--- test_adhocRecipe")

    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
    jsonPath <- file.path(resourcePath, jsonFile)
    
    sourceDirectory <- system.file('extdata', package='AnnotationHubData')
    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

    md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    adhoc <- function(recipe) {
        nchar(inputFiles(recipe)[1])
        }
    
    md@Recipe <- "adhoc"
    
    recipe <- AnnotationHubRecipe(md)
    checkTrue(validObject(recipe))
    checkEquals(md@Recipe, "adhoc")
    checkEquals(recipeName(recipe), "adhoc")

       # adhoc  does character count on the input file specified
       # in the json file.  will change on each run due to the
       # random tmp directory name.  check for identical counts
    checkEquals(runWild(recipe, adhoc), nchar(inputFiles(recipe)[1]))

} # test_adhocRecipe
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
test_extendedBedFileRecipe <- function()
{
    print ("--- test_extendedBedFileRecipe")

        # copy the source data to a writable temporary directory

    sourceDirectory <- system.file("extdata", package="AnnotationHubData")


    workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage"
    jsonPath <- file.path(resourcePath, jsonFile)

    checkTrue(file.exists(file.path(annotationHubRoot, jsonPath)))

        # create a metadata object from this file
    md <- constructMetadataFromJsonPath(annotationHubRoot,
                                                     jsonPath)

        # now create a Recipe instance
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
dev.extendedBedWithAuxiliaryTable <- function(recipe)
{
     bedFile <- grep(".bed.gz$", inputFiles(recipe), value=TRUE)
     auxFile <- grep(".tab$", inputFiles(recipe), value=TRUE)
     stopifnot(length(bedFile) == 1)
     stopifnot(length(auxFile) == 1)

     colClasses <- recipe@metadata@RecipeArgs$bedColClasses
     tbl.bed <- read.table(gzfile(bedFile), sep="\t", header=FALSE,
                           colClasses=colClasses)
     colnames(tbl.bed) <- names(colClasses)
     
     colClasses <- recipe@metadata@RecipeArgs$auxColClasses
     tbl.aux <- read.table(auxFile, sep="\t", colClasses=colClasses)
     colnames(tbl.aux) <- names(colClasses)

     mergeArgs <- recipe@metadata@RecipeArgs$merge
     browser()

        # TODO:  special knowledge inserted here, adding a column
        # TODO:  to tbl.aux (rowIndex) so that tables can be linked
        # TODO:  future data sources using otherwise identical
        # TODO:  treatment may suggest more general approach.
     
     tbl.aux <- cbind(tbl.aux, rowIndex=1:nrow(tbl.aux))

     tbl <- merge(tbl.bed, tbl.aux, by.x=mergeArgs[["byX"]],
                                    by.y=mergeArgs[["byY"]],
                                    all.x=TRUE)
     colnames <- colnames(tbl)
     requiredColnames <- c("seqnames", "start", "end")
     stopifnot(all(requiredColnames %in% colnames))
     otherColnames <- setdiff(colnames, requiredColnames)

     gr <- with(tbl, GRanges(seqnames, IRanges(start, end)))
     mcols(gr) <- DataFrame(tbl[, otherColnames])

        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(recipe@metadata@Species, recipe@metadata@Genome) 
        # if gr only has a subset of all possible chromosomes, then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(recipe))

    outputFile(recipe)

} # dev.extendedBedWithAuxiliaryTable 
#-------------------------------------------------------------------------------
test_extendedBedWithAuxiliaryTableRecipe <- function()
{
    print ("--- test_extendedBedWithAuxiliaryTableRecipe")

        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")
    workingDirectory <-
        AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <-
       "wgEncodeRegDnaseClustered.bed-wgEncodeRegDnaseClusteredInputs.tab.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    jsonPath <- file.path(resourcePath, jsonFile)

        # create a metadata object from this file
    md <-
      constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    browser()
    checkEquals(length(gr), 100)
    checkEquals(dim(mcols(gr)), c(100,8))
    checkEquals(colnames(mcols(gr)), c("name", "score", "track", "cellType",
                                       "treatment", "replicate", "source", "date"))
      # hand-check one range
    x <- gr[start(gr)==95923360]
    checkEquals(end(x), 95924150)
    checkEquals(as.character(seqnames(x)), "chr13")
    x.md <- as.list(mcols(x))
    #checkEquals(x.md,

    print ("--- test_extendedBedWithAuxiliaryTableRecipe")

        # copy the source data to a writable temporary directory
    sourceDirectory <- system.file("extdata", package="AnnotationHubData")
    workingDirectory <-
        AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
    annotationHubRoot <- workingDirectory

        # locate the json metadata file
    jsonFile <-
       "wgEncodeRegDnaseClustered.bed-wgEncodeRegDnaseClusteredInputs.tab.json"
    resourcePath <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    jsonPath <- file.path(resourcePath, jsonFile)

        # create a metadata object from this file
    md <-
      constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
    browser()
    x <- runWild(recipe)
    browser()

} # test_extendedBedWithAuxiliaryTableRecipe
#-------------------------------------------------------------------------------
