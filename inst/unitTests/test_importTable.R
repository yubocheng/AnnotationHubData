#-------------------------------------------------------------------------------
paulsTests <- function()
{
   library(AnnotationHubData)
   library(RUnit)
   
   test_csv()
   test_tsv()
   test_colClassesNA()
   test_noHeader()
   test_haemCodeTableSample()
   
} # paulsTests
#-------------------------------------------------------------------------------
test_csv <- function()
{
    print("--- test_csv")

    sourceDirectory <- system.file(package="AnnotationHubData", "extdata",
                                   "importTableSamples")

       # copy the input files to a temp dir; same directory will be used
       # to write the serialized version, which is done in the recipe
       # and which, by loading it, we can check for the success of the recipe
    
    rootForTesting <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

    md <- AnnotationHubMetadata(
              AnnotationHubRoot=rootForTesting,
              SourceFile="test0.csv",
              SourceUrl="http://bogus.com",
              SourceVersion=NA_character_,
              Title="test importTable.R",
              Description="",
              Species="Homo sapiens",
              Genome="hg19",
              Recipe="importTable",
              RecipeArgs=list(header=TRUE, sep=",",
                              colClasses=c("numeric", "numeric", "character")),
              Tags="no tags",
              RDataClass="data.frame",
              RDataVersion=numeric_version("0.0.1"),
              Coordinate_1_based=TRUE,
              Maintainer="Paul Shannon <pshannon@fhcrc.org>",
              DataProvider="nowhere",
              Notes="no notes",
              RDataDateAdded=as.POSIXct("2013-01-01", tz="GMT"))

    recipe <- AnnotationHubRecipe(md)
    updated.metadata <- run(recipe)
    browser()
    checkTrue(file.exists(outputFile(recipe)))
    load(outputFile(recipe))
    checkEquals(dim(tbl), c(2,3))
    checkEquals(colnames(tbl), c("col1", "col2", "col3"))
    checkEquals(as.character(lapply(tbl, class), use.names=FALSE),
                c("numeric", "numeric", "character"))
    checkEquals(tbl[2,2], 45)

} # test_csv
#-------------------------------------------------------------------------------
test_tsv <- function()
{
    print("--- test_tsv")

    sourceDirectory <- system.file(package="AnnotationHubData", "extdata",
                                   "importTableSamples")
       # copy the input files to a temp dir; same directory will be used
       # to write the serialized version, which is done in the recipe
       # and which, by loading it, we can check for the success of the recipe

    rootForTesting <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

    md <- AnnotationHubMetadata(
              AnnotationHubRoot=rootForTesting,
              SourceFile="test1.tsv",
              SourceUrl="http://bogus.com",
              SourceVersion=NA_character_,
              Title="test importTable.R",
              Description="",
              Species="Homo sapiens",
              Genome="hg19",
              Recipe="importTable",
              RecipeArgs=list(header=TRUE, sep="\t",
                              colClasses=c("numeric", "numeric", "character")),
              Tags="no tags",
              RDataClass="data.frame",
              RDataVersion=numeric_version("0.0.1"),
              Coordinate_1_based=TRUE,
              Maintainer="Paul Shannon <pshannon@fhcrc.org>",
              DataProvider="nowhere",
              Notes="no notes",
              RDataDateAdded=as.POSIXct("2013-01-01", tz="GMT"))

    recipe <- AnnotationHubRecipe(md)
    updated.metadata <- run(recipe)
    checkTrue(file.exists(outputFile(recipe)))
    load(outputFile(recipe))
    checkEquals(dim(tbl), c(2,3))
    checkEquals(as.character(lapply(tbl, class), use.names=FALSE),
                c("numeric", "numeric", "character"))
    checkEquals(tbl[2,2], 45)

} # test_tsv
#-------------------------------------------------------------------------------
test_colClassesNA <- function()
{
    print("--- test_colClassesNA")

    sourceDirectory <- system.file(package="AnnotationHubData", "extdata",
                                   "importTableSamples")
       # copy the input files to a temp dir; same directory will be used
       # to write the serialized version, which is done in the recipe
       # and which, by loading it, we can check for the success of the recipe

    rootForTesting <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

    md <- AnnotationHubMetadata(
              AnnotationHubRoot=rootForTesting,
              SourceFile="test1.tsv",
              SourceUrl="http://bogus.com",
              SourceVersion=NA_character_,
              Title="test importTable.R",
              Description="",
              Species="Homo sapiens",
              Genome="hg19",
              Recipe="importTable",
              RecipeArgs=list(header=TRUE, sep="\t",
                              colClasses=NA),
              Tags="no tags",
              RDataClass="data.frame",
              RDataVersion=numeric_version("0.0.1"),
              Coordinate_1_based=TRUE,
              Maintainer="Paul Shannon <pshannon@fhcrc.org>",
              DataProvider="nowhere",
              Notes="no notes",
              RDataDateAdded=as.POSIXct("2013-01-01", tz="GMT"))

    recipe <- AnnotationHubRecipe(md)
    updated.metadata <- run(recipe)
    checkTrue(file.exists(outputFile(recipe)))
    load(outputFile(recipe))
    checkEquals(dim(tbl), c(2,3))
    checkEquals(as.character(lapply(tbl, class)),
                c("integer", "integer", "factor"))
    checkEquals(tbl[2,2], 45)

} # test_colClassesNA
#-------------------------------------------------------------------------------
test_noHeader <- function()
{
    print("--- test_noHeader")

    sourceDirectory <- system.file(package="AnnotationHubData", "extdata",
                                   "importTableSamples")

       # copy the input files to a temp dir; same directory will be used
       # to write the serialized version, which is done in the recipe
       # and which, by loading it, we can check for the success of the recipe

    rootForTesting <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

    md <- AnnotationHubMetadata(
              AnnotationHubRoot=rootForTesting,
              SourceFile="noHeader.tsv",
              SourceUrl="http://bogus.com",
              SourceVersion=NA_character_,
              Title="test importTable.R",
              Description="",
              Species="Homo sapiens",
              Genome="hg19",
              Recipe="importTable",
              RecipeArgs=list(header=FALSE, sep="\t",
                              colClasses=c("integer", "numeric", "numeric")),
              Tags="no tags",
              RDataClass="data.frame",
              RDataVersion=numeric_version("0.0.1"),
              Coordinate_1_based=TRUE,
              Maintainer="Paul Shannon <pshannon@fhcrc.org>",
              DataProvider="nowhere",
              Notes="no notes",
              RDataDateAdded=as.POSIXct("2013-01-01", tz="GMT"))

    recipe <- AnnotationHubRecipe(md)
    updated.metadata <- run(recipe)
    checkTrue(file.exists(outputFile(recipe)))
    load(outputFile(recipe))
    checkEquals(dim(tbl), c(3,3))
    checkEquals(as.character(lapply(tbl, class), use.names=FALSE),
                c("integer", "numeric", "numeric"))
    checkEquals(tbl[2,2], 45)

} # test_noHeader
#-------------------------------------------------------------------------------
test_haemCodeTableSample <- function()
{
    print("--- test_haemCodeTableSample")

    sourceDirectory <- system.file(package="AnnotationHubData", "extdata",
                                   "importTableSamples")

       # copy the input files to a temp dir; same directory will be used
       # to write the serialized version, which is done in the recipe
       # and which, by loading it, we can check for the success of the recipe

    rootForTesting <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)

       # columns in the standard HaemCode geneList csv files
       #
       # "factor"
       # "gene_symbol"
       # "entrezgene"
       # "coord"
       # "experiment"
       # "chr"

       # "start"
       # "end"
       # "width"

       # "strand"
       # "feature"

       # "feature_start_position"
       # "feature_end_position"

       # "insideFeature"

       # "distancetoFeature"
       # "shortestDistance"

       # "fromOverlappingOrNearest"

    col.classes <- c(rep("character", 6),
                     rep("integer",   3),
                     rep("character", 2),
                     rep("integer",   2),
                     rep("character", 1),
                     rep("integer",   2),
                     rep("character", 1))
                     
                     
    md <- AnnotationHubMetadata(
              AnnotationHubRoot=rootForTesting,
              SourceFile="haemCodeSample-10.lines.csv",
              SourceUrl="http://bogus.com",
              SourceVersion=NA_character_,
              Title="test importTable.R",
              Description="",
              Species="Homo sapiens",
              Genome="hg19",
              Recipe="importTable",
              RecipeArgs=list(header=TRUE, sep=",",
                              colClasses=col.classes),
              Tags="no tags",
              RDataClass="data.frame",
              RDataVersion=numeric_version("0.0.1"),
              Coordinate_1_based=TRUE,
              Maintainer="Paul Shannon <pshannon@fhcrc.org>",
              DataProvider="nowhere",
              Notes="no notes",
              RDataDateAdded=as.POSIXct("2013-01-01", tz="GMT"))

    recipe <- AnnotationHubRecipe(md)
    updated.metadata <- run(recipe)
    checkTrue(file.exists(outputFile(recipe)))
    name.of.loaded.variable <- load(outputFile(recipe))
    checkEquals(name.of.loaded.variable, "tbl")

    checkEquals(dim(tbl), c(9, 17))
    colClasses <- sapply(tbl, class)
    checkEquals(as.character(colClasses), col.classes)
    checkEquals(names(colClasses),
                c("factor", "gene_symbol","entrezgene","coord","experiment",
                  "chr","start","end","width","strand","feature",
                  "feature_start_position","feature_end_position","insideFeature",
                  "distancetoFeature","shortestDistance","fromOverlappingOrNearest"))

       # spot check a few cells
    checkEquals(tbl[1:5, "start"], c(4516539,5017828,5103196,5138407,6135071))


} # test_haemCodeTableSample
#-------------------------------------------------------------------------------
