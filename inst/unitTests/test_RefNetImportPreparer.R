# test_RefNetImportPreparer.R
#-------------------------------------------------------------------------------
.printf <- function(...) noquote(print(sprintf(...)))
#-------------------------------------------------------------------------------
library(httr)
url.exists <- function(url) {
    HEAD(url)$headers$status == "200"
    }
#-------------------------------------------------------------------------------
paulsTests <- function()
{
    library(AnnotationHubData)
    library(RUnit)

    test_.getRefNetFileURIs()
    test_defaultConstructor()
    test_.ahMetadataFromRefNetFiles()
    test_RefNetImportPreparer()
    test_newResources()
    test_recipe()

} # paulsTests
#-------------------------------------------------------------------------------
test_defaultConstructor <- function()
{
    print("--- test_defaultConstructor")
    
    checkTrue(validObject(new("RefNetImportPreparer")))

} # test_defaultConstructor()
#-------------------------------------------------------------------------------
test_.getRefNetFileURIs <- function()
{
    print("--- test_.getRefNetFileURIs")
    file.info <- AnnotationHubData:::.getRefNetFileURIs()
    checkEquals(names(file.info), c("repo.base.url", "filenames"))
    checkEquals(file.info$repo.base.url, "http://s3.amazonaws.com/refnet-networks")
       # the only files that should be stored at the repo are xxx.tsv files.
       # make sure there is at least one file, and that it has a proper name
    allFiles <- file.info$filenames
    checkTrue(length(allFiles) > 0)
    files.matching.expected.form <- grep(".*\\.tsv$", allFiles, value=TRUE)
    checkEquals(length(files.matching.expected.form), length(allFiles))

} # test_.getRefNetFileURIs
#-------------------------------------------------------------------------------
test_.ahMetadataFromRefNetFiles <- function()
{
    print("--- test_.ahMetadataFromRefNetFiles")
    
    file.info <- AnnotationHubData:::.getRefNetFileURIs()
    repo <- file.info$repo.base.url
    test.file <- "hypoxiaSignaling-2006.tsv"
    checkTrue(test.file %in% file.info$filenames)

    ahRoot <- tempdir()
    ahmd.list <- AnnotationHubData:::.ahMetadataFromRefNetFiles(ahRoot,
                                                                repo,
                                                                test.file,
                                                                verbose=FALSE)
    checkEquals(length(ahmd.list), 1)
    ahmd <- ahmd.list[[1]]
    checkTrue(is(ahmd, "AnnotationHubMetadata"))
    checkEquals(ahmd@AnnotationHubRoot, ahRoot)
    checkEquals(ahmd@TaxonomyId, "9606")
    checkEquals(ahmd@SourceFile,  "hypoxiaSignaling-2006.tsv")
    checkEquals(ahmd@SourceUrl, file.path(repo, test.file))
    checkEquals(ahmd@DataProvider, "RefNet")
    checkEquals(ahmd@RDataPath,   "refnet/hypoxiaSignaling-2006.tsv_0.0.1.RData")

} # test_.ahMetadataFromRefNetFiles 
#-------------------------------------------------------------------------------
test_RefNetImportPreparer <- function()
{
    print("--- test_RefNetImportPreparer")

    checkException(!validObject(
           RefNetImportPreparer(annotationHubRoot="/bogus/fake")), silent=TRUE)

    temp.dir <- tempdir()
    rnip <- RefNetImportPreparer(annotationHubRoot=temp.dir,
                                 verbose=TRUE,
                                 maxForTesting=1)
    checkEquals(annotationHubRoot(rnip), temp.dir)
    checkEquals(length(metadataList(rnip)), 1)
       # though only a list of 1, subset with [[ to get the element out
    md1 <- metadataList(rnip)[[1]]
    checkEquals(md1@Recipe, "tsvToRefnet")

} # test_RefNetImportPreparer
#-------------------------------------------------------------------------------
test_newResources <- function()
{
    print("--- test_newResources")
    
    rnip <- RefNetImportPreparer(tempdir())
    existing.resources <- metadataList(rnip)
    new.resources <- newResources(rnip, existing.resources[1])
    checkEquals(length(new.resources), 1)
    checkTrue(is(new.resources[[1]],"AnnotationHubMetadata"))

} # test_newResources
#------------------------------------------------------------------------------------------------------------------------
test_recipe <- function()
{
    print("--- test_recipe")
    test.directory <-  tempdir()

       # create a clean slate
    unlink(file.path(test.directory, dir(test.directory)))
    destination.directory <- file.path(test.directory, "refnet")
    if(!file.exists(destination.directory)){
       dir.create(destination.directory)
       stopifnot(file.exists(destination.directory))
       }
    
    rnip <- RefNetImportPreparer(annotationHubRoot=test.directory,
                                 verbose=FALSE,
                                 maxForTesting=1)
    md.list <- metadataList(rnip)
    checkEquals(length(md.list), 1)
    ahRoot <- annotationHubRoot(rnip)
    checkEquals(ahRoot, test.directory)

    md <- md.list[[1]]
    remote.file <- md@SourceUrl
    checkTrue(url.exists(remote.file))
    .printf("downloading %s", remote.file)
    local.file <- file.path(ahRoot, basename(md@SourceFile))
    download.file(remote.file, local.file, quiet=TRUE)
    checkTrue(file.exists(local.file))
    .printf("local: %s", local.file)
    recipe <- AnnotationHubRecipe(md)
    .printf("--- running recipe %s for %s", md@Recipe, md@SourceFile)
    md2 <- run(recipe)
    serializedDataFileName <- file.path(md2@AnnotationHubRoot, md2@RDataPath)
    checkTrue(file.exists(serializedDataFileName))
    .printf("loading %s", serializedDataFileName)
    load(serializedDataFileName)
    checkEquals(ncol(tbl), 28)
    checkEquals(colnames(tbl), c("A", "B",
                                 "altA", "altB",
                                 "aliasA", "aliasB",
                                 "detectionMethod", "firstAuthor",
                                 "publicationID", "taxonA",
                                 "taxonB", "type",
                                 "sourceDatabases", "interactionID",
                                 "confidenceScore", "provider",
                                 "A.common", "B.common",
                                 "A.canonical", "B.canonical",
                                 "cellType", "a.modification",
                                 "a.cellularComponent", "b.modification",
                                 "b.cellularComponent", "a.canonicalIdType",
                                 "b.canonicalIdType", "comment"))

} # test_recipe
#-------------------------------------------------------------------------------
