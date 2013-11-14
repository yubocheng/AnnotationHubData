#-------------------------------------------------------------------------------
paulsTests <- function()
{
    library(AnnotationHubData)
    library(RUnit)

    test_ctor()
    test_oneExperimentMetadataExtractAndParse()
    test_allExperimentsMetadataExtractAndParse()

    test_newResources()

    # admin_test_endToEndProcessing()

} # paulsTests
#-------------------------------------------------------------------------------
test_ctor <- function()
{
    print("--- test_ctor")
    
    checkTrue(validObject(new("HaemCodeImportPreparer")))
    checkException(!validObject(
          HaemCodeImportPreparer(annotationHubRoot="/bogus/fake",
                                 maxForTesting=0)),
          silent=TRUE)

} # test_ctor
#-------------------------------------------------------------------------------
# though tested with the first entry in the oct 2013 release from HAEMCODE,
#       CTCF_GSM546526_Pro-B-Cells
# these tests should pass for all HAEMCODE curated data
#
# one experiment produces three for HaemCode:
#   
test_oneExperimentMetadataExtractAndParse <- function()
{
    print("--- test_oneExperimentMetadataExtractAndParse")
    hip <- HaemCodeImportPreparer(annotationHubRoot=tempdir(),
                                  verbose=FALSE, maxForTesting=1)
    md.list <- metadataList(hip)
    checkEquals(length(md.list), 3)  # 3 files for each experiment
    checkTrue(file.exists(annotationHubRoot(hip)))

      # spot check some of the metadata fields

    checkEquals(sapply(md.list, function(el) el@SourceFile),
                c("blood/BigWig/mm10/CTCF_GSM546526_Pro-B-Cells.bw",
                  "blood/Peaks/mm10/CTCF_GSM546526_Pro-B-Cells.bed",
                  "blood/geneList/CTCF_GSM546526_Pro-B-Cells.csv"))

    checkEquals(sapply(md.list, function(el) el@SourceUrl),
      c("http://haemcode.stemcells.cam.ac.uk/blood/BigWig/mm10/CTCF_GSM546526_Pro-B-Cells.bw",
        "http://haemcode.stemcells.cam.ac.uk/blood/Peaks/mm10/CTCF_GSM546526_Pro-B-Cells.bed",
        "http://haemcode.stemcells.cam.ac.uk/blood/geneList/CTCF_GSM546526_Pro-B-Cells.csv"))

    checkEquals(sapply(metadataList(hip), function(el) el@DataProvider),
                rep("HAEMCODE", 3))
    checkEquals(sapply(metadataList(hip), function(el) el@Genome),
                rep("mm10", 3))

    checkEquals(sapply(metadataList(hip), function(el) el@Species),
                rep("Mus musculus", 3))
    checkEquals(sapply(metadataList(hip), function(el) el@Recipe),
                c("rtrackLayerImport", "rtrackLayerImport", "importTable"))
    checkTrue(any(grepl("aligned reads", metadataList(hip)[[1]]@Tags)))
    checkTrue(any(grepl("called TF binding peaks", metadataList(hip)[[2]]@Tags)))
    checkTrue(any(grepl("nearby genes", metadataList(hip)[[3]]@Tags)))

        # check each of the tags in metadata element 1
    
    tags <- md.list[[1]]@Tags

    checkEquals(tags[1], "experiment.type=ChIP-Seq")
    checkEquals(tags[2], "data.type=aligned reads")
    checkEquals(tags[3], "cell.type=B-Cells")
    checkEquals(tags[4], "cell.subtype=[PC] Pro-B-Cells (Rag1-/-)")
    checkEquals(tags[5], "transcription.factor=Ctcf")
    checkEquals(tags[6], "geneID=13018")
    checkEquals(tags[7], "geo.series=GSE21978")
    checkEquals(tags[8], "geo.sample=GSM546526")

    
} # test_oneExperimentMetadataExtractAndParse
#-------------------------------------------------------------------------------
test_allExperimentsMetadataExtractAndParse <- function()
{
    print("--- test_allExperimentsMetadataExtractAndParse")
    hip <- HaemCodeImportPreparer(annotationHubRoot=tempdir(),
                                  verbose=FALSE, maxForTesting=NA)
    md.list <- metadataList(hip)
    checkEquals(length(md.list), 945)  # 3 files for each experiment
    checkTrue(file.exists(annotationHubRoot(hip)))

      # spot check some of the metadata fields
    checkEquals(unique(sapply(md.list, function(el) el@DataProvider)), "HAEMCODE")
    checkEquals(sort(unique(sapply(md.list, function(el) el@Recipe))),
                c("importTable","rtrackLayerImport"))
    
    sourceFiles <- sapply(md.list, function(el) el@SourceFile)
       # each source file different
    checkEquals(length(unique(sourceFiles)), length(sourceFiles))
    expected <- length(sourceFiles)/3
    checkEquals(length(grep("BigWig", sourceFiles)), expected)
    checkEquals(length(grep("geneList", sourceFiles)), expected)
    checkEquals(length(grep("Peaks", sourceFiles)), expected)
    
    tags <- sapply(md.list, function(el) el@Tags)
    files.per.experiment <- 3
    tags.per.file <- 8
    checkEquals(length(tags), expected * files.per.experiment * tags.per.file)

} # test_allExperimentsMetadataExtractAndParse
#-------------------------------------------------------------------------------
# takes about 2 minutes elapsed time
admin_test_endToEndProcessing <- function()
{
    print("--- test_endToEndProcessing")
    unlink(file.path(tempdir(), dir(tempdir())))
    max.experiments=1
    hip <- HaemCodeImportPreparer(tempdir(), verbose=TRUE,
                                  maxForTesting=max.experiments)
    md.list <- metadataList(hip)
    checkEquals(length(md.list), 3 * max.experiments)
    ahRoot <- annotationHubRoot(hip)

    for(md in md.list) {
        remote.file <- md@SourceUrl
        printf("downloading %s", remote.file)
        checkTrue(RCurl::url.exists(remote.file))
        directory.path <- file.path (ahRoot, dirname(md@SourceFile))
        if(!file.exists(directory.path))
            dir.create(directory.path, recursive=TRUE)
        local.file <- file.path(directory.path, basename(md@SourceFile))
        download.file(remote.file, local.file, quiet=TRUE)
        checkTrue(file.exists(local.file))
        printf("local: %s", local.file)
        recipe <- AnnotationHubRecipe(md)
        printf("--- running recipe for %s", md@SourceFile)
        md2 <- run(recipe)
        serializedDataFileName <- file.path(md2@AnnotationHubRoot, md2@RDataPath)
        checkTrue(file.exists(serializedDataFileName))
        printf("loading %s", serializedDataFileName)
        load(serializedDataFileName)
        if(md@Recipe == "rtrackLayerImport"){
            printf(" gr size: %d", length(gr))
            checkTrue(length(gr) > 100)
            }
        if(md@Recipe == "importTable"){
            printf("tbl size: %d", nrow(tbl))
            checkTrue(nrow(tbl) > 100)
            }
        } # for md
    
} # admin_test_endToEndProcessing
#-------------------------------------------------------------------------------

