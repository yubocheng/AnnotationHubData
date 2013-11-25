# test_HaemCodeImportPreparer.R
#-------------------------------------------------------------------------------
.printf <- function(...) noquote(print(sprintf(...)))
library(httr)
url.exists <- function(url) {
    HEAD(url)$headers$status == "200"
    }
#-------------------------------------------------------------------------------
paulsTests <- function()
{
    library(AnnotationHubData)
    library(RUnit)

    test_ctor()
    test_oneExperimentMetadataExtractAndParse()
    test_allExperimentsMetadataExtractAndParse()

    test_newResources()

    admin_test_readTableArgs()
    admin_test_endToEndProcessing()

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
                c("haemcode/blood/BigWig/mm10/CTCF_GSM546526_Pro-B-Cells.bw",
                  "haemcode/blood/Peaks/mm10/CTCF_GSM546526_Pro-B-Cells.bed",
                  "haemcode/blood/geneList/CTCF_GSM546526_Pro-B-Cells.csv"))

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
test_newResources <- function()
{
    print("--- test_newResources")
    
         # the HaemCode data is specified statically: we were given
         # a text file containing filenames, accompanied by another
         # file supplying metadata.  these are found in extdata:
         #    annotation_haemcode.tsv (38k)
         #    haemCodeFileList.txt (10k)
         # the AnnotationHub loader does its loading by calling newResources
         # on each importPreparer, passing in a list of its current
         # holdings (as metadata). an empty list is passed at first load.

     hip <- HaemCodeImportPreparer(tempdir())
     existing.resources <- metadataList(hip)

         # simulate initial loading
     checkEquals(length(newResources(hip, list())), length(existing.resources))

         # simulate the state in which all resources are already loaded
     checkEquals(length(newResources(hip, existing.resources)), 0)

        # pretend that there are only five existing resources
     md <- newResources(hip, existing.resources[1:5])
     checkEquals(length(md), length(existing.resources) - 5)

     first.new.url <- md[[1]]@SourceUrl
     sixth.old.url <- existing.resources[[6]]@SourceUrl
     checkEquals(first.new.url, sixth.old.url)

} # test_newResources
#------------------------------------------------------------------------------------------------------------------------
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

    for(md in md.list[3]) {   # just do the 3rd file, the csv-delimited table
        remote.file <- md@SourceUrl
        .printf("downloading %s", remote.file)
        checkTrue(RCurl::url.exists(remote.file))
        directory.path <- file.path (ahRoot, dirname(md@SourceFile))
        if(!file.exists(directory.path))
            dir.create(directory.path, recursive=TRUE)
        local.file <- file.path(directory.path, basename(md@SourceFile))
        download.file(remote.file, local.file, quiet=TRUE)
        checkTrue(file.exists(local.file))
        .printf("local: %s", local.file)
        recipe <- AnnotationHubRecipe(md)
        .printf("--- running recipe for %s", md@SourceFile)
        md2 <- run(recipe)
        serializedDataFileName <- file.path(md2@AnnotationHubRoot, md2@RDataPath)
        checkTrue(file.exists(serializedDataFileName))
        .printf("loading %s", serializedDataFileName)
        load(serializedDataFileName)
        if(md@Recipe == "rtrackLayerImport"){
            .printf(" gr size: %d", length(gr))
            checkTrue(length(gr) > 100)
            }
        if(md@Recipe == "importTable"){
            .printf("tbl size: %d", nrow(tbl))
            checkTrue(nrow(tbl) > 100)
            }
        } # for md
    
} # admin_test_endToEndProcessing
#-------------------------------------------------------------------------------
# read -all- of the csv files provided by HaemCode to make sure the
# colClasses argument (in particular) is suitable
#
# predictable form of a geneList url:
#
#    http://haemcode.stemcells.cam.ac.uk/blood/geneList/FoxO1_GSM546525_Pro-B-Cells.csv
#
admin_test_readTableArgs <- function()
{
   print("--- admin_test_readTableArgs")
   filename <- system.file(package="AnnotationHubData", "extdata",
                           "haemCodeFileList.txt")
   tbl.md <- read.table(filename, sep="\t", header=TRUE, as.is=TRUE)
   experimentNames <- scan(filename, sep="\n", what=character(0))

   base.url <- "http://haemcode.stemcells.cam.ac.uk"
   directory <- "blood/geneList"
   extension <- "csv"

    col.classes <- c(rep("character", 6),
                     rep("numeric",   3),
                     rep("character", 2),
                     rep("numeric",   2),
                     rep("character", 1),
                     rep("numeric",   1),
                     rep("numeric",   1),
                     rep("character", 1))
                     


   for(experimentName in experimentNames){
      filename <- sprintf("%s.%s", experimentName, extension)
      url <- file.path(base.url, directory, filename)
      print(url)
      printf("%s exists? %s", experimentName, url.exists(url))
      x <- read.table(url, header=TRUE, sep=",", colClasses=col.classes)
      checkEquals(ncol(x), 17)
      checkTrue(nrow(x) > 0)
      }

} # admin_test_readTableArgs
#-------------------------------------------------------------------------------
admin_test_allUrlsExist <- function()
{
   filename <- system.file(package="AnnotationHubData", "extdata",
                           "haemCodeFileList.txt")
   experiments <- scan(filename, sep="\n", what=character(0))

   for(experiment in experiments){
      for(file.type in c("bigWig", "peaks", "geneList")){
         file.extension <- switch(file.type,
                                  bigWig="bw",
                                  peaks="bed",
                                  geneList="csv")
         directory <- switch(file.type,
                             bigWig="blood/BigWig/mm10",
                             peaks="blood/Peaks/mm10",
                             geneList="blood/geneList")
         filename <- sprintf("%s.%s", experiment, file.extension)
         url <- paste("http://haemcode.stemcells.cam.ac.uk",
                      directory,
                      filename,
                      sep="/")
         printf("%s: %s", url.exists(url), url)
         } # for file.type
     } # for experiment
    

} # admin_test_allUrlsExist
#-------------------------------------------------------------------------------
    
