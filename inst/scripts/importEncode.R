# exploratory code, much of which will migrate to the EncodeImporter class
#--------------------------------------------------------------------------------
library(AnnotationHubData)
library(AnnotationHubServer)
library(rtracklayer)
library(RUnit)
library(rmongodb)
#-------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#-------------------------------------------------------------------------------
# all operations here require an instance of the importer, and easy access to
# the per-experiment metadata it contains (which is extracted and combined
# from all of the "files.txt" files at
# 
importer <- EncodeImporter()
tbl.md <- metadataTable(importer)
#-------------------------------------------------------------------------------
runTests <- function()
{
    test_importResource()
    test_mongoFind()

} # runTests
#-------------------------------------------------------------------------------
importResource <- function(dataFileName,
                           annotationHubRoot="/mnt/extdata/AnnotationHubServer_data",
                           webSiteRoot="http://hgdownload.cse.ucsc.edu",
                           genomeVersion="hg19")
{
    stopifnot(dataFileName %in% rownames(tbl.md))
    experiment.metadata <- as.list(tbl.md[dataFileName,])
    RDataFilename <- createResource(importer, annotationHubRoot,
                                    webSiteRoot, genomeVersion,
                                    dataFileName, experiment.metadata)

} # importResource
#-------------------------------------------------------------------------------
test_importResource <- function()
{
    print("--- test_importResource")
        # the smallest encode file, just 204 bytes
    dataFileName <- "wgEncodeSunySwitchgearHt1080Elavl1RbpAssocRna.broadPeak.gz"

    RDataFile <- importResource(dataFileName)

    if(exists("gr")) 
        rm(gr)

    load(RDataFile)
    checkEquals(length(gr), 5)
    checkEquals(sort(seqnames(seqinfo(gr))), c("chr10", "chr5", "chr8","chrX" ))
    checkEquals(colnames(mcols(gr)), c("name", "score", "signalValue", "pValue", "qValue"))
 

} # test_importResource
#-------------------------------------------------------------------------------
bulkImport <- function(formatName, min.data.resource.size=0, 
                       max.data.resource.size=20000)
{
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)

    tbl.bpk <- subset(tbl.md, type==formatName & size <= max.data.resource.size & size > min.data.resource.size)
    printf(" %d %s resources between %8.0f and %8.0f bytes", nrow(tbl.bpk), formatName,
           min.data.resource.size, max.data.resource.size)
    #annotationHubRoot <- "."
    annotationHubRoot <- "/mnt/extdata/AnnotationHubServer_data"

    for(r in 1:nrow(tbl.bpk)) {
       directory <- tbl.bpk[r, "dataDir"]
       file.info <- as.list(tbl.bpk[r,])
       dataFileName <- rownames(tbl.bpk)[r]
       file.size <- tbl.bpk[r,"size"]
       printf("creating resource for %s of size: %8.0f", dataFileName, file.size)

       experiment.metadata <- as.list(tbl.md[dataFileName,])
       webSiteRoot <- "http://hgdownload.cse.ucsc.edu"
       genomeVersion <- "hg19"
       RDataFilename <- createResource(importer, annotationHubRoot,
                                        webSiteRoot, genomeVersion,
                                        dataFileName, experiment.metadata)

       load(RDataFilename)
       printf(" %60s: %d rows (%8.0f bytes)", dataFileName, length(gr), file.size)
       } # for r


} # bulkImport
#-------------------------------------------------------------------------------
singleImport <- function(filename, tbl.md=NULL)
{
    if(is.null(tbl.md)) {
        importer <- EncodeImporter()
        tbl.md <- metadataTable(importer)
        }

    stopifnot(filename %in% rownames(tbl.md))
    tbl.sub <- tbl.md[filename,]
    annotationHubRoot <- "/mnt/extdata/AnnotationHubServer_data"

    for(r in 1:nrow(tbl.sub)) {
       directory <- tbl.sub[r, "dataDir"]
       file.info <- as.list(tbl.sub[r,])
       dataFileName <- rownames(tbl.sub)[r]
       file.size <- tbl.sub[r,"size"]
       printf("creating resource for %s of size: %8.0f", dataFileName, file.size)

       experiment.metadata <- as.list(tbl.md[dataFileName,])
       webSiteRoot <- "http://hgdownload.cse.ucsc.edu"
       genomeVersion <- "hg19"
       RDataFilename <- createResource(importer, annotationHubRoot,
                                        webSiteRoot, genomeVersion,
                                        dataFileName, experiment.metadata)

       load(RDataFilename)
       printf(" %60s: %d rows (%8.0f bytes)", dataFileName, length(gr), file.size)
       } # for r

} # singleImport
#-------------------------------------------------------------------------------
test_singleImport <- function()
{
        # the smallest encode file, just 204 bytes
    filename <- "wgEncodeSunySwitchgearHt1080Elavl1RbpAssocRna.broadPeak.gz"
    singleImport(filename)
  
} # test_singleImport
#-------------------------------------------------------------------------------
mongoFind <- function(expression)
{
  cursor <- mongo.find(AnnotationHubServer:::.mongo$getMongo(),
                       AnnotationHubServer:::.mongo$getNamespace(),
                       list(RDataPath="fakedata/data.bed_0.0.3.RData"))

  result <- list()
  while(mongo.cursor.next(cursor)) {
      x <- mongo.bson.to.list(mongo.cursor.value(cursor));
      result <- c (result, x)
      }

  result
  
} # mongoFind
#-------------------------------------------------------------------------------
test_mongoFind <- function()
{
   zz <- mongoFind(list(RDataPath="fakedata/data.bed_0.0.3.RData"))
   checkEquals(sort(names(zz)),
               c("BiocVersion", "Coordinate_1_based", "DataProvider", 
                 "Description", "Genome", "_id", 
                 "Maintainer", "Notes", "RDataClass", 
                 "RDataDateAdded", "RDataLastModifiedDate", "RDataPath", 
                 "RDataSize", "RDataVersion", "Recipe", 
                 "SourceFile", "SourceLastModifiedDate", "SourceMd5", 
                 "SourceSize", "SourceUrl", "SourceVersion", 
                 "Species", "Tags", "TaxonomyId", 
                 "Title"))


} # test_mongoFind
#-------------------------------------------------------------------------------
