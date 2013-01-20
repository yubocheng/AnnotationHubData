library(AnnotationHubData)
library(AnnotationHubServer)
library(RUnit)
#-------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#-------------------------------------------------------------------------------
bulkBroadPeakImport <- function(max.data.resource.size=20000)
{
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)

    tbl.bpk <- subset(tbl.md, type=="broadPeak" & size < max.data.resource.size)
    printf(" %d broadPeak resources below %d bytes", nrow(tbl.bpk), max.data.resource.size)
    #annotationHubRoot <- "."
    annotationHubRoot <- "/mnt/extdata/AnnotationHubServer_data"

    for(r in 1:nrow(tbl.bpk)) {
       directory <- tbl.bpk[r, "dataDir"]
       file.info <- as.list(tbl.bpk[r,])
       dataFileName <- rownames(tbl.bpk)[r]
       file.size <- tbl.bpk[r,"size"]
       printf("creating resource for %s of size: %d", dataFileName, file.size)

       experiment.metadata <- as.list(tbl.md[dataFileName,])
       webSiteRoot <- "http://hgdownload.cse.ucsc.edu"
       genomeVersion <- "hg19"
       RDataFilename <- createResource(importer, annotationHubRoot,
                                        webSiteRoot, genomeVersion,
                                        dataFileName, experiment.metadata)

       load(RDataFilename)
       printf(" %60s: %d rows (%f bytes)", dataFileName, length(gr), file.size)
       } # for r


} # bulkBroadPeakImport
#-------------------------------------------------------------------------------
