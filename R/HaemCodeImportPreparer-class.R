setClass("HaemCodeImportPreparer",
         representation=representation(annotationHubRoot="character",
                                       ahmd.list="list"),
         contains="ImportPreparer")

#------------------------------------------------------------------------------
setValidity("HaemCodeImportPreparer", function(object) {
    msg = NULL
        # annotationHubRoot slot can be empty, or an actual
        # existing directory
    if(!identical(object@annotationHubRoot, character(0))){
        if(!file.exists(object@annotationHubRoot))
             msg <- c(msg, "annotationHubRoot directory does not exist")
      }
    if (is.null(msg)) TRUE else msg
    })
#-------------------------------------------------------------------------------
setMethod("show", "HaemCodeImportPreparer",

    function(object) {
        msg = sprintf("HaemCodeImportPreparer object with")
        cat (msg, "\n", sep="")
        msg = sprintf("  AnnotationHubMetadata list of size %d",
                      length(object@ahmd.list))
        cat (msg, "\n", sep="")
        if(identical(object@annotationHubRoot, character(0))){
            ahroot="unassigned"
        } else {
            ahroot=object@annotationHubRoot
            }
        msg = sprintf("  annotationHubRoot: %s", ahroot)
        cat (msg, "\n", sep="")
        })

#------------------------------------------------------------------------------
.newHaemCodeResources <- function(self, currentMetadata=list())
{
    hip <- HaemCodeImportPreparer(annotationHubRoot=tempdir())
    md.list <- metadataList(hip)
    available.urls <- sapply(md.list, function(elt) metadata(elt)$SourceUrl)
    current.urls <- sapply(currentMetadata,
                           function(elt) metadata(elt)$SourceUrl)
    new.urls <- setdiff(available.urls, current.urls)
    indices <- match(new.urls, available.urls)
    
    if(length(indices) > 0)
      return(md.list[indices])

    return(list())

} # .newHaemCodeResources
#------------------------------------------------------------------------------
  # the HaemCode data is specified statically: we were given
  # a text file containing filenames, accompanied by another
  # file supplying metadata.  these are found in extdata:
  #    annotation_haemcode.tsv (38k)
  #    haemCodeFileList.txt (10k)
  # new resources will be accompanied by new versions of these
  # two files, so the current method is essentially a nop

setMethod("newResources", signature="HaemCodeImportPreparer",

    function(importPreparer, currentMetadata=list(), ...) {
       return(.newHaemCodeResources(importPreparer, currentMetadata))
       })

#------------------------------------------------------------------------------
# general utility functions not yet generally available:
.printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------
HaemCodeImportPreparer <- function(annotationHubRoot, verbose=FALSE,
                                   maxForTesting=NA)
{
    filename <- system.file(package="AnnotationHubData", "extdata",
                            "haemCodeFileList.txt")
    stopifnot(file.exists(filename))
    
    metadata.filename <-  system.file(package="AnnotationHubData", "extdata",
                                      "annotation_haemcode.tsv")
    stopifnot(file.exists(metadata.filename))
    tbl.md <- read.table(metadata.filename, sep="\t", header=TRUE, as.is=TRUE)
                         
    chipSeqExperiments <- scan(filename, what=character(0), sep="\n", quiet=TRUE)
    stopifnot(all(chipSeqExperiments %in% tbl.md$filename))

    if(!is.na(maxForTesting) & maxForTesting < length(chipSeqExperiments))
        chipSeqExperiments <- chipSeqExperiments[1:maxForTesting]

    ahmd.list <- .haemCodeMetadataToAnnotationHubMetadata(chipSeqExperiments,
                                                          tbl.md,
                                                          annotationHubRoot,
                                                          verbose)
    self <- new("HaemCodeImportPreparer", annotationHubRoot=annotationHubRoot,
                ahmd.list=ahmd.list)
   
    self
   
} # constructor
#------------------------------------------------------------------------------
setMethod("annotationHubRoot", "HaemCodeImportPreparer",

    function(object) {
        object@annotationHubRoot
      })

#------------------------------------------------------------------------------
setMethod("metadataList", "HaemCodeImportPreparer",

    function(object) {
        object@ahmd.list
        })

#------------------------------------------------------------------------------
setMethod("sourceUrls", "HaemCodeImportPreparer",

    function(object) {
        sapply(object@ahmd.list, function(elt) metadata(elt)$SourceUrl)
        })

#------------------------------------------------------------------------------
.haemCodeMetadataToAnnotationHubMetadata <- function (file.list, tbl.md,
                                                      annotationHubRoot,
                                                      verbose=FALSE,
                                                      test.urls=FALSE)
{
       # these three file types are provided, in different directories,
       # for each experiment (and thus for each row the correlated
       # file.list and tbl.md
    
    file.types <- c("bigWig", "peaks", "geneList")
    
    max <- length(file.list)
    result <- vector("list", max * 3)
    entry <- 0

    geneList.read.table.colClasses <- c(rep("character", 6),
                                        rep("numeric",   3),
                                        rep("character", 2),
                                        rep("numeric",   2),
                                        rep("character", 1),
                                        rep("numeric",   2),
                                        rep("character", 1))
    
    for(i in 1:max){
       filename <- file.list[i]
       metadata <- as.list(tbl.md[which(tbl.md$filename == filename),])
       if(verbose) .printf("--- %d: %s", i, filename)
       for(file.type in file.types){
           entry <- entry + 1
           directory <- switch(file.type,
                               bigWig="blood/BigWig/mm10",
                               peaks="blood/Peaks/mm10",
                               geneList="blood/geneList")
           
           recipe.name <- switch(file.type,
                                 bigWig="rtrackLayerImport",
                                 peaks="rtrackLayerImport",
                                 geneList="importTable")
           recipe.args <- switch(file.type,
                                 bigWig=list(),
                                 peaks=list(),
                                 geneList=list(header=TRUE, sep=",",
                                               colClasses=geneList.read.table.colClasses))

           file.extension <- switch(file.type,
                                 bigWig="bw",
                                 peaks="bed",
                                 geneList="csv")
           data.class <- switch(file.type,
                                 bigWig="GRanges",
                                 peaks="GRanges",
                                 geneList="data.frame")
           data.type <- switch(file.type,
                                 bigWig="aligned reads",
                                 peaks="called TF binding peaks",
                                 geneList="nearby genes")

           sourceFile <- file.path(directory, filename)
           sourceUrl <- paste("http://haemcode.stemcells.cam.ac.uk",
                              directory,
                              filename,
                              sep="/")
           sourceFile <- paste(sourceFile, file.extension, sep=".")
           sourceUrl <- paste(sourceUrl, file.extension, sep=".")
           size <- 0
           if(verbose)
               .printf("url: %s", sourceUrl)
           if(test.urls){
               if(!url.exists(sourceUrl)) {
                   .printf("url %s does not exist, skipping", sourceUrl)
                next
                }
             }# if test.urls
           tags <- c("experiment.type=ChIP-Seq",
                     sprintf("data.type=%s",            data.type),
                     sprintf("cell.type=%s",            metadata$CT_General),
                     sprintf("cell.subtype=%s",         metadata$CT_Subtype),
                     sprintf("transcription.factor=%s", metadata$factor),
                     sprintf("geneID=%s",               metadata$factorGeneID),
                     sprintf("geo.series=%s",           metadata$GSE),
                     sprintf("geo.sample=%s",           metadata$GSM))
           description <- filename
           dataVersion <- "0.0.1"
           result[[entry]] <- AnnotationHubMetadata(
                                     AnnotationHubRoot=annotationHubRoot,
                                     SourceFile=sourceFile,
                                     SourceSize=size,
                                     SourceUrl=sourceUrl,
                                     SourceVersion=dataVersion,
                                     DataProvider="HAEMCODE",
                                     Title=description,
                                     Description=description,
                                     Species="Mus musculus",
                                     TaxonomyId="10090",
                                     Genome="mm10",
                                     Tags=tags,
                                     Recipe=recipe.name,
                                     RecipeArgs=recipe.args,
                                     RDataClass=data.class,
                                     RDataVersion=numeric_version("0.0.1"),
                                     RDataDateAdded=as.Date(Sys.Date(), "%Y"),
                                     Maintainer="Paul Shannon <pshannon@fhcrc.org>",
                                     Coordinate_1_based=TRUE,
                                     Notes=NA_character_)

           } # for file.type
      } # for max

    if(verbose)
        printf("--- leaving .haemCodeMetadataToAHMD, count: %d", length(result))
    #browser()
    #xyzzz <- 91231
    result

} # .haemCodeMetadataToAnnotationHubMetadata
#------------------------------------------------------------------------------
