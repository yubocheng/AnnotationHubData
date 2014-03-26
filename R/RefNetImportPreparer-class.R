#-------------------------------------------------------------------------------
setClass("RefNetImportPreparer",
         representation=representation(annotationHubRoot="character",
                                       ahmd.list="list"),
         contains="ImportPreparer")


#------------------------------------------------------------------------------
setValidity("RefNetImportPreparer", function(object) {

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
setMethod("metadataList", "RefNetImportPreparer",

    function(object) {
        object@ahmd.list
    })
#-------------------------------------------------------------------------------
setMethod("annotationHubRoot", "RefNetImportPreparer",

    function(object) {
        object@annotationHubRoot
    })
#-------------------------------------------------------------------------------
setMethod("show", "RefNetImportPreparer",

    function(object) {
        msg = sprintf("RefNetImportPreparer object with")
        cat (msg, "\n", sep="")
        count <- length(object@ahmd.list)
        if(identical(object@annotationHubRoot, character(0))){
            ahroot="unassigned"
        } else {
            ahroot=object@annotationHubRoot
            }
        msg = sprintf("  annotationHubRoot: %s", ahroot)
        cat (msg, "\n", sep="")
        msg = sprintf("  AnnotationHubMetadata list of size %d", count)
        cat (msg, "\n", sep="")
        for(i in seq_len(count)){
            md <- object@ahmd.list[[i]]
            msg <- sprintf("    %s (%s) %s", md@SourceFile, md@RDataDateAdded,
                           md@Recipe)
            cat(msg, "\n", sep="")
            } # for i
        })

#------------------------------------------------------------------------------
.newRefNetResources <- function(importPreparer, currentMetadata)
{
    knownURIs <- sapply(currentMetadata, function(elt) {
        metadata(elt)$SourceUrl
        })

    currentURIs.raw <- .getRefNetFileURIs()
    currentURIs <- file.path(currentURIs.raw$repo.base.url,
                             currentURIs.raw$filenames)

    new.uris <- currentURIs[!currentURIs %in% knownURIs]
    if(length(new.uris) == 0)
        return(list())

    ah.root <- annotationHubRoot(importPreparer)
    repo <- unique(.getRefNetFileURIs()$repo.base.url)
    filenames <- basename(new.uris)
    ahmd.list <- AnnotationHubData:::.ahMetadataFromRefNetFiles(ah.root,
                                                                repo,
                                                                filenames,
                                                                verbose=FALSE)
    ahmd.list
    
} # .newRefNetResources
#------------------------------------------------------------------------------
setMethod("newResources", signature="RefNetImportPreparer",

    function(importPreparer, currentMetadata=list(), ...) {
       return(.newRefNetResources(importPreparer, currentMetadata))
       })

#------------------------------------------------------------------------------
.getRefNetFileURIs <- function()
{
    base.url <- "http://s3.amazonaws.com/refnet-networks"

       # everything is embedded in the second line of xml
    raw.text <- scan(base.url, what=character(0), sep="\n", quiet=TRUE)[2]
    raw.tokens <- strsplit(raw.text, "<")[[1]]
    filenames.raw <- raw.tokens[grep("^Key>", raw.tokens)]
    stopifnot(length(filenames.raw) > 0)
    filenames <- sub("Key>", "", filenames.raw)
    list(repo.base.url=base.url, filenames=filenames)

} # .getRefNetFileURIs
#------------------------------------------------------------------------------
.ahMetadataFromRefNetFiles <- function(annotationHubRoot, repo.base.url,
                                       filenames, verbose=FALSE)
{
    max <- length(filenames)
    entry <- 0

    result <- vector("list", max)
    
    for(i in seq_len(max)){
       filename <- filenames[i]
       if(verbose) .printf("--- %d: %s", i, filename)
       entry <- entry + 1
       directory <- repo.base.url
       recipe.name <- "refnetImporter"
       recipe.args <- list()
       file.extension <- "tsv"
       data.class <- "data.frame"
       data.type <- "interactions"
       sourceFile <- basename(filename)
       sourceUrl <- file.path(repo.base.url, filename)
       size <- 0
       description <- filename
       dataVersion <- "0.0.1"
         # this test is not appropriate for aws s3, the amazon web service/cloud 
         #     stopifnot(url.exists(sourceUrl))
       top.lines.in.file <- scan(sourceUrl, sep="\n", what=character(0), n=20,
                                 quiet=TRUE)
       unlink(sourceUrl)
       comment.lines <- grep("^# ", top.lines.in.file, value=TRUE)
       comment.lines <- sub("# ", "", comment.lines)
       comment.tokens <- strsplit(comment.lines, ": ")
       keywords <- unlist(lapply(comment.tokens, "[", 2))
       names(keywords) <- unlist(lapply(comment.tokens, "[", 1))

       filename.stem <- sub(".tsv", "", filename)
       title <- sprintf("interactions from %s", filename.stem)
       if("TITLE" %in% names (keywords))
           title <- keywords[["TITLE"]]
       
       data.provider <- "RefNet"
       #if("DATA.PROVIDER" %in% names (keywords))
       #    data.provider <- keywords[["DATA.PROVIDER"]]
        
       description <- sprintf("interactions from %s", filename.stem)
       if("DESCRIPTION" %in% names (keywords))
           description <- keywords[["DESCRIPTION"]]
        
       tbl <- read.table(sourceUrl, sep="\t", header=TRUE, as.is=TRUE)
       expected.colnames <- c("a.canonical",
                              "b.canonical",
                              "relation",
                              "bidirectional",
                              "detectionMethod",
                              "pmid",
                              "a.organism",
                              "b.organism",
                              "a.common",
                              "a.canonicalIdType",
                              "b.common",
                              "b.canonicalIdType",
                              "cellType",
                              "a.modification",
                              "a.cellularComponent",
                              "b.modification",
                              "b.cellularComponent",
                              "provider",
                              "comment")
       
       stopifnot(length(intersect(colnames(tbl), expected.colnames)) == ncol(tbl))
       organisms <- as.character(unique(c(tbl$a.organism, tbl$b.organism)))

       organism.name <- organisms

       if("ORGANISM.NAME" %in% names (keywords))
           organism.name <- as.character(keywords[["ORGANISM.NAME"]])

       tags <- c("interactions", title, description)

       current.version = "0.0.1"
       expected.RDataPath <- file.path("refnet",
                                       sprintf("%s_%s.%s",
                                               basename(sourceFile),
                                               current.version,
                                               "RData"));
                                               
       result[[entry]] <- AnnotationHubMetadata(
                                 AnnotationHubRoot=annotationHubRoot,
                                 SourceFile=sourceFile,
                                 SourceSize=size,
                                 SourceUrl=sourceUrl,
                                 SourceVersion=dataVersion,
                                 DataProvider=data.provider,
                                 Title=title,
                                 Description=description,
                                 Species=organism.name,
                                 TaxonomyId=organisms,
                                 Genome=NA_character_,
                                 Tags=tags,
                                 Recipe="tsvToRefnet",
                                 RecipeArgs=list(),
                                 RDataPath=expected.RDataPath,
                                 RDataClass="data.frame",
                                 RDataVersion=numeric_version("0.0.1"),
                                 RDataDateAdded=as.Date(Sys.Date(), "%Y"),
                                 Maintainer="Paul Shannon <pshannon@fhcrc.org>",
                                 Coordinate_1_based=TRUE,
                                 Notes=NA_character_)
      } # for max

    result


} # .ahMetadataFromRefNetFiles
#-------------------------------------------------------------------------------
# constructor, ctor
RefNetImportPreparer <- function(annotationHubRoot, verbose=FALSE,
                                 maxForTesting=NA)
{
    fileURIs <- .getRefNetFileURIs()

    ahmd.list <- .ahMetadataFromRefNetFiles(annotationHubRoot,
                                            fileURIs$repo.base.url,
                                            fileURIs$filenames,
                                            verbose)   # for testing
    if(!is.na(maxForTesting))
        ahmd.list <- ahmd.list[seq_len(maxForTesting)]
    
    self <- new("RefNetImportPreparer", annotationHubRoot=annotationHubRoot,
                ahmd.list=ahmd.list)
   
    self
   
} # constructor
#------------------------------------------------------------------------------
