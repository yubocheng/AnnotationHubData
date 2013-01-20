setClass("EncodeImporter",
    representation(
        tbl.md="data.frame"
        )
)
#------------------------------------------------------------------------------
EncodeImporter <- function()
{
   x <- new("EncodeImporter")
   file.path <- system.file("extdata", "encodeMetadata.RData",
                            package="AnnotationHubData")
   load(file.path)
   x@tbl.md <- tbl.md
   x
}
#------------------------------------------------------------------------------
setGeneric("metadataTable", signature="object",
           function(object)
           standardGeneric ("metadataTable"))

setGeneric("assembleParams", signature="object",
           function(object, experimentMetadata, webSiteSourceDirectory,
                    annotationHubRoot, projectPath,
                    genomeVersion, dataFileName)

           standardGeneric ("assembleParams"))


setGeneric("createResource", signature="object",
           function(object, annotationHubRoot, webSiteRoot,
                    genomeVersion, dataFileName,experimentMetadata)
           standardGeneric ("createResource"))



#------------------------------------------------------------------------------
setMethod("metadataTable", "EncodeImporter",

    function(object) {
        object@tbl.md
        })

#------------------------------------------------------------------------------
# incoming information
# ====================
# filename: "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz"
# experimentdataDir: "wgEncodeSunyAlbanyGeneSt"
# website.baseUrl: "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/"
# projectPath: "goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt"

setMethod("assembleParams", "EncodeImporter",

   function (object, experimentMetadata, webSiteSourceDirectory,
             annotationHubRoot, projectPath,
             genomeVersion, dataFileName) {


       params <- list()
       params$Species <- "Homo sapiens"
       params$Genome <- genomeVersion

       dataFormat <- experimentMetadata$type
       stopifnot(dataFormat %in% c("broadPeak", "narrowPeak"))

       browser("importer, dataformat");
       
       if(dataFormat == "broadPeak") {
           params$Recipe <- "extendedBedToGRanges"
           params$RecipeArgs <- list(colClasses=list(seqnames="character",
                                                     start="integer",
                                                     end="integer",
                                                     name="character",
                                                     score="integer",
                                                     strand="character",
                                                     signalValue="numeric",
                                                     pValue="numeric",
                                                     qValue="numeric"))
           } # if broadPeak
       if(dataFormat == "narrowPeak") {
           params$Recipe <- "extendedBedToGRanges"
           params$RecipeArgs <- list(colClasses=list(seqnames="character",
                                                     start="integer",
                                                     end="integer",
                                                     name="character",
                                                     score="integer",
                                                     strand="character",
                                                     signalValue="numeric",
                                                     pValue="numeric",
                                                     qValue="numeric",
                                                     peak="integer"))
           } # if narrowPeak
     
     params$RDataClass <- "GRanges"
     params$RDataVersion <- "0.0.1"
     params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
     params$DataProvider <- "hgdownload.cse.ucsc.edu"
     params$Coordinate_1_based <- FALSE
     params$RDataDateAdded <- as.character(Sys.Date())
     params$AnnotationHubRoot <- annotationHubRoot
     params$SourceFile <- file.path(projectPath, dataFileName)
 
     if(nchar(experimentMetadata$tableName) > 0)
         params$Title <- experimentMetadata$tableName
     else
         params$Title <- filename
 
     params$Description <- with(experimentMetadata,
                                {paste (view, type, cell, geoSampleAccession,
                                        tableName, dataType, dccAccession, lab)})
     params$SourceUrl <- file.path(webSiteSourceDirectory, dataFileName)
     params$SourceVersion <- experimentMetadata$labVersion
 
     all.fields <- paste(as.character(experimentMetadata), collapse='@')
     all.fields.clean <- gsub('@+', '|', all.fields, perl=TRUE)
     params$Tags <- all.fields.clean
 
     params
     }) # assembleParams
#-------------------------------------------------------------------------------
setMethod("createResource", "EncodeImporter",

   function (object, annotationHubRoot, webSiteRoot,
             genomeVersion, dataFileName, experimentMetadata) {

       projectName <- experimentMetadata$dataDir
       projectPath <- file.path("goldenpath", genomeVersion, "encodeDCC", projectName)
       localStorageDirectory <- file.path(annotationHubRoot, projectPath)
       webSiteSourceDirectory <- file.path(webSiteRoot, projectPath)
       localFile <- file.path(localStorageDirectory, dataFileName)
   
       if(!file.exists(localStorageDirectory)) {
           message(sprintf(" -- fresh directory creation: %s", localStorageDirectory))
           dir.create(localStorageDirectory, recursive=TRUE)
           }
   
       params <- assembleParams(object, experimentMetadata,
                                webSiteSourceDirectory,
                                annotationHubRoot,
                                projectPath, genomeVersion,
                                dataFileName)


       if(!file.exists (localFile)) {
          message(sprintf("-- fresh download to %s",localFile))
          download.file(params$SourceUrl, destfile=localFile, quiet=TRUE)
          }
       
       md <- do.call(AnnotationHubMetadata, params)
       recipe <- AnnotationHubRecipe(md)
       RDataFilename <- run(recipe)
       postProcessMetadata(annotationHubRoot,  metadata(md)$RDataVersion, metadata(md)$SourceFile)

       localJsonFile <- sub("\\.RData", "\\.json", RDataFilename)
       stopifnot(file.exists(localJsonFile))
       stopifnot(json2mongo(localJsonFile))
       
       RDataFilename
       }) # createResource

#-------------------------------------------------------------------------------
