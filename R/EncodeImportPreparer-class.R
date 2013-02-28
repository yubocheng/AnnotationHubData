setClass("EncodeImportPreparer",
         representation=representation(x="character"),
         contains="ImportPreparer")

#------------------------------------------------------------------------------
ucscHome <- function() return("http://hgdownload.cse.ucsc.edu/")
ucscEncodePath <- function() return("goldenpath/hg19/encodeDCC/")
ucscEncodeTop <- function() return(paste(ucscHome(),
                                         ucscEncodePath(), sep=""))
EncodeBaseURL <- function () return (ucscEncodeTop())
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------
EncodeImportPreparer <- function()
{
   x <- new("EncodeImportPreparer")
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
                    genomeVersion, dataFileName,experimentMetadata,
                    insertIntoDatabase, verbose)
           standardGeneric("createResource"))

setGeneric("parseMetadataFiles", signature="object",
           function(object, metadata.filenames, all.keys, data.file.count)
           standardGeneric("parseMetadataFiles"))

setGeneric("assembleEncodeMetadata", signature="object",
           function(object, verbose=FALSE, test=FALSE) standardGeneric("assembleEncodeMetadata"))

setGeneric("saveMetadata", signature="object",
           function(object, tbl, directory, filename="encodeDCC.metadata.RData")
           standardGeneric("saveMetadata"))

setGeneric("loadMetadata", signature="object",
           function(object, directory, filename="encodeDCC.metadata.RData")
           standardGeneric("loadMetadata"))


#------------------------------------------------------------------------------
setMethod("metadataTable", "EncodeImportPreparer",

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

setMethod("assembleParams", "EncodeImportPreparer",

   function (object, experimentMetadata, webSiteSourceDirectory,
             annotationHubRoot, projectPath,
             genomeVersion, dataFileName) {


       params <- list()
       params$Species <- "Homo sapiens"
       params$Genome <- genomeVersion

       dataFormat <- experimentMetadata$type
       stopifnot(dataFormat %in% c("broadPeak", "narrowPeak", "gtf", "bedRnaElements"))

       if(dataFormat == "gtf")
           params$Recipe = "rtrackLayerImport"

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
     
       if(dataFormat == "bedRnaElements") {
           params$Recipe <- "extendedBedToGRanges"
           params$RecipeArgs <- list(colClasses=list(seqnames="character",
                                                     start="integer",
                                                     end="integer",
                                                     name="character",
                                                     score="integer",
                                                     strand="character",
                                                     level="numeric",
                                                     signif="numeric",
                                                     score2="integer"))
           } # if bedRnaElements
     
     params$RDataClass <- "GRanges"
     params$RDataVersion <- "0.0.1"
     params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
     params$DataProvider <- "hgdownload.cse.ucsc.edu"
     params$Coordinate_1_based <- FALSE
     params$RDataDateAdded <- as.character(Sys.Date())
     params$AnnotationHubRoot <- annotationHubRoot
     params$SourceFile <- file.path(projectPath, dataFileName)
 
        # most files.txt-derived entries have tableName, but
        # wgEncodeCshlLongRnaSeq, for one, does not
        # accomodate this by direct assignment from the
        # supplied dataFileName

     if(nchar(experimentMetadata$tableName) > 0)
         params$Title <- experimentMetadata$tableName
     else
         params$Title <- dataFileName
 
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
setMethod("createResource", "EncodeImportPreparer",

   function (object, annotationHubRoot, webSiteRoot,
             genomeVersion, dataFileName, experimentMetadata,
             insertIntoDatabase=TRUE, verbose=FALSE) {

       projectName <- experimentMetadata$dataDir
       projectPath <- file.path("goldenpath", genomeVersion, "encodeDCC", projectName)
       localStorageDirectory <- file.path(annotationHubRoot, projectPath)
       webSiteSourceDirectory <- file.path(webSiteRoot, projectPath)
       localFile <- file.path(localStorageDirectory, dataFileName)
   
       if(!file.exists(localStorageDirectory)) {
           if(verbose)
               message(sprintf(" -- fresh directory creation: %s", localStorageDirectory))
           dir.create(localStorageDirectory, recursive=TRUE)
           }
   
       params <- assembleParams(object, experimentMetadata,
                                webSiteSourceDirectory,
                                annotationHubRoot,
                                projectPath, genomeVersion,
                                dataFileName)


       if(!file.exists (localFile)) {
          if(verbose)
              message(sprintf("-- fresh download to %s",localFile))
          download.file(params$SourceUrl, destfile=localFile, quiet=TRUE)
          }
       
       md <- do.call(AnnotationHubMetadata, params)
       ## everything below here should be a server maintenance task
       recipe <- AnnotationHubRecipe(md)
       RDataFilename <- run(recipe)
       postProcessMetadata(annotationHubRoot,  metadata(md)$RDataVersion, metadata(md)$SourceFile)

       localJsonFile <- sub("\\.RData", "\\.json", RDataFilename)
       stopifnot(file.exists(localJsonFile))
       if(insertIntoDatabase)
           stopifnot(json2mongo(localJsonFile))
       
       RDataFilename
       }) # createResource

#-------------------------------------------------------------------------------
.downloadFileInfo <- function(baseUrl, subdirs, destinationDir, verbose=FALSE)
{
  for(subdir in subdirs){
     #printf('--- %s', subdir)
     url <- file.path(baseUrl, subdir, "files.txt")
     subdir.stripped <- gsub("/", "", subdir)
     destination <- file.path(destinationDir, sprintf("%s.info", subdir.stripped))
     if(verbose)
        printf ("%s -> %s", url, destination)
     download.file(url, destination, quiet=!verbose)
     }

} # .downloadFileInfo
#--------------------------------------------------------------------------------
.extractLinksFromHtmlLines <- function(htmlLines)
{
        # some lines (at least) look like this:
        #  <a href=\"wgEncodeAffyRnaChip/\">wgEncodeAffyRnaChip/</a>
        #     05-Jul-2012 06:57    -   "
        # match from start of quoted string up to the escaped closing quote
        # will eliminate the trailing slash ("Chip/") below

    hrefLines <- grep("a href", htmlLines, ignore.case=TRUE, value=TRUE)
    matches <- gregexpr("<a href=\"(.*?)\">", hrefLines, perl=TRUE)

    result <- vector('character', length(hrefLines))
    for(i in 1:length(hrefLines)){
      match <- matches [[i]]
      start <- attr(match, "capture.start")
      length <- attr(match, "capture.length")
         # subtract 1 for basic arithmetic, and 1 to eliminate trailing / in the href link text
      end <- start + length - 1
      result[i] <- substr(hrefLines[i], start, end)
      } # for i

    result


} # .extractLinksFromHtmlLines
#-------------------------------------------------------------------------------
.extractExperimentDirectoriesFromWebPage <- function(encodeSummaryWebPageURL)
{
    lines <- strsplit(getURL(encodeSummaryWebPageURL), "\n")[[1]]
    lines <- lines[grep("href", lines, ignore.case=TRUE)]
    removers <- grep ("Parent Directory", lines, ignore.case=TRUE)
    if(length(removers) > 0)
      lines <- lines[-removers]

    .extractLinksFromHtmlLines(lines)

} # .extractExperimentDirectoriesFromWebPage
#-------------------------------------------------------------------------------
.retrieveEncodeDCCMetadataFiles <- function(destinationDir, max=NA, verbose=TRUE)
{
    all.dirs <- .extractExperimentDirectoriesFromWebPage(EncodeBaseURL())

        # some hard-coded knowledge.  there is no files.txt (no metadata)
        # in http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/referenceSequences/
    skip.these.dirs <- grep("referenceSequences", all.dirs)

    if(length(skip.these.dirs) > 0)
        all.dirs <- all.dirs[-skip.these.dirs]

    if(!is.na(max))  # for testing only
        all.dirs <- all.dirs[1:max]

    .downloadFileInfo(EncodeBaseURL(),all.dirs,
                      destinationDir, verbose=verbose)


} # .retrieveEncodeDCCMetadataFiles
#-------------------------------------------------------------------------------
.learnAllEncodeMetadataCategories <- function(metadataFilesDirectory,
                                              verbose=FALSE)
{
        # get a list of all metadataFiles, each titled "file.info",
        # each having as many lines as there are data files in
        # the encodeDCC directory from which file.info was read
  
    files <- list.files(metadataFilesDirectory)

    all.keys <- c()

    total.lines <- 0
    
    for(file in files){
       full.path <- file.path(metadataFilesDirectory, file)
          # one line per data file, each separately characterized in this
          # metadata file
       lines <- scan(full.path, what=character(0), sep="\n",
                     quiet=TRUE)
       total.lines <- total.lines + length(lines)
          # first split on tab character, separating filename from info
       tokens.0 <- strsplit(lines, "\t")
       data.filenames <- sapply(tokens.0, "[", 1)
       info.strings <- sapply(tokens.0, "[", 2)
       keyValuePairSets <- strsplit(info.strings, "; ")
       stopifnot(length(data.filenames) == length(keyValuePairSets))
       for(i in 1:length(keyValuePairSets)) {
           pairs <- strsplit(keyValuePairSets[[i]], "=")
           keys <- sapply(pairs, "[", 1)
           new.unique.keys <- setdiff(keys, all.keys)
           if(verbose)
               if (length(new.unique.keys) > 0)
                   printf("%30s, new.unique.keys: %d", file, length(new.unique.keys))
           all.keys <- unique(c(all.keys, keys))
           } # for i
       if(verbose)
            printf("%30s  new keys %d  currentTotal: %d", file,
                   length(keys), length(all.keys))
        } # file

    list(all.keys=sort(all.keys), total.lines=total.lines)

} # .learnAllEncodeMetadataCategories
#-------------------------------------------------------------------------------
setMethod("parseMetadataFiles", "EncodeImportPreparer",

   function (object, metadata.filenames, all.keys, data.file.count) {

        # our convention is that encodeDCC metadata files, though starting
        # out life as "files.txt", is downloaded and renamed xxxx.info,
        # where "xxxx" is the name of the encodeDCC/xxxx directory
        # from which the files.txt file was obtained.
        # it is always possible that other files could be found in
        # the download directory; filter them out here:
    metadata.filenames <- grep(".info$", metadata.filenames, value=TRUE)
    tbl <- data.frame()
    row.template <-vector("character", length(all.keys))
    names(row.template) <- all.keys
    all.data.filenames <- c()

    for(filename in metadata.filenames) {
        if(!file.exists(filename)){
            msg <- sprintf("parseMetadataFiles, %s does not exist", sQuote(filename))
            stop(msg)
            }
        stopifnot(file.exists(filename))
        lines <- scan(filename, what=character(0), sep="\n", quiet=TRUE)
            # first split on tab character, separating filename from info
        tokens.0 <- strsplit(lines, "\t")
        data.filenames <- sapply(tokens.0, "[", 1)
        info.strings <- sapply(tokens.0, "[", 2)
        keyValuePairSets <- strsplit(info.strings, "; ")
        stopifnot(length(data.filenames) == length(keyValuePairSets))
        for(i in 1:length(keyValuePairSets)) {
            pairs <- strsplit(keyValuePairSets[[i]], "=")
            keys <- sapply(pairs, "[", 1)
            new.row <- sapply(pairs, "[", 2)
            names(new.row) <- keys
            new.row.full <- row.template
            new.row.full[names(new.row)] <- as.character(new.row)
            new.row.as.df <- t(data.frame(new.row.full))
            tbl <- rbind(tbl, new.row.as.df)
            }# for i
        all.data.filenames <- c(all.data.filenames, data.filenames)
        } # for filename
        
    rownames(tbl) <- all.data.filenames
    tbl
    }) # parseMetadataFiles
#-------------------------------------------------------------------------------
setMethod("assembleEncodeMetadata", "EncodeImportPreparer",

   function (object, verbose=FALSE, test=FALSE) {
       remote.directory.names <-.extractExperimentDirectoriesFromWebPage(EncodeBaseURL()) 
       avoid <- grep("referenceSequences", remote.directory.names)
       if(length(avoid) > 0)
           remote.directory.names <- remote.directory.names[-avoid]

       local.destinationDir <- tempdir()
       if(test)
           remote.directory.names <- remote.directory.names[1:2]
       .downloadFileInfo(EncodeBaseURL(), remote.directory.names,
                         local.destinationDir, verbose=verbose)
       info <- .learnAllEncodeMetadataCategories(local.destinationDir)
       all.files <- file.path(local.destinationDir, dir(local.destinationDir))
       parseMetadataFiles(object, all.files, info$all.keys, info$total.lines)
     })
#-------------------------------------------------------------------------------
setMethod("saveMetadata", "EncodeImportPreparer",

    function(object, tbl, directory, filename="encodeDCC.metadata.RData") {
        full.path <- file.path(directory, filename)
        save(tbl, file=full.path)
        })
#-------------------------------------------------------------------------------
setMethod("loadMetadata", "EncodeImportPreparer",

    function(object, directory, filename="encodeDCC.metadata.RData") {
        full.path <- file.path(directory, filename)
        load(file=full.path)
        tbl
        })
#-------------------------------------------------------------------------------
