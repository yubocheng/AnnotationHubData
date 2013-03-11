setClass("EncodeImportPreparer",
         representation=representation(annotationHubRoot="character",
                                       tbl.md="data.frame",
                                       ahmd.list="list"),
         contains="ImportPreparer")

#------------------------------------------------------------------------------
setValidity("EncodeImportPreparer", function(object) {
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
setMethod("show", "EncodeImportPreparer",

    function(object) {
        msg = sprintf("EncodeImportPreparer object with")
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
ucscHome <- function() return("http://hgdownload.cse.ucsc.edu/")
ucscEncodePath <- function() return("goldenpath/hg19/encodeDCC/")
ucscEncodeTop <- function() return(paste(ucscHome(),
                                         ucscEncodePath(), sep=""))
EncodeBaseURL <- function () return (ucscEncodeTop())
#------------------------------------------------------------------------------
# general utility functions not yet generally available:
.printf <- function(...) print(noquote(sprintf(...)))
.url.exists <- function(url){HEAD(url)$headers$status == "200"}
#------------------------------------------------------------------------------
EncodeImportPreparer <- function(annotationHubRoot, tbl.md=NULL, verbose=FALSE,
                                 maxForTesting=NA)
{
       # encodeDCC metadata files are downloaded, parsed, and converted to
       # a data.frame, in this directory:
  
    downloadDir <- tempdir()
    currentContents <- list.files(downloadDir)

        # tempdir apparently has the same value across repeated calls
        # made within the same process -- such as in testing. we want
        # a clean slate, to delete these downloaded files if found
    
    if(length(currentContents) > 0){
        if(verbose) .printf("removing %d files from tempdir %s",
                           length(currentContents),
                           downloadDir)
    
        unlink(file.path(downloadDir, currentContents))
        }

   
    if(!is.na(maxForTesting) & maxForTesting <= 0){
        return(new("EncodeImportPreparer", annotationHubRoot=annotationHubRoot,
                   tbl.md=data.frame(), ahmd.list=list()))
        }
        
    
    .retrieveEncodeDCCMetadataFiles(downloadDir,verbose=verbose,
                                    maxForTesting=maxForTesting)
    dataFile.summary <- .learnAllEncodeMetadataCategories(downloadDir,
                                                          verbose=verbose)
    metadata.files <- grep("\\.info$", dir(downloadDir), value=TRUE)
    full.paths <- file.path(downloadDir, metadata.files)
    tbl.md <- .parseMetadataFiles(full.paths, dataFile.summary, verbose=verbose)
    tbl.md <- subset(tbl.md, type %in%
                      c("narrowPeak", "broadPeak", "bedRnaElements", "gtf"))
    tbl.md <- subset(tbl.md, size < 100000000)
    ahmd.list <- .encodeMetadataToAnnotationHubMetadata(tbl.md,
                                                        annotationHubRoot,
                                                        verbose)
    self <- new("EncodeImportPreparer", annotationHubRoot=annotationHubRoot,
                tbl.md=tbl.md, ahmd.list=ahmd.list)
   
    self
   
} # constructor
#------------------------------------------------------------------------------
setGeneric("metadataList", signature="object",
           function(object)
           standardGeneric ("metadataList"))

setGeneric("metadataTable", signature="object",
           function(object)
           standardGeneric ("metadataTable"))

setGeneric("annotationHubRoot", signature="object",
           function(object)
           standardGeneric("annotationHubRoot"))

setGeneric("sourceUrls", signature="object",
           function(object)
           standardGeneric("sourceUrls"))
#------------------------------------------------------------------------------
setMethod("annotationHubRoot", "EncodeImportPreparer",

    function(object) {
        object@annotationHubRoot
      })

#------------------------------------------------------------------------------
setMethod("metadataList", "EncodeImportPreparer",

    function(object) {
        object@ahmd.list
        })

#------------------------------------------------------------------------------
setMethod("metadataTable", "EncodeImportPreparer",

    function(object) {
        object@tbl.md
        })

#------------------------------------------------------------------------------
setMethod("sourceUrls", "EncodeImportPreparer",

    function(object) {
        sapply(object@ahmd.list, function(elt) metadata(elt)$SourceUrl)
        })

#------------------------------------------------------------------------------
setMethod("newResources", "EncodeImportPreparer",

    function(importPreparer, currentMetadata=list(), ...) {


        ahmd.list <- .encodeMetadataToAnnotationHubMetadata(
                          metadataTable(importPreparer),
                          annotationHubRoot(importPreparer),
                          verbose=FALSE)
        previous.urls  <- sapply(currentMetadata,
                                 function(elt) metadata(elt)$SourceUrl)
        all.known.urls <- sapply(importPreparer@ahmd.list,
                                 function(elt) metadata(elt)$SourceUrl)
        new.urls <- setdiff(all.known.urls, previous.urls)
        
        if(length(new.urls) == 0)
            return(list())

        if(length(new.urls) == 1)
            if (is.na(new.urls))
                return(list())
        
        indices <- match(new.urls, all.known.urls)
        importPreparer@ahmd.list[indices]
        })


#------------------------------------------------------------------------------
.encodeMetadataToAnnotationHubMetadata <- function (tbl.md, annotationHubRoot,
                                                    verbose=FALSE)
{
    xlate <- function(annotationHubRoot, filename, encode.metadata.list){
        with(encode.metadata.list, {
           if(verbose)
               .printf("%s: %s", filename, type)
           recipe.info <- AnnotationHubData:::.assignRecipeAndArgs(type)
           sourceFile <- file.path("goldenpath/hg19/encodeDCC", remoteDirectory,
                                   filename)
           sourceUrl <- paste(EncodeBaseURL(), remoteDirectory, filename,
                              sep="/")
           if (!url.exists(sourceUrl)) {
               #browser("exists")
               return(NA)
               }
           tags <- as.character(encode.metadata.list)
               # remove empty fields
           tags <- tags[nchar(tags) > 0]
           AnnotationHubMetadata(AnnotationHubRoot=annotationHubRoot,
                                 SourceFile=sourceFile,
                                 SourceSize=size,
                                 SourceUrl=sourceUrl,
                                 SourceVersion=dataVersion,
                                 DataProvider="EncodeDCC",
                                 Title=tableName,
                                 Description=tableName,
                                 Species="Homo sapiens",
                                 TaxonomyId="9606",
                                 Genome="hg19",
                                 Tags=tags,
                                 Recipe=recipe.info$recipe,
                                 RecipeArgs=recipe.info$recipeArgs,
                                 RDataClass="GRanges",
                                 RDataVersion=numeric_version("0.0.1"),
                                 RDataDateAdded=as.Date(Sys.Date(), "%Y"),
                                 Maintainer="Paul Shannon <pshannon@fhcrc.org>",
                                 Coordinate_1_based=TRUE,
                                 Notes=NA_character_)
             }) # with encode.metadata.list
        } # xlate
    ahmd.list <- lapply(seq_len(nrow(tbl.md)),
           function(index) xlate(annotationHubRoot, rownames(tbl.md)[index],
                                 as.list(tbl.md[index,])))
    #not.legit <- function (x) is.logical(x) && is.na(x)
    #Filter(Negate(not.legit), ahmd.list)
    Filter(function(x) is(x, "AnnotationHubMetadata"), ahmd.list) # or use NULL & is.null instead of NA


} # .encodeMetadataToAnnotationHubMetadata
#-------------------------------------------------------------------------------
.assignRecipeAndArgs <- function(dataFormat)
{
    recipe <- NULL
    recipeArgs <- list()
  
    dataFormat <- tolower(dataFormat)
    if(dataFormat == "gtf")
        recipe = "rtrackLayerImport"
  
    if(dataFormat == "broadpeak") {
        recipe <- "extendedBedToGRanges"
        recipeArgs <- list(colClasses=list(seqnames="character",
                                                  start="integer",
                                                  end="integer",
                                                  name="character",
                                                  score="integer",
                                                  strand="character",
                                                  signalValue="numeric",
                                                  pValue="numeric",
                                                  qValue="numeric"))
        } # if broadPeak
    if(dataFormat == "narrowpeak") {
        recipe <- "extendedBedToGRanges"
        recipeArgs <- list(colClasses=list(seqnames="character",
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
  
    if(dataFormat == "bedrnaelements") {
        recipe <- "extendedBedToGRanges"
        recipeArgs <- list(colClasses=list(seqnames="character",
                                                  start="integer",
                                                  end="integer",
                                                  name="character",
                                                  score="integer",
                                                  strand="character",
                                                  level="numeric",
                                                  signif="numeric",
                                                  score2="integer"))
        } # if bedRnaElements
  
   list(recipe=recipe, recipeArgs=recipeArgs)

} # .assignRecipeAndArgs
#-------------------------------------------------------------------------------
.mapCompositeFieldToDirectory <- function(composite)
{
    result <- composite
    if(composite=="wgEncodeAwgDnaseUniPk")
        result <- "wgEncodeAwgDnaseUniform"

    result


} # .mapCompositeFieldToDirectory 
#-------------------------------------------------------------------------------
.downloadFileInfo <- function(baseUrl, subdirs, downloadDir, verbose=FALSE)
{
  for(subdir in subdirs){
     if(verbose)
         .printf("downloading from %s: %s", baseUrl, subdir)
     url <- file.path(baseUrl, subdir, "files.txt")
     subdir.stripped <- gsub("/", "", subdir)
     destination <- file.path(downloadDir, sprintf("%s.info", subdir.stripped))
     if(verbose)
         .printf ("%s -> %s", url, destination)
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

    result <- vector("character", length(hrefLines))
    for(i in 1:length(hrefLines)){
      match <- matches [[i]]
      start <- attr(match, "capture.start")
      length <- attr(match, "capture.length")
         # subtract 1 for basic arithmetic, and 1 to eliminate
         # trailing / in the href link text
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
.retrieveEncodeDCCMetadataFiles <- function(downloadDir, verbose=TRUE,
                                            maxForTesting=NA)
{
    all.dirs <- .extractExperimentDirectoriesFromWebPage(EncodeBaseURL())

        # some hard-coded knowledge.  there is no files.txt (no metadata)
        # in http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/\
        #      referenceSequences/
    skip.these.dirs <- grep("referenceSequences", all.dirs)

    if(length(skip.these.dirs) > 0)
        all.dirs <- all.dirs[-skip.these.dirs]

    if(!is.na(maxForTesting)){
        all.dirs <- all.dirs[1:maxForTesting]
        }

    .downloadFileInfo(EncodeBaseURL(),all.dirs,
                      downloadDir, verbose=verbose)


} # .retrieveEncodeDCCMetadataFiles
#-------------------------------------------------------------------------------
.convertSizeStringsToNumeric <- function(sizeStrings)
{
    units.present <- grepl("[KMG]",sizeStrings)
    has.units <- which(units.present)
    no.units <- which(!units.present)
    result <- integer(length=length(sizeStrings))
    
    result[no.units] <- as.integer(sizeStrings[no.units])

    k.units <- which(grepl("K", sizeStrings))
    if (length(k.units) > 0){
        k.strings <- as.character(sizeStrings[k.units])
        #.printf("k.strings: %s", paste(k.strings, collapse=","))
        k.values <- 1000 * as.numeric(substring(k.strings, 1,
                                                nchar(k.strings)-1))
        result[k.units] <- k.values
        }

    m.units <- which(grepl("M", sizeStrings))
  
    if (length(m.units) > 0){
        m.strings <- sizeStrings[m.units]
        m.values <- 1000000 *
            as.numeric(substring(m.strings, 1, nchar(m.strings)-1))
        result[m.units] <- m.values
        }

    g.units <- which(grepl("G", sizeStrings))
    if (length(g.units) > 0){
        g.strings <- sizeStrings[g.units]
        g.values <- 1000000000 *
             as.numeric(substring(g.strings, 1, nchar(g.strings)-1))
        result[g.units] <- g.values
        }# if

    result

} # .convertSizeStringsToNumeric
#-------------------------------------------------------------------------------
.learnAllEncodeMetadataCategories <- function(metadataFilesDirectory,
                                              verbose=FALSE)
{
        # get a list of all metadataFiles, each titled "XXXX.info",
        # each having as many lines as there are data files in
        # the encodeDCC directory from which file.info was read
  
    files <- list.files(metadataFilesDirectory)
    files <- grep("\\.info$", files, value=TRUE)

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
                   .printf("%30s, new.unique.keys: %d", file,
                          length(new.unique.keys))
           all.keys <- unique(c(all.keys, keys))
           } # for i
       if(verbose)
            .printf("%30s  new keys %d  currentTotal: %d", file,
                   length(keys), length(all.keys))
        } # file

    list(all.keys=sort(all.keys), total.lines=total.lines)

} # .learnAllEncodeMetadataCategories
#-------------------------------------------------------------------------------
.parseMetadataFiles <- function (metadata.filenames, dataFile.summary,
                                 verbose=FALSE)
{

        # our convention is that encodeDCC metadata files, though starting
        # out life as "files.txt", are downloaded locally, and
        # renamed xxxx.info,  where "xxxx" is the name of the
        # encodeDCC/xxxx directory from which the files.txt file was obtained.
        # it is always possible that other files could be found in
        # the local download directory; filter them out here:

    
    metadata.filenames <- grep(".info$", metadata.filenames, value=TRUE)

        # when pre-allocating the character matrix, 
        # add a column for "remoteDirectory".  this is the only metadata which
        # we construct (the only one not taken from encodeDCC)
        # their composite field is almost reliable for this, but
        # (for instance) composite= wgEncodeAwgDnaseUniPk but
        # directory = wgEncodeAwgDnaseUniform/

    all.columns <- c(dataFile.summary$all.keys, "remoteDirectory")

    tbl <- matrix(data="", nrow=dataFile.summary$total.lines,
                           ncol=length(all.columns))
    colnames(tbl) <- all.columns

        # create a reusable named character vector into which encodeDCC
        # metadata is parsed.  

    row.template <-vector("character", length(all.columns))
    names(row.template) <- all.columns

        # we do not know the names of the data files when we start.
        # pre-allocate a list to collect these as we loop through the
        # metdata files in which they are named and described
    all.data.filenames <- vector("character",
                                 length=dataFile.summary$total.lines)

    data.file.count <- 0

    for(metadata.filename in metadata.filenames) {
        if(!file.exists(metadata.filename)){
            msg <- sprintf("parseMetadataFiles, %s does not exist",
                           sQuote(metadata.filename))
            stop(msg)
            }
           # deterimine the remote directory.  
           # our convention (visible in .downloadFileInfo)  is to rename
           # each of the many "files.txt" into <directory>.info
           # where <directory> is the immediate container of the actual
           # encode data files.  here we re-extract that directory name from
           # the metadata.filename.
        tokens <- strsplit(metadata.filename, "/")[[1]]
        short.filename <- tokens[length(tokens)]
        stopifnot(grep(".info", short.filename, fixed=TRUE)==1)
        remote.directory.name <- sub(".info", "", short.filename)
        if(verbose) message(sprintf("parsing metadata file %s",
                                    sQuote(metadata.filename)))
        lines <- scan(metadata.filename, what=character(0), sep="\n",
                                    quiet=TRUE)
            # first split on tab character, separating filename from info
        tokens.0 <- strsplit(lines, "\t")
        data.filenames <- sapply(tokens.0, "[", 1)
            # one info.string per described data file, each consisting of a
            # semi-colon-separated list of key=value pairs
        info.strings <- sapply(tokens.0, "[", 2)
        keyValuePairSets <- strsplit(info.strings, "; ")
        keySets.count <- length(keyValuePairSets)
        stopifnot(length(data.filenames) == keySets.count)
        for(i in 1:keySets.count){
            data.file.count <- data.file.count + 1
            pairs <- strsplit(keyValuePairSets[[i]], "=")
            keys <- sapply(pairs, "[", 1)
            new.row <- sapply(pairs, "[", 2)
            names(new.row) <- keys
            new.row.full <- row.template
            new.row.full[names(new.row)] <- as.character(new.row)
            new.row.full["remoteDirectory"] <- remote.directory.name
            tbl[data.file.count,names(new.row.full)] <- new.row.full
            all.data.filenames[data.file.count] <- data.filenames[i]
            }# for i
        } # for filename
    rownames(tbl) <- all.data.filenames
    tbl <- as.data.frame(tbl, stringsAsFactors=FALSE)
    tbl$size <- .convertSizeStringsToNumeric(tbl$size)
    invisible(tbl)
    } # .parseMetadataFiles

#-------------------------------------------------------------------------------
