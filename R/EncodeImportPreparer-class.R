setClass("EncodeImportPreparer",
         representation=representation(annotationHubRoot="character",
                                       tbl.md="data.frame",
                                       ahmd="list"),
         contains="ImportPreparer")

#------------------------------------------------------------------------------
ucscHome <- function() return("http://hgdownload.cse.ucsc.edu/")
ucscEncodePath <- function() return("goldenpath/hg19/encodeDCC/")
ucscEncodeTop <- function() return(paste(ucscHome(),
                                         ucscEncodePath(), sep=""))
EncodeBaseURL <- function () return (ucscEncodeTop())
printf <- function(...) print(noquote(sprintf(...)))
PARSED_METADATA_FILENAME <- "tbl.parsedEncodeMetadata.RData"
#------------------------------------------------------------------------------
EncodeImportPreparer <- function(annotationHubRoot, tbl.md=NULL)
{
    if(is.null(tbl.md)) {
        full.path <- file.path(annotationHubRoot, PARSED_METADATA_FILENAME)
        if(!file.exists(full.path))
            tbl.md <- data.frame()
        else {  # null tbl.md, file found, try to load
            tbl.name <- load(full.path)
            stopifnot(tbl.name == "tbl.md")
            }
        } # if is.null(tbl.md)
   
    stopifnot(is(tbl.md, "data.frame"))
   
    if(nrow(tbl.md) > 0)
        tbl.md <- subset(tbl.md, type %in%
                         c("narrowPeak", "broadPeak", "bedRnaElements", "gtf"))
    
    x <- new("EncodeImportPreparer", annotationHubRoot=annotationHubRoot,
             tbl.md=tbl.md)
    #x@ahmd <- encodeMetadataToAnnotationHubMetadata(x, annotationHubRoot)
   
    x
   
} # constructor
#------------------------------------------------------------------------------
setGeneric("metadataTable", signature="object",
           function(object)
           standardGeneric ("metadataTable"))

setGeneric("encodeMetadataToAnnotationHubMetadata", signature="object",
   function (object, subset=NA, verbose=FALSE)
           standardGeneric ("encodeMetadataToAnnotationHubMetadata"))


setGeneric("createResource", signature="object",
           function(object, annotationHubRoot, webSiteRoot,
                    genomeVersion, dataFileName,experimentMetadata,
                    insertIntoDatabase, verbose)
           standardGeneric("createResource"))


setGeneric("parseMetadataFiles", signature="object",
           function(object, metadata.filenames,dataFile.summary, verbose=FALSE)
           standardGeneric("parseMetadataFiles"))

setGeneric("saveMetadata", signature="object",
           function(object, tbl, directory, filename="encodeDCC.metadata.RData")
           standardGeneric("saveMetadata"))

setGeneric("loadMetadata", signature="object",
           function(object, directory, filename="encodeDCC.metadata.RData")
           standardGeneric("loadMetadata"))

setGeneric("annotationHubRoot", signature="object",
           function(object)
           standardGeneric("annotationHubRoot"))
#------------------------------------------------------------------------------
setMethod("annotationHubRoot", "EncodeImportPreparer",

    function(object) {
        object@annotationHubRoot
      })

#------------------------------------------------------------------------------
setMethod("newResources", "EncodeImportPreparer",

    function(importPreparer, currentMetadata=list(), ...) {


        ahmd.list <- encodeMetadataToAnnotationHubMetadata(importPreparer,
                                                           subset=NA,
                                                           verbose=FALSE)
        previous.urls  <- sapply(currentMetadata, function(elt) metadata(elt)$SourceUrl)
        all.known.urls <- sapply(importPreparer@ahmd, function(elt) metadata(elt)$SourceUrl)
        new.urls <- setdiff(all.known.urls, previous.urls)
        
        if(length(new.urls) == 0)
            return(list())

        if(length(new.urls) == 1)
            if (is.na(new.urls))
                return(list())
        
        indices <- match(new.urls, all.known.urls)
        importPreparer@ahmd[indices]
        })


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

setMethod("encodeMetadataToAnnotationHubMetadata", "EncodeImportPreparer",

   function (object, subset=NA, verbose=FALSE){

       annotationHubRoot <- annotationHubRoot(object)
       if(length(subset) == 1 && is.na(subset))
           tbl.enc.md <- object@tbl.md
       else
           tbl.enc.md <- object@tbl.md[subset,]
       
       xlate <- function(annotationHubRoot, filename, encode.metadata.list ){
           with(encode.metadata.list, {
              if(verbose)
                  printf("%s: %s", filename, type)
              recipe.info <- .assignRecipeAndArgs(type)
              sourceFile <- file.path("goldenpath/hg19/encodeDCC", remoteDirectory,
                                      filename)
              sourceUrl <- paste(EncodeBaseURL(), remoteDirectory, filename, sep="/")
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
       object@ahmd <- lapply(seq_len(nrow(tbl.enc.md)),
              function(index) xlate(annotationHubRoot, rownames(tbl.enc.md)[index],
                                    as.list(tbl.enc.md[index,])))
       object
       }) # encodeMetadataToAnnotationHubMetadata

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
.downloadFileInfo <- function(baseUrl, subdirs, destinationDir, verbose=FALSE)
{
  for(subdir in subdirs){
     if(verbose)
         printf('downloading from %s: %s', baseUrl, subdir)
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
.convertSizeStringsToNumeric <- function(sizeStrings)
{
    units.present <- grepl("[KMG]",sizeStrings)
    has.units <- which(units.present)
    no.units <- which(!units.present)
    result <- integer(length=length(sizeStrings))
    
    result[no.units] <- as.integer(sizeStrings[no.units])

    k.units <- which(grepl("K", sizeStrings))
    if (length(k.units) > 0){
        k.strings <- sizeStrings[k.units]
        k.values <- 1000 * as.numeric(substring(k.strings, 1, nchar(k.strings)-1))
        result[k.units] <- k.values
        }

    m.units <- which(grepl("M", sizeStrings))
  
    if (length(m.units) > 0){
        m.strings <- sizeStrings[m.units]
        m.values <- 1000000 * as.numeric(substring(m.strings, 1, nchar(m.strings)-1))
        result[m.units] <- m.values
        }

    g.units <- which(grepl("G", sizeStrings))
    if (length(g.units) > 0){
        g.strings <- sizeStrings[g.units]
        g.values <- 1000000000 * as.numeric(substring(g.strings, 1, nchar(g.strings)-1))
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

   function (object, metadata.filenames, dataFile.summary,
             verbose=FALSE) {

        # our convention is that encodeDCC metadata files, though starting
        # out life as "files.txt", are downloaded locally, and renamed xxxx.info,
        # where "xxxx" is the name of the encodeDCC/xxxx directory
        # from which the files.txt file was obtained.
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
    all.data.filenames <- vector("character", length=dataFile.summary$total.lines)

    data.file.count <- 0

    for(metadata.filename in metadata.filenames) {
        if(!file.exists(metadata.filename)){
            msg <- sprintf("parseMetadataFiles, %s does not exist", sQuote(metadata.filename))
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
        if(verbose) message(sprintf("parsing metadata file %s", sQuote(metadata.filename)))
        lines <- scan(metadata.filename, what=character(0), sep="\n", quiet=TRUE)
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
            #browser("keySets")    
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
        filename = sprintf ('tbl.md.%s.RData', format (Sys.time(), "%a.%b.%d.%Y-%H:%M:%S"))
        #save(tbl, file=filename)
        #printf("saved %d rows to %s", nrow(tbl), filename)
        } # for filename
    rownames(tbl) <- all.data.filenames
    tbl <- as.data.frame(tbl)
    tbl$size <- .convertSizeStringsToNumeric(tbl$size)
    invisible(tbl)
    }) # parseMetadataFiles

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
