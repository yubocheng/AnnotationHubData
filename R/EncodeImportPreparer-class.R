setClass("EncodeImportPreparer",
         representation=representation(tbl.md="data.frame"),
         contains="ImportPreparer")

#------------------------------------------------------------------------------
ucscHome <- function() return("http://hgdownload.cse.ucsc.edu/")
ucscEncodePath <- function() return("goldenpath/hg19/encodeDCC/")
ucscEncodeTop <- function() return(paste(ucscHome(),
                                         ucscEncodePath(), sep=""))
EncodeBaseURL <- function () return (ucscEncodeTop())
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------
EncodeImportPreparer <- function(tbl.md=data.frame())
{
   x <- new("EncodeImportPreparer", tbl.md=tbl.md)
   x
}
#------------------------------------------------------------------------------
setGeneric("metadataTable", signature="object",
           function(object)
           standardGeneric ("metadataTable"))

setGeneric("encodeMetadataToAnnotationHubMetadata", signature="object",
   function (object, annotationHubRoot, subset=NA, verbose=FALSE)
           standardGeneric ("encodeMetadataToAnnotationHubMetadata"))


setGeneric("createResource", signature="object",
           function(object, annotationHubRoot, webSiteRoot,
                    genomeVersion, dataFileName,experimentMetadata,
                    insertIntoDatabase, verbose)
           standardGeneric("createResource"))

setGeneric("parseMetadataFiles", signature="object",
           function(object, metadata.filenames, all.keys, data.file.count, verbose=FALSE)
           standardGeneric("parseMetadataFiles"))

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

setMethod("encodeMetadataToAnnotationHubMetadata", "EncodeImportPreparer",

   function (object, annotationHubRoot, subset=NA, verbose=FALSE){

       if(length(subset) == 1 && is.na(subset))
           tbl.enc.md <- object@tbl.md
       else
           tbl.enc.md <- object@tbl.md[subset,]
       
       xlate <- function(annotationHubRoot, filename, encode.metadata.list ){
           with(encode.metadata.list, {
              if(verbose)
                 print(noquote(sprintf("%s: %s", filename, type)))
              recipe.info <- .assignRecipeAndArgs(type)
              sourceFile <- file.path("goldenpath/hg19/encodeDCC", remoteDirectory,
                                      filename)
              sourceUrl <- paste(EncodeBaseURL(), remoteDirectory, filename, sep="/")
              tags <- as.character(encode.metadata.list)
                  # remove empty fields
              tags <- tags[nchar(tags) > 0]
              AnnotationHubMetadata(AnnotationHubRoot=annotationHubRoot,
                                    SourceFile=sourceFile,
                                    SourceUrl=sourceUrl,
                                    SourceVersion=dataVersion,
                                    DataProvider="EncodeDCC",
                                    Title=tableName,
                                    Description=tableName,
                                    Species="Homo sapiens",
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
       lapply(1:nrow(tbl.enc.md),
              function(index) xlate(annotationHubRoot, rownames(tbl.enc.md)[index],
                                    as.list(tbl.enc.md[index,])))
       }) # encodeMetadataToAnnotationHubMetadata
                               


  # AnnotationHubRoot=NA_character_,
  # BiocVersion=biocVersion(),
  # Coordinate_1_based=NA,
  # DataProvider=NA_character_,
  # DerivedMd5=NA_character_,
  # Description=NA_character_,
  # Genome=NA_character_,
  # Maintainer=
  # "Bioconductor Package Maintainer <maintainer@bioconductor.org>",
  # Notes=NA_character_,
  # RDataClass=NA_character_,
  # RDataDateAdded=as.POSIXct(NA_character_),
  # RDataLastModifiedDate=as.POSIXct(NA_character_),
  # RDataPath=NA_character_,
  # RDataSize=NA_integer_,
  # RDataVersion=.NA_version_,
  # Recipe=NA_character_,
  # RecipeArgs=list(),
  # SourceFile=NA_character_,
  # SourceLastModifiedDate=as.POSIXct(NA_character_),
  # SourceMd5=NA_character_,
  # SourceSize=NA_integer_,
  # SourceVersion=NA_character_,
  # Species=NA_character_,
  # Tags=NA_character_,
  # TaxonomyId=NA_character_,
  # Title=NA_character_

#
#   
#     
#       params <- list()
#       params$Species <- "Homo sapiens"
#       params$Genome <- genomeVersion
#
#       dataFormat <- experimentMetadata$type
#       stopifnot(dataFormat %in% c("broadPeak", "narrowPeak", "gtf", "bedRnaElements"))
#
#       if(dataFormat == "gtf")
#           params$Recipe = "rtrackLayerImport"
#
#       if(dataFormat == "broadPeak") {
#           params$Recipe <- "extendedBedToGRanges"
#           params$RecipeArgs <- list(colClasses=list(seqnames="character",
#                                                     start="integer",
#                                                     end="integer",
#                                                     name="character",
#                                                     score="integer",
#                                                     strand="character",
#                                                     signalValue="numeric",
#                                                     pValue="numeric",
#                                                     qValue="numeric"))
#           } # if broadPeak
#       if(dataFormat == "narrowPeak") {
#           params$Recipe <- "extendedBedToGRanges"
#           params$RecipeArgs <- list(colClasses=list(seqnames="character",
#                                                     start="integer",
#                                                     end="integer",
#                                                     name="character",
#                                                     score="integer",
#                                                     strand="character",
#                                                     signalValue="numeric",
#                                                     pValue="numeric",
#                                                     qValue="numeric",
#                                                     peak="integer"))
#           } # if narrowPeak
#     
#       if(dataFormat == "bedRnaElements") {
#           params$Recipe <- "extendedBedToGRanges"
#           params$RecipeArgs <- list(colClasses=list(seqnames="character",
#                                                     start="integer",
#                                                     end="integer",
#                                                     name="character",
#                                                     score="integer",
#                                                     strand="character",
#                                                     level="numeric",
#                                                     signif="numeric",
#                                                     score2="integer"))
#           } # if bedRnaElements
#     
#     params$RDataClass <- "GRanges"
#     params$RDataVersion <- "0.0.1"
#     params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
#     params$DataProvider <- "hgdownload.cse.ucsc.edu"
#     params$Coordinate_1_based <- FALSE
#     params$RDataDateAdded <- as.character(Sys.Date())
#     params$AnnotationHubRoot <- annotationHubRoot
#     params$SourceFile <- file.path(projectPath, dataFileName)
# 
#        # most files.txt-derived entries have tableName, but
#        # wgEncodeCshlLongRnaSeq, for one, does not
#        # accomodate this by direct assignment from the
#        # supplied dataFileName
#
#     if(nchar(experimentMetadata$tableName) > 0)
#         params$Title <- experimentMetadata$tableName
#     else
#         params$Title <- dataFileName
# 
#     params$Description <- with(experimentMetadata,
#                                {paste (view, type, cell, tableName,
#                                        dataType, dccAccession, lab)})
#     params$SourceUrl <- file.path(sourceDirectory, dataFileName) 
#     params$SourceVersion <- experimentMetadata$labVersion
# 
#     all.fields <- paste(as.character(experimentMetadata), collapse='@')
#     all.fields.clean <- gsub('@+', '|', all.fields, perl=TRUE)
#     params$Tags <- all.fields.clean
# 
#     params
#     }) # assembleMetadata

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

   function (object, metadata.filenames, all.keys,
             data.file.count, verbose=FALSE) {

        # our convention is that encodeDCC metadata files, though starting
        # out life as "files.txt", are downloaded and renamed xxxx.info,
        # where "xxxx" is the name of the encodeDCC/xxxx directory
        # from which the files.txt file was obtained.
        # it is always possible that other files could be found in
        # the download directory; filter them out here:
    metadata.filenames <- grep(".info$", metadata.filenames, value=TRUE)
    tbl <- data.frame()
    row.template <-vector("character", length(all.keys))
    names(row.template) <- all.keys
    all.data.filenames <- c()

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
            new.row.full["remoteDirectory"] <- remote.directory.name
            new.row.as.df <- t(data.frame(new.row.full))
            tbl <- rbind(tbl, new.row.as.df)
            }# for i
        all.data.filenames <- c(all.data.filenames, data.filenames)
        filename = sprintf ('tbl.md.%s.RData', format (Sys.time(), "%a.%b.%d.%Y-%H:%M:%S"))
        #save(tbl, file=filename)
        #printf("saved %d rows to %s", nrow(tbl), filename)
        } # for filename
    rownames(tbl) <- all.data.filenames
    tbl
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
