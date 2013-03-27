###########################################################################
## Pre-Processing code to allow saving of "bad" tracks that will not load...
## Run these helper functions ahead of time and save output objects

## This function takes about 20 + minutes to run.  The next two (for
## finding bad tracks) take overnight.
.getTracksForGenomes <- function(genomes, session){

    res <- list()
    ## Temp only look at a couple genomes
    ## genomes <- genomes[1:3]
    ## This step alone takes a very long time.
    for (i in seq_along(genomes)){
        genome(session) <- genomes[[i]]
        res[[i]] <- trackNames(session)
    }
    
    ## return a list of genomes with a character of tracks for each.
    names(res) <- genomes
    ## save(res, file="allPossibleTracks.rda") ## auto-save
    res
}
## Usage:
## genomes <- ucscGenomes()$db; session <- browserSession()
## tracks <- .getTracksForGenomes(genomes, session)


## Need a helper to find "bad tracks" for a genome.
.findBadTracks <- function(tracks, genome){
    session <- .ucscSession()
    genome(session) <- genome    
    errors <- list()
    for(i in seq_along(tracks)){
        errors[[i]] <- tryCatch({
            ucscTableQuery(session, tracks[[i]])
        }, error = function(err){return(err)})
    }
    idx = unlist(lapply(errors, is, "error"))
    badTracks <- tracks[idx]
    ## save(badTracks, file=paste0(genome,"BadTracks.rda")) ## safety net
    badTracks
}
## Takes a vector of possible tracks, and a genome (as a string)
## Usage: (after calling:  tracks <- .getTracksForGenomes(genomes, session) )
## badTracks = .findBadTracks(tracks[[1]], "hg19")

## helper to find bad tracks for ALL the genomes (this takes all night)
.getBadTracksForGenomes <- function(genomes, tracksList){
    allBadTracks <- list()
    ## This step takes a lot of very long times.
    for(i in seq_along(genomes)){
        allBadTracks[[i]] <- .findBadTracks(tracksList[[i]], genomes[[i]])
    }
    ## return a list of genomes with a character of tracks for each.
    names(allBadTracks) <- genomes
    ## save(allBadTracks, file="allBadTracks.rda") ## auto-save
    allBadTracks
}
## just gets the bad tracks for all the genomes (should take ages to run)
## Usage: (after calling:  tracks <- .getTracksForGenomes(genomes, session) )
## badTracks = .getBadTracksForGenomes(genomes, tracks)


##############################################################################
##############################################################################
##############################################################################

##############################################################################
## Code to import tracks from UCSC and to make them into AHMs
UCSCTrackImportPreparer <-
    setClass("UCSCTrackImportPreparer", contains="ImportPreparer")

UCSCFullTrackImportPreparer <-
    setClass("UCSCFullTrackImportPreparer", contains="ImportPreparer")


## To store data on each track:
## ftp://hgdownload.cse.ucsc.edu/goldenpath/<genome name>/database/<track name>

## SO: hg19, track oreganno looks like this.
## ftp://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/oreganno
## The reason is that the 1st part (up to oreganno) is actual location
## of ftp data the oreganno data will be there in several files dumped
## from the DB.

.checkAllTracks <- function(allTracks) {
    species <- GenomicFeatures:::UCSCGenomeToOrganism(names(allTracks))
    if (any(idx <- is.na(species))) {
        badSpecies <- names(species)[idx]
        stop("update GenomicFeatures:::UCSCGenomeToOrganism",
             " to support ", paste(sQuote(badSpecies), collapse=","))
    }
    TRUE
}

.UCSCTrackSourceTracks <- function(){
    ## retrieve all possible tracks from UCSC
    genomes <- ucscGenomes()$db

    ## get the tracks for each genome. (pre-computed)
    .cachedTracks <- function(filename) {
        loadFile <- system.file("extdata","badUCSCTracks", filename,
                                package = "AnnotationHubData")
        x <- load(loadFile)
        get(x)
    }
    allTracks <- .cachedTracks("allPossibleTracks.rda")
    badTracks <- .cachedTracks("allBadTracks.rda")

    ## check that we can know all species names for all these tracks.
    .checkAllTracks(allTracks)
    ## Now I just have to merge the results of these two things
    Map(function(a, b) a[!(a %in% b)], allTracks, badTracks)
}




.UCSCTrackMetadata <-
    function(sourceTracks, type = c("FULL", "TRACKONLY"))
{
    type <- match.arg(type)
    recipe <- switch(type, FULL="trackandTablesToGRangesRecipe",
                     TRACKONLY="trackToGRangesRecipe",
                     stop("No valid type argument"))
    
    ## To store data on each track:
    ## ftp://hgdownload.cse.ucsc.edu/goldenpath/<genome>/database/<track>

    genome <- rep(names(sourceTracks), sapply(sourceTracks,length))
    track <- unlist(sourceTracks, use.names=FALSE)
    names(track) <- genome    
    trackName <- unlist(lapply(sourceTracks, names), use.names=FALSE)

    ## This really has to be the same for both. (parsed later on for trackName)
    sourceFile <- paste0("goldenpath/", genome, "/database/", track)
    
    ## customize name and description depending if it's the full track or not
    description <-
        paste0("GRanges object from UCSC track ", sQuote(trackName))
    if (type=="FULL")
        description <- paste0(description, ", with additional tables")

    sourceUrl <- paste0("rtracklayer://hgdownload.cse.ucsc.edu/", sourceFile)
    title <- trackName
    species <-  unname(GenomicFeatures:::UCSCGenomeToOrganism(genome))

    sourceVersion <- genome
    stockTags <- c("UCSC", "track", "Gene", "Transcript", "Annotation")
    tags <- lapply(track, c, stockTags)

    uspecies <- unique(species)
    taxonomyId <- .taxonomyId(uspecies)[match(species, uspecies)]

    ## use Map to make all these from vectors

    Map(AnnotationHubMetadata, Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrl,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title, Tags=tags,
        MoreArgs=list(
          AnnotationHubRoot=NA_character_, Coordinate_1_based = TRUE,
          DataProvider = "hgdownload.cse.ucsc.edu",
          Maintainer = "Marc Carlson <mcarlson@fhcrc.org>",
          RDataClass = "GRanges",
          RDataDateAdded = format(Sys.time(), "%Y-%m-%d GMT"),
          RDataVersion = "0.0.1",
          Recipe = c(recipe, package="AnnotationHubData"),
          SourceMd5=NA_character_, SourceSize=NA_real_))
}

## method for track only recipe
setMethod(newResources, "UCSCTrackImportPreparer",
    function(importPreparer, currentMetadata=list(), numberGenomesToProcess=NULL,
             ...)
{
    
    allGoodTracks <- .UCSCTrackSourceTracks()
    if( is.null(numberGenomesToProcess)){
        sourceTracks <- allGoodTracks
    }else{
        sourceTracks <- allGoodTracks[numberGenomesToProcess]
    }

    ## filter known    
##     knownTracks <- sapply(currentMetadata, function(elt) {
##         sub("^.+/database/","",(metadata(elt)@SourceFile) 
##     })
##     sourceTracks <- sourceTracks[!sourceTracks %in% knownTracks]
   
    ## AnnotationHubMetadata
    .UCSCTrackMetadata(sourceTracks, type="TRACKONLY")
})


## For full tracks
setMethod(newResources, "UCSCFullTrackImportPreparer",
    function(importPreparer, currentMetadata=list(), numberGenomesToProcess=NULL,
             ...)
{
    
    allGoodTracks <- .UCSCTrackSourceTracks()
    if( is.null(numberGenomesToProcess)){
        sourceTracks <- allGoodTracks
    }else{
        sourceTracks <- allGoodTracks[1:numberGenomesToProcess]
    }

    ## filter known    
##     knownTracks <- sapply(currentMetadata, function(elt) {
##         sub("^.+/database/","",(metadata(elt)@SourceFile) 
##     })
##     sourceTracks <- sourceTracks[!sourceTracks %in% knownTracks]

    ## AnnotationHubMetadata
    .UCSCTrackMetadata(sourceTracks, type="FULL")
})
















## I need to be able to learn what the files are that are associated
## with each track in an automatic way.  Maybe I can use rtracklayer?
## Or maybe I will just have to look for common names in the dir
## listed?

## ?trackName
## library(rtracklayer)
## session <- browserSession()
## head(ucscGenomes())
## genome(session) <- "hg19"
## head(trackNames(session))


## Already can do stuff like this:
## query <- ucscTableQuery(session, "Conservation",
##                         GRangesForUCSCGenome("mm9", "chr12",
##                                              IRanges(57795963, 57815592)))
## track(query, asRangedData = FALSE)  # gets a GRanges object


## So does this work with my example track?  Well only "kinda".
## You get the main table alright, but you don't get the rest.
## query <- ucscTableQuery(session, "oreganno")
## foo = track(query, asRangedData = FALSE)  


## But here is something that is more useful (potentially)
## tableNames(query)  ## lists table names
## This is good so long as table names match the filenames...

## or how about just doing this:
## tableName(query) <- "oregannoAttr"
## bar  = getTable(query)

## ?ucscGenomes




## There are some other issues: (some tracks are listed but are also unknown).
## BUT: these failure tracks seem to be consistently failing.

## So for example: c("ruler", "oligoMatch","cutters") all fail on hg19 and hg16.
## And others too?  (untested).  This suggests that I can probably
## just make a black list and use that to prevent certain things.

## 
