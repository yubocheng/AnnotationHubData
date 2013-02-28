## Code to import tracks from UCSC and to make them into AHMs

UCSCTrackImportPreparer <-
    setClass("UCSCTrackImportPreparer", contains="ImportPreparer")


## To store data on each track:
## ftp://hgdownload.cse.ucsc.edu/goldenPath/<genome name>/database/<track name>

## SO: hg19, track oreganno looks like this.
## ftp://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/oreganno
## The reason is that the 1st part (up to oreganno) is actual location
## of ftp data the oreganno data will be there in several files dumped
## from the DB.

.getTracksForGenomes <- function(genomes, session){

    res <- list()
    ## Temp only look at a couple genomes
    ## genomes <- genomes[1:3]
    ## This step alone takes a very long time.
    for(i in seq_along(genomes)){
        genome(session) <- genomes[[i]]
        res[[i]] <- trackNames(session)
    }
    
    ## return a list of genomes with a character of tracks for each.
    names(res) <- genomes
#    save(res, file="allPossibleTracks.rda")
    res
}

## For each genome, I need to 1) set the build, 2) iterate through
## tracknames, 3) list tables for each track.


## foo <-  tracks[[1]]
## now "try" each one capturing errors.  Should look similar to
## tryCatch(stop(e), error = function(err){return(err)})

## We must loop to learn which things are really "bad tracks".
.findBadTracks <- function(tracks, genome){
    session <- browserSession()
    genome(session) <- genome    
    errors <- list()
    for(i in seq_along(tracks)){
        errors[[i]] <- tryCatch({
            ucscTableQuery(session, tracks[[i]])
        }, error = function(err){return(err)})
    }
    idx = unlist(lapply(errors, is, "error"))
    badTracks <- tracks[idx]
    save(badTracks, file=paste0(genome,"BadTracks.rda"))
    badTracks
}
## Takes a vector of possible tracks, and a genome
## Usage: (after calling:  tracks <- .getTracksForGenomes(genomes, session) )
## badTracks = .findBadTracks(tracks[[1]], "hg19")

.getBadTracksForGenomes <- function(genomes, tracksList){
    allBadTracks <- list()
    ## This step takes a lot of very long times.
    for(i in seq_along(genomes)){
        allBadTracks[[i]] <- .findBadTracks(tracksList[[i]], genomes[[i]])
    }
    ## return a list of genomes with a character of tracks for each.
    names(allBadTracks) <- genomes
    save(allBadTracks, file="allBadTracks.rda")
    allBadTracks
}
## just gets the bad tracks for all the genomes (should take ages to run)
## Usage: (after calling:  tracks <- .getTracksForGenomes(genomes, session) )
## badTracks = .getBadTracksForGenomes(genomes, tracks)







.UCSCTrackSourceTracks <- function(){
    ## retrieve all possible tracks from UCSC
    genomes <- ucscGenomes()$db
    session <- browserSession()
    ## get the tracks for each genome.
    tracks <- .getTracksForGenomes(genomes, session)

    ## save the result of this to allPossibleTracks.rda
    
    ## I need a blacklist to remove busted tracks from consideration.
    badTracks = .getBadTracksForGenomes(genomes, tracks)
    
}


## .UCSCTrackMetadata <-
##     function(baseUrl, sourceUrl)
## {
##     gtf <- sub(baseUrl, "ensembl/", sourceUrl)
##     rdata <- sub(".gz$", ".RData", gtf)
##     regex <- "^([[:alpha:]_]+)\\.([[:alpha:]]+).*"
##     title <- sub(".gz$", "", basename(gtf))
##     species <- gsub("_", " ", sub(regex, "\\1", title), fixed=TRUE)
##     genome <- sub(regex, "\\2", title)
##     description <- paste("Gene Annotation for", species)
##     sourceUrl <- paste0("ftp://ftp.ensembl.org/", gtf)
##     sourceVersion <- sub(".*(release-[[:digit:]]+).*", "\\1", gtf)
##     rDataDateAdded <- format(Sys.time(), "%Y-%m-%d GMT")

##     sourceVersion <- sub(".*(release-[[:digit:]]+).*", "\\1", sourceUrl)
##     rDataDateAdded <- 
    
##     Map(AnnotationHubMetadata, AnnotationHubRoot=NA_character_,
##         Description=description, Genome=genome, SourceFile=gtf,
##         SourceUrl=sourceUrl, SourceVersion=sourceVersion,
##         Species=species, Title=title,
##         MoreArgs=list(
##           Coordinate_1_based = TRUE,
##           DataProvider = "ftp.ensembl.org",
##           Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
##           RDataClass = "GRanges",
##           RDataDateAdded = format(Sys.time(), "%Y-%m-%d %T"),
##           RDataVersion = "0.0.1",
##           Recipe = c("UCSCTrackToGRangesRecipe", package="AnnotationHubData"),
##           Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation")))
## }

## setMethod(newResources, "UCSCTrackImportPreparer",
##     function(importPreparer, currentMetadata)
## {
    
##     sourceTracks <- .UCSCTrackSourceTracks()

##     ## filter known
##     knownTracks <- sapply(currentMetadata, function(elt) {
##         ## assumption is that we will stick the track info in $SourceUrl
##         ## So this string will ID a track uniquely (even though it
##         ## won't point to a real resource)
##         metadata(elt)$SourceUrl 
##     })
##     sourceTracks <- sourceTracks[!sourceTracks %in% knownTracks]

##     ## AnnotationHubMetadata
##     .UCSCTrackMetadata(baseUrl, sourceUrls)
## })














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
