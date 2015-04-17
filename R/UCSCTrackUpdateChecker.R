#############################################################
### Code to get the dir list and then filter it etc.



## helper to filter for each
.filter <- function(allTracks, badTracks){
    allTracks[!(allTracks %in% badTracks)]
}
## goodTracks <- mapply(.filter, allTracks, badTracks)


## Try a new strategy for getting ALL the dir listings at ONCE.

###############################################
## make this into a function and call it
.getListing <- function(){
    ## I did this once and then saved it (it takes a few minutes)
    listing <- getURL(url="ftp://hgdownload.cse.ucsc.edu/goldenPath/",
                      followlocation=TRUE, customrequest="LIST -R")
    ## then bust that into single rows.
    listing <- strsplit(listing,"\n")[[1]]
    save(listing, file="listing.Rda")
    listing
}
## listing <- .getListing()

###############################################
## need to get symlinks
.getGenomeAbbrevs <- function(genomes){
    baseList <- getURL(url="ftp://hgdownload.cse.ucsc.edu/goldenPath/",
                       curl=handle_find(url)$handle)
    baseList <- strsplit(baseList,"\n")[[1]]
    ## baseList[grep('cb1',baseList)]
    ## baseList[grep(genome[1],baseList)] ## a normal one

    ## go through all the genomes, and for each one, check if it's like    
    ## cb1, replace as necessary
    checkForAndRetrieveSymLinks <- function(genStr){
        genStr2 <- paste(" ",genStr, sep="")
        str <- baseList[grep(genStr2, baseList)] ## gets one str
        ## if(length(str) >1){ message("too many strs for: \n", str)}
        if(grepl('->', str)){ ## does it have a symlink?
            res <- sub("^.+->\\s+","",str)
        }else{
            res <- genStr
        }
        res
    }    
    unlist(lapply(genomes, checkForAndRetrieveSymLinks))
}
## genomes2 <- .getGenomeAbbrevs(genomes)



###############################################
## make this into a couple of functions and call
## Now I need to split that up based on subdirs of interest.
## there are empty rows whenever we switch dirs.
.getAllBreakPoints <- function(listing){
    grep('^$',listing) ## index of ALL split points
}
## subsIdx <- .getAllBreakPoints(listing)


## SECOND function
## There are a subset of other dirs that I want after some of those...
## To get these 1st make a set of strings based on genomes
.getNamedGenomeBreakPoints <- function(genomes, listing){
    genStrs1 <- paste(genomes, "/database:",sep="")
    genStrs2 <- paste("^", genomes, "/database:",sep="")
    ## and then grep to get these indices
    genDbIdx <- unlist(lapply(genStrs2, grep, listing))  ## takes a min
    names(genDbIdx) <- sub("/database:","",listing[genDbIdx])
    genDbIdx
}
## genDbIdx <- .getNamedGenomeBreakPoints(genomes2, listing)



## PROCESSES LATEST DATE AND TABLE NAME INFO FOR A GENOME
.getTableDates <- function(genome, listing, subsIdx, genDbIdx){

    ## helper takes all subsIdx and one genDbIdx
    getRange <- function(genIdx, subsIdx, listing){
        if(length(genIdx) >1){ message("too many genIdxs for: \n", genome)}
        end <- min(subsIdx[subsIdx > genIdx])
        ## if(length(end) >1){ message("too many ends for: \n", genome)}
        listing[genIdx:(end-1)]
    }
    
    dirs <- getRange(genDbIdx[names(genDbIdx) %in% genome],
                     subsIdx, listing)
    dirs <- dirs[-(1:3)]
    ## drop all but the txt.gz files.
    dirs <- dirs[grep(".txt.gz$",dirs)]

    ## Then remove unwanted stuff from the front end
    dirs <- sub("^.+ftp\\s+\\d+\\s","",dirs)
    ## remove .txt.gz from name
    dirs <- sub(".txt.gz$","",dirs)

    
    ## UCSC means for abbreviating months.
    months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
                'Oct','Nov','Dec')
    names(months) <- c('01','02','03','04','05','06','07','08','09','10',
                       '11','12')
    ## helper for cleanup
    dateClean <- function(str){
        ## then replace whitespaces with single tabs
        swp = gsub("\\s+","\t",str)
        ## then split on tabs
        res <- unlist(strsplit(swp, "\t"))
        if(length(grep(":",res[3]))>0){
            res[3] <- "2013"
        }
        paste(res[3],
              names(months)[months %in% res[1]],
              res[2],
              sep="-")
        ##as.POSIXct(date, tz = "GMT")
    }

    dates <- unlist(lapply(dirs, dateClean))
    nameClean <- function(str){
        swp = gsub("\\s+","\t",str)
        res <- unlist(strsplit(swp, "\t"))[4]
    }
    tables <- unlist(lapply(dirs, nameClean))
    ## hand back as data.frame
    #data.frame(dates=dates, tables=tables)
    names(tables) <- dates
    tables
}
## example of how you can use this for a particular genome.
## res <- .getTableDates(genome="hg19", listing, subsIdx, genDbIdx)



## Wrapper function to do all the work of getting and processing the
## latest date information from the FTP site.
getLatestTableDates <- function(){
    listing <- .getListing()
    allTracks <- AnnotationHubData:::.cachedTracks("allPossibleTracks.rda") 
    badTracks <- AnnotationHubData:::.cachedTracks("allBadTracks.rda")
    goodTracks <- mapply(.filter, allTracks, badTracks)
    genomes <- names(goodTracks)
    genomes2 <- .getGenomeAbbrevs(genomes)
    subsIdx <- .getAllBreakPoints(listing)
    genDbIdx <- .getNamedGenomeBreakPoints(genomes2, listing)
    curTables <- lapply(genomes2, .getTableDates, listing, subsIdx, genDbIdx)
    names(curTables) <- genomes
    curTables
}
## curTables <- getLatestTableDates()




############################################################################
## I also need the association between a table and it's tracks

## get a list of tables for a genome and track
.findTrackTables <- function(track, session){
    query <- ucscTableQuery(session, track)
    tableNames(query)
}
## session <- browserSession()
## genome(session) <- "hg19"
## res <- .findTrackTables("cytoBand", session)


## get all tables for a genome based on tracks
.getTrackTablesForGenome <- function(genome, tracks){
    ## iterate through the list to get all the tableNames for each track
    session <- browserSession()
    genome(session) <- genome
    tables <- lapply(tracks, .findTrackTables, session)
    names(tables) <- tracks
    tables
}
## tracks <- goodTracks[[1]][1:3] ## reduce to a simple vector o track names
## genome <- "hg19"
## res <- .getTrackTablesForGenome(genome, tracks)


## Actual code to make and save this will be done as a loop instead of
## the more elegant mapply just so that I can save as I go...
.getTrackTablesForAllGenomes <- function(genomes, trackLists){
    if(length(genomes) != length(trackLists)) stop("The number of genomes must equal the number of track lists.")
    genomeTrackTable <- list()
    for(i in seq_along(genomes)){
        message("Now processing: ",genomes[i])
        genomeTrackTable[[i]] <- .getTrackTablesForGenome(genomes[i],
                                                          trackLists[[i]])
        names(genomeTrackTable)[i] <- genomes[i]
        save(genomeTrackTable,file="genomeTrackTable.Rda") 
    }
    genomeTrackTable
}
## res <- .getTrackTablesForAllGenomes(genomes, goodTracks)
## Can also load this with load("genomeTrackTable.Rda")  ## it will be saved




##############################################################################
## Helpers for translating table dates to track dates.
.getLatestTrackDate <- function(track, genome, genomeTrackTable, curTables){
    tables <- genomeTrackTable[[genome]][[track]]
    dates <- curTables[[genome]][curTables[[genome]] %in% tables]
    maxDate <- max(as.POSIXct(names(dates)))
    as.character(maxDate) ## cast because there were issues storing this data.
}
## genome <- "hg19"
## track <- "stsMap"
## res <- .getLatestTrackDate(track, genome, genomeTrackTable, curTables)


getLatestTrackDates <- function(){
    curTables <- getLatestTableDates()
    genomeTrackTable <-
        AnnotationHubData:::.cachedTracks("genomeTrackTable.Rda") 
    ## rough sketch (for now)
    trackDates <- list()
    for(i in seq_along(names(genomeTrackTable))){
        genome <- names(genomeTrackTable[i])
        tracks <- names(genomeTrackTable[[i]])
        trackDates[[i]] <- unlist(lapply(tracks, .getLatestTrackDate,
                                         genome,  genomeTrackTable, curTables))
        names(trackDates[[i]]) <- tracks
    }
    names(trackDates) <- names(genomeTrackTable)
    trackDates
}
## This runs
## trackDates <- getLatestTrackDates()


## BUT there are strange warnings!
##  1: In max.default(structure(numeric(0), class = c("POSIXct",  ... :
##  no non-missing arguments to max; returning -Inf

## It causes me to have some NA values for some of the dates...
## This in turn is caused by a small percentage of the tracks
## (134/3573) that are misnamed in the FS presented by UCSC (named
## differently from the tracks).



############################################################################
## Now I just need some code to compare this data with what is in the
## metadata

## So get the metadata
getAHTrackDates <- function(){
    ah = AnnotationHub()
    m = AnnotationHub:::.metadata(snapshotUrl(ah),
      filters = list(DataProvider="hgdownload.cse.ucsc.edu"),
      cols = c("SourceFile","RDataDateAdded","DataProvider"))

    ## SourceFile is easier to clean up than sourceUrl...
    data <- strsplit(m$SourceFile,"/")
    trackNames <- unlist(lapply(data, function(x){x[4]}))
    genNames <- unlist(lapply(data, function(x){x[2]}))
    if(length(trackNames) == length(genNames) &&
       dim(m)[1] == length(trackNames)){
        m <- cbind(as.data.frame(m), trackNames, genNames)
    }

    ## Now make m into a similar structure to trackDates
    trx <- as.character(m$RDataDateAdded)
    names(trx) <- trackNames
    ## then split it
    split(trx, f=m$genNames)
}
## ahTrackDates <-  getAHTrackDates()



############################################################################
## And now we can use this to write code that compares and just lists
## tracks that are out of date.
tracksToUpdate <- function(){
    trackDates <- getLatestTrackDates()
    ahTrackDates <-  getAHTrackDates()
    
    ## go along and for each list element in ahTrackDates we will need
    ## to look at the equivalent element from trackDates and then
    ## compare the dates.  Dates will be not OK (TRUE for update
    ## this), OK, or NA.
    ## This requires a helper to test indiv, elements of a vector
    .testDate <- function(x, other){
        if(is.na(x)){
            return(NA)
        }else if(is.na(other[names(x)])){
            return(NA)
        }else{
            if(as.POSIXct(x) > as.POSIXct(other[names(x)])){
                return(FALSE)
            }else{ ## this means it needs an update 
                return(TRUE)
            }
        }
    }
    ## And another helper to lets us loop across genomes
    .compareDates <- function(genomeDateSet,trackDates){
        ah <- genomeDateSet[[1]]
        other <- trackDates[names(genomeDateSet)][[1]]
        ## res <- lapply(ah, .testDate, other=other) ## rips names from vector
        res <- vector()
        for(i in seq_along(ah)){
            res[i] <- .testDate(ah[i], other)
        }
        names(res) <- names(ah)
        res
    }
    

    ## I have to loop along the ahTrackDates object. I can use lapply
    ## (but not mapply), and have to pass in all of trackDates every
    ## time.

    ## And again I am burned because lapply() jetisons the names...
    res <- list()
    for(i in seq_along(ahTrackDates)){
        res[[i]] <- .compareDates(ahTrackDates[i], trackDates)
    }
    names(res) <- names(ahTrackDates)

    ## return list of vectors.
    res
}
## res <- tracksToUpdate()

