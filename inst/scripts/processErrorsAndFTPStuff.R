
## Code to parse error log

lines = readLines("error.log")

## 1st lets get a list of the tracks that failed
bt1 = sub("^.+hgdownload.cse.ucsc.edu\\/goldenpath\\/.+\\/database\\/","",lines, perl=TRUE)
bt = sub(":.+$","",bt1, perl=TRUE)

## and the genomes
bg1 = sub("^.+hgdownload.cse.ucsc.edu\\/goldenpath\\/","",lines, perl=TRUE)
bg = sub("/.+$","",bg1, perl=TRUE)

## now lets start categorizing the errors.
er <- sub("^.+hgdownload.cse.ucsc.edu\\/goldenpath\\/.+\\/database\\/.+?: ","",lines, perl=TRUE)

## So now we could make a data.frame
df = data.frame(genome=bg, track=bt, error=er, stringsAsFactors=FALSE)
## but probably we want to get rid of redundant genome/track info.
dupIdx = !duplicated(df[,c(1,2)])
erTab = df[dupIdx,] ## throws out 2nd+ occurrences of genome/track

save(erTab,file="errorTable.Rda")




#############################################################3
#############################################################3
#############################################################3

#############################################################3
### Code to get the dir list and then filter it etc.


library(AnnotationHubData)
## get the tracks for each genome. (pre-computed)
.cachedTracks <- function(filename) {
    loadFile <- system.file("extdata","badUCSCTracks", filename,
                            package = "AnnotationHubData")
    x <- load(loadFile)
    get(x)
}
allTracks <- .cachedTracks("allPossibleTracks.rda") 
badTracks <- .cachedTracks("allBadTracks.rda")

## helper to filter for each
filter <- function(allTracks, badTracks){
    allTracks[!(allTracks %in% badTracks)]
}
goodTracks <- mapply(filter, allTracks, badTracks)

## all the genomes we want:
genomes <- names(goodTracks)


## use the existing whitelist from the prepare code for UCSC
## use this:

#getURL("ftp://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/", opts=list(dirlistonly=TRUE))

## filter out stuff we don't need and then trap key information.


## Try a new strategy for getting ALL the dir listings at ONCE.
require(RCurl)

###############################################
## make this into a function and call it
getListing <- function(){
    ## I did this once and then saved it (it takes a few minutes)
    listing <- getURL(url="ftp://hgdownload.cse.ucsc.edu/goldenPath/",
                      followlocation=TRUE, customrequest="LIST -R")
    save(listing, file="listing.Rda")
    ## then bust that into single rows.
    strsplit(listing,"\n")[[1]]
}
listing <- getListing()

###############################################
## need to get symlinks
getGenomeAbbrevs <- function(genomes){

    baseList <- getURL(url="ftp://hgdownload.cse.ucsc.edu/goldenPath/")
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
genomes2 <- getGenomeAbbrevs(genomes)



###############################################
## make this into a couple of functions and call
## Now I need to split that up based on subdirs of interest.
## there are empty rows whenever we switch dirs.
getAllBreakPoints <- function(listing){
    grep('^$',listing) ## index of ALL split points
}
subsIdx <- getAllBreakPoints(listing)


## SECOND function
## There are a subset of other dirs that I want after some of those...
## To get these 1st make a set of strings based on genomes
getNamedGenomeBreakPoints <- function(genomes, listing){
    genStrs1 <- paste(genomes, "/database:",sep="")
    genStrs2 <- paste("^", genomes, "/database:",sep="")
    ## and then grep to get these indices
    genDbIdx <- unlist(lapply(genStrs2, grep, listing))  ## takes a min
    names(genDbIdx) <- sub("/database:","",listing[genDbIdx])
    genDbIdx
}
genDbIdx <- getNamedGenomeBreakPoints(genomes2, listing)



## PROCESSES LATEST DATE AND TABLE NAME INFO FOR A GENOME
getTableDates <- function(genome, listing, subsIdx, genDbIdx){

##     require(RCurl)
##     url <- paste("ftp://hgdownload.cse.ucsc.edu/goldenPath/",
##                  genome,"/database/",sep="")
##     dir <- getURL(url) ## do dirListOnly=TRUE just makes it not get dates...
##     dirs <- strsplit(dir,"\n")[[1]]


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
## res <- getTableDates(genome="hg19", listing, subsIdx, genDbIdx)









### TODO: 1) return a data frame with cols: genome, track, date. 
### 2) date needs to be a date object as.POSIXct("2012-01-15",tz = "GMT")
### 3) track needs to be track name (not file name)
### 5) everything with a 2013 date has been given a time stamp instead of a
## year, I need to update these and also convert months to numbers.

## 4) filter the track names with the whitelist... (filter at the end?
## or as we go?) - use proj/mcarlson/UCSC_Download/processFiles.R to
## get whiteList and process as we go...


## Now lets get all the table dates for each genome
tables <- lapply(genomes2, getTableDates, listing, subsIdx, genDbIdx)
names(tables) <- genomes

## ## to find problem ones:
## tables = list()
## for(i in 1:145){print(i); tables[[i]] <- getTableDates(genome=genomes2[[i]],listing, subsIdx, genDbIdx)}

##for(i in 1:145){print(i); tables[[i]] <- getTableDates(genome=genomes[[i]])}

## This is a PROBLEM.  They won't let me scan their dirs...
## save(tables,file="curTables.Rda")



table(res %in% goodTracks[[1]])
table(goodTracks[[1]] %in% res)
##Hmmm apparently some things are missing if I just scan for track names

grep( "decodeRmap", res)
## it happens because sometimes tracks have names that are not
## repeated in any tables...  So for decodeRmap some tables are found
## this way..
grep( "Sex", res)
## So to do this right, I need to translate from track -> table and
## then check the dates...

## AND since these are tables that can (in theory be updates
## independently of each other, I need to check ALL the tables
## associated with a track.


## So 1st I need to get all the track/table associations (for the good
## tracks).  I can do this in rtracklayer.







## So next up: I need to get all the tablenames for all the tracks.
## Have I done this already??? - sadly it appears I have not...

## BUT this is a piece of info that I could save for later and reuse...
## I need to modify the functions that I used before to accomplish this.


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



## And when that works...
## .getTrackTablesForAllGenomes <- function(genomes, trackLists){
##     mapply(.getTrackTablesForGenome, genomes, trackLists)
## }
## trx = goodTracks[c(1,15)]
## genomes <- names(trx)
## trx[[1]] <- trx[[1]][1:2]
## trx[[2]] <- trx[[2]][1:3]
## res <- .getTrackTablesForAllGenomes(genomes, trx)



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

res <- .getTrackTablesForAllGenomes(genomes, goodTracks)


##########################################################################
## Now I just have to compare the two objects and the metadata from the DB
## the simplest way is to get the newest date from the tables
## associated with a particular track so that I have the most recent
## possible data for a track.

## So I probably want to iterate through all the tables for each track, and get the dates, then make them into POSIXct dates, and then get the newest one and associate THAT date stamp with the track then return THAT to Dan.

## So max(dates)

## ALSO: I may have a bug in the code that processes the year stamps

## foo = as.POSIXct("12-02-14",tz = "GMT")
## bar = as.POSIXct("12-02-14",tz = "GMT")
## sna = as.POSIXct("12-01-17",tz = "GMT")
## max(c(foo, sna, bar))




## So my tables/tracks object is saved.  And my dates can be processed
## (re-running and temp saving those too).

## And then I have to process my data to reflect just the dates per track.


load("genomeTrackTable.Rda")

## helper takes genome and track and finds the latest date based on
## values in curTables (curTables is what we get from getTableDates()
## above)

.getLatestTrackDate <- function(track, genome, genomeTrackTable, curTables){
    tables <- genomeTrackTable[[genome]][[track]]
    dates <- curTables[[genome]][curTables[[genome]] %in% tables]
    maxDate <- max(as.POSIXct(names(dates)))
    as.character(maxDate) ## cast because there were issues storing this data.
}
## genome <- "hg19"
## track <- "stsMap"
## res <- .getLatestTrackDate(track, genome, genomeTrackTable, curTables)


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
