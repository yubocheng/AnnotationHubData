.makeAuxTable <- function(n, auxFiles, recipe){
    
     colClasses <- metadata(recipe@metadata)$RecipeArgs$auxColClasses[n][[1]]$cols
     auxFile <- auxFiles[n]
     tbl.aux <- read.table(auxFile, sep="\t", colClasses=colClasses)
     colnames(tbl.aux) <- names(colClasses)
     tbl.aux
}

.getMergeArgs <- function(n, recipe){
    metadata(recipe@metadata)$RecipeArgs$auxColClasses[n][[1]]$merge
}


## from FILES (with json)
trackWithAuxiliaryTablesToGRanges <- function(recipe)
{
     mainFile <- inputFiles(recipe)[1] ## always the 1st one? - discuss with Dan and Paul
     auxFiles <- inputFiles(recipe)[-1]
     if(!(length(mainFile) == 1)) stop("No files present in input json.") 
     if(!(length(auxFiles) >= 1)) stop("No auxiliary files listed in input json.  Wrong recipe?")

     colClasses <- metadata(recipe@metadata)$RecipeArgs$mainColClasses
     tbl.main <- read.table(gzfile(mainFile), sep="\t", header=FALSE,
                           colClasses=colClasses)
     colnames(tbl.main) <- names(colClasses)

     auxLen <- length(auxFiles)
     ## a couple for loops because we need to know 'n'...
     auxTabs <- list()
     for(i in seq_len(auxLen)){
         auxTabs[[i]] <- .makeAuxTable(i, auxFiles, recipe)
     } 
     mergeArgs <- list()
     for(i in seq_len(auxLen)){
         mergeArgs[[i]] <- .getMergeArgs(i, recipe)
     } 
     
     ## merge together uses for loop again (to concentrate result down to one thing)
     for(i in  seq_len(auxLen)){
         if(i ==1){tbl <- tbl.main}
         ## otherwise recycle
         tbl <- merge(tbl, auxTabs[[i]], by.x=mergeArgs[[i]][["byX"]],
                       by.y=mergeArgs[[i]][["byY"]],
                       all.x=TRUE)  
     }
     
     tbl <- AnnotationHubData:::.sortTableByChromosomalLocation(tbl)
     colnames <- colnames(tbl)
     requiredColnames <- c("seqname", "start", "end")
     stopifnot(all(requiredColnames %in% colnames))
     otherColnames <- setdiff(colnames, requiredColnames)

     ## drop any rows withouth a seqname
     tbl <- tbl[!is.na(tbl$seqname),]
     
     if("strand" %in%  otherColnames){
         gr <- with(tbl, GRanges(seqname, IRanges(start, end), strand))
         otherColnames <- setdiff(colnames, c(requiredColnames,"strand"))
     }else{  
         gr <- with(tbl, GRanges(seqname, IRanges(start, end)))
     }
     
     mcols(gr) <- DataFrame(tbl[, otherColnames])

        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(recipe@metadata)$Species,
                                    metadata(recipe@metadata)$Genome)
        # if gr only has a subset of all possible chromosomes,
        # then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(recipe))

    outputFile(recipe)

} # trackWithAuxiliaryTableToGRanges
#-------------------------------------------------------------------------------






## Track AND auxiliary tables.
trackandTablesToGRangesRecipe <- function(recipe)
{

    session <- browserSession()
    genome <- recipe@Genome
    genome(session) <- genome
    sourceFile <- recipe@sourceFile
    track <- sub("^.+/database/","",sourceFile)
    query <- ucscTableQuery(session, track)
    tablenames <- tablenames(query)
    
    mainFile <- tablenames[1] ## always the 1st one to be main table 
    auxFiles <- tablenames[-1]    
    if(!(length(auxFiles) >= 1)) { ## this means we are done already
        gr <- track(query, asRangedData = FALSE)
    }else{ ## have to do a merge 1st        
        tbl.main <- getTable(ucscTableQuery(session, mainFile))
        
        auxLen <- length(auxFiles)
        auxTabs <- list()
        for(i in seq_len(auxLen)){
            auxTabs[[i]] <- getTable(ucscTableQuery(session, auxFiles[i]))
        } 
     
        ## merge together uses for loop again (to concentrate result
        ## down to one thing) 
        ## WARNING: this ASSUMES a consistent DB schema at UCSC!
        for(i in  seq_len(auxLen)){
            if(i ==1){tbl <- tbl.main}
            ## otherwise recycle
            tbl <- merge(tbl, auxTabs[[i]], by.x="id",
                         by.y="id",
                         all.x=TRUE)  
        }
        ## replace "chrom" with "seqnames".
        colnames(tbl)[colnames(main) %in% "chrom"] <- "seqname"
        colnames(tbl)[colnames(main) %in% "chromStart"] <- "start"
        colnames(tbl)[colnames(main) %in% "chromEnd"] <- "end"
        
     
        tbl <- AnnotationHubData:::.sortTableByChromosomalLocation(tbl)
        colnames <- colnames(tbl)
        requiredColnames <- c("seqname", "start", "end")
        stopifnot(all(requiredColnames %in% colnames))
        otherColnames <- setdiff(colnames, requiredColnames)
        
        ## drop any rows withouth a seqname
        tbl <- tbl[!is.na(tbl$seqname),]
        
        if("strand" %in%  otherColnames){
            gr <- with(tbl, GRanges(seqname, IRanges(start, end), strand))
            otherColnames <- setdiff(colnames, c(requiredColnames,"strand"))
        }else{  
            gr <- with(tbl, GRanges(seqname, IRanges(start, end)))
        }
        
        mcols(gr) <- DataFrame(tbl[, otherColnames])
    }
    
    
    ## add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(recipe@metadata)$Species,
                                    metadata(recipe@metadata)$Genome)
    ## if gr only has a subset of all possible chromosomes,
    ## then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(recipe))

    outputFile(recipe)

} # trackandTablesToGRangesRecipe
#-------------------------------------------------------------------------------







## make ONLY a simple track
trackToGRangesRecipe <- function(recipe)
{
    session <- browserSession()
    genome <- recipe@Genome
    genome(session) <- genome
    sourceFile <- recipe@sourceFile
    track <- sub("^.+/database/","",sourceFile)
    query <- ucscTableQuery(session, track)

    ## then get the object
    gr <- track(query, asRangedData = FALSE)
    
    
        # add seqlength & chromosome circularity information
    newSeqInfo <- constructSeqInfo(metadata(recipe@metadata)$Species,
                                    metadata(recipe@metadata)$Genome)
        # if gr only has a subset of all possible chromosomes,
        # then update those only
    seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]

    save(gr, file=outputFile(recipe))

    outputFile(recipe)

} # trackToGRangesRecipe
#-------------------------------------------------------------------------------
