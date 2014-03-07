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
    if (!getOption("AnnotationHub_Use_Disk", FALSE)) {
        upload_to_S3(outputfile(recipe), metadata(recipe)@RDataPath)
    }

    outputFile(recipe)

} # trackWithAuxiliaryTableToGRanges
#-------------------------------------------------------------------------------




## helper to remove 'id' col
.removeId <- function(table){ 
    newColnames <- setdiff(colnames(table), "id")
    table[,newColnames]
}
## compress a whole table one col at a time.
## This (currently) assumes all cols should be characters
.compressTable <- function(table, levels){
    sf <- factor(table$id,levels=levels)
    table <- .removeId(table)
    res <- DataFrame()
    for(i in seq_len(ncol(table))){ ## for ea. column
        col <- splitAsList(as.character(table[[i]]), f=sf)
        if(i==1){
            res <- DataFrame(col)
        }else{
            res <- DataFrame(res, DataFrame(col)) ## cbind doesn't work?
        }
    }
    colnames(res) <- colnames(table)
    res
}


.makeComplexGR <- function(tbl,auxFiles,auxTabs){
    ## replace "chrom" with "seqnames".
    colnames(tbl)[colnames(tbl) %in% "chrom"] <- "seqname"
    colnames(tbl)[colnames(tbl) %in% "chromStart"] <- "start"
    colnames(tbl)[colnames(tbl) %in% "chromEnd"] <- "end"
    
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
    ## append the initial mcols
    mcols(gr) <- DataFrame(tbl[, otherColnames])
    
    
    
    ## make a spliting factor based on the initial table
    splitFactor <- factor(tbl$id, levels=tbl$id)
    
    for(i in seq_along(auxFiles)){
        new <- auxTabs[[i]]
        if(identical(as.character(splitFactor), as.character(new$id))){
            ## Add it in
            mcols(gr) <- DataFrame(mcols(gr), .removeId(new))
        }else{## otherwise compress it 1st               
            mcols(gr) <- DataFrame(mcols(gr),.compressTable(new,
                                                    levels(splitFactor)))
        }
    }
    gr
}

## Track AND auxiliary tables.
## Unfortunately, the schemas for some tracks are complex.
## This means that in the future I will have to use ucscSchema etc. to
## get the addional information so that I can properly assemble them.
## For now, we will check for "id" and only proceed if all tables have this.
trackandTablesToGRangesRecipe <- function(recipe)
{
    session <- browserSession()
    genome <- metadata(recipe)@Genome
    genome(session) <- genome
    sourceFile <- metadata(recipe)@SourceFile
    track <- sub("^.+/database/","",sourceFile)
    query <- ucscTableQuery(session, track)
    tableNames <- tableNames(query)
    
    mainFile <- tableNames[1] ## always the 1st one to be 2main table 
    auxFiles <- tableNames[-1]    
    if(!(length(auxFiles) >= 1)) { ## this means we are done already
        gr <- track(query, asRangedData = FALSE)
    }else{ ## have to do a merge 1st
        ## have to "get" primary in table form to assure "id" will be present
        tbl <- getTable(ucscTableQuery(session, mainFile))

        ## Now get the other tables
        auxTabs <- list()
        for(i in seq_along(auxFiles)){
            ## query <- ucscTableQuery(session, track)
            tableName(query) <- auxFiles[i]
            auxTabs[[i]] <- getTable(query)
        }

        allColNames <- list()
        allColNames[[1]] <- colnames(tbl)
        for(i in seq_len(length(auxTabs))){
            idx <- i+1
            #print(idx)
            allColNames[[idx]] <- colnames(auxTabs[[i]])
        }
        ## for each element is there a value called "id"?
        idPresent <- unlist(lapply(allColNames, function(x){'id' %in% x}))
        
        if(all(idPresent)){
            gr <- .makeComplexGR(tbl,auxFiles,auxTabs)
        }else{
            message("track schema is too complex: using basic track instead")
            query <- ucscTableQuery(session, track)
            gr <- track(query, asRangedData = FALSE)
        }
        
    }
    
    
##     ## add seqlength & chromosome circularity information
##     newSeqInfo <- constructSeqInfo(metadata(recipe)@Species,
##                                     metadata(recipe)@Genome)
##     ## if gr only has a subset of all possible chromosomes,
##     ## then update those only
##     seqinfo(gr) <- newSeqInfo[names(seqinfo(gr))]
    save(gr, file=outputFile(recipe))  
##     outputFile(recipe)

} # trackandTablesToGRangesRecipe
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
