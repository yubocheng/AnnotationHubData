## Code to import tracks from UCSC and to make them into AHMs

## .importUCSCTracks <-
##     function(files)
## {
##     base <- "ftp://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/"
##     gtf <- dir(base, pattern=".*gtf.gz", recursive=TRUE, full.names=TRUE)

    
##     gtf <- sub(ahroot, "", gtf, fixed=TRUE)
##     gtf <- sub(sprintf("^%s", .Platform$file.sep), "", gtf)
##     rdata <- sub(".gz$", ".RData", gtf)

##     regex <- "^([[:alpha:]_]+)\\.([[:alpha:]]+).*"
##     title <- sub(".gz$", "", basename(gtf))
##     species <- gsub("_", " ", sub(regex, "\\1", title), fixed=TRUE)
##     genome <- sub(regex, "\\2", title)

##     description <- paste("Gene Annotation for", species)
##     sourceUrl <- paste0("ftp://ftp.ensembl.org/", gtf)
##     sourceVersion <- sub(".*(release-[[:digit:]]+).*", "\\1", gtf)
##     rDataDateAdded <- format(Sys.time(), "%Y-%m-%d %T")

##     Map(importOneGTF, gtf=gtf, rdata=rdata, title=title,
##         species=species, genome=genome, description=description,
##         sourceUrl=sourceUrl, sourceVersion=sourceVersion,
##         MoreArgs=list(ahroot=ahroot, rDataDateAdded=rDataDateAdded))
## }

## function to make one track into an AHM
## .importOneTrack <- function(ahroot, gtf, rdata, species, genome, title,
##                            description, sourceUrl, sourceVersion,
##                            rDataDateAdded)
## {
##     message(gtf)
##     x = AnnotationHubMetadata(
##       AnnotationHubRoot = ahroot,
##       SourceFile = gtf,
##       Species = species,
##       Genome = genome,
##       Title = title,
##       Recipe = "recipe_ensembl_gtf",
##       Description = description,
##       SourceUrl = sourceUrl,
##       RDataClass = "GRanges",
##       RDataVersion = "0.0.1",
##       SourceVersion = sourceVersion,
##       Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
##       DataProvider = "ftp.ensembl.org",
##       Coordinate_1_based = TRUE,
##       RDataDateAdded = rDataDateAdded,
##       Tags = c("GTF", "ensembl", "Gene", "Transcript", "Annotation"))
##     postProcessMetadata(ahroot, metadata(x)$RDataVersion,
##                         metadata(x)$SourceFile)
## }


## newResources method should call these helpers
## list() is a list of AHMs, the idea being that if these are not
## null, then they could be used as a filter for deciding which AHMs
## will be returned by the object.  
## setMethod("newResources", "UCSCTableHubProvider", function(list())
##       {
##           if(length(list)==0){
##               ## then we have to get list from mongo.
##           }

##           ## then call importer on the files for each track.
##           .importUCSCTracks()


##       }
## )

## I need to be able to learn what the files are that are associated
## with each track in an automatic way.  Maybe I can use rtracklayer?
## Or maybe I will just have to look for common names in the dir
## listed?

## ?trackName
## library(rtracklayer)
## session <- browserSession()
## genome(session) <- "hg19"
## trackNames(session)


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
