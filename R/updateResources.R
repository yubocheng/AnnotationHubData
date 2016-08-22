### =========================================================================
### updateResources() and friends
### -------------------------------------------------------------------------
###

getImportPreparerClasses <- function() {
    subclasses <- names(getClassDef("ImportPreparer")@subclasses)
    dont.use <- c("UCSCFullTrackImportPreparer") ## revisit this
    subclasses[!subclasses %in% dont.use]

}

## FIXME: modify to track errors in hubErrors()
pushMetadata <- function(allAhms, url) {
    flog(INFO, "inserting metadata in db ...")

    jsons <- lapply(allAhms, ahmToJson)
    lapply(jsons, function(x) {
        result <- POST(handle=handle(url), body=list(payload=x))
        print(result)
        print(content(result))
        result
    })
}

pushResources <- function(allAhms, hubroot, uploadToS3=TRUE) {
    flog(INFO, "processing and pushing data ...")
    res <- lapply(allAhms, 
        function(xx) {
            tryCatch({
                runRecipes(xx, hubroot=hubroot, ..., uploadToS3=uploadToS3)
                xx
            }, error=function(err) {
                msg <- paste0("error in runRecipes():", conditionMessage(err))
                hubError(xx) <- msg 
                flog(ERROR, msg) 
                xx
            })
        })
    res 
}

downloadResource <- function(ahm, downloadIfExists) {
    SourceUrl <- metadata(ahm)$SourceUrl
    RDataPath <- metadata(ahm)$RDataPath
    AnnotationHubRoot <- metadata(ahm)$AnnotationHubRoot
    flog(INFO, "in downloadResource(), url is : %s", SourceUrl)

    ## the [1] assumes all files in this resource have the same protocol:
    protocol <- tolower(URL_parts(SourceUrl)[, 'protocol'])[1]
    filename <- basename(URL_parts(SourceUrl)[, 'path'])

    ## create local directory
    destdir <- dirname(outputFile(ahm))
    for (dir in unique(destdir))
        if (!dir.exists(dir))
            dir.create(dir, recursive=TRUE)
    destfile <- file.path(destdir, filename)

    ## file exists
    if (file.exists(destfile) && (!downloadIfExists)) {
        flog(INFO, "%s exists, skipping...", destfile)
        return()
    }

    if (protocol == "rtracklayer")
        return ## recipe will download
    if (protocol == "ftp") {
        oldwd <- getwd()
        on.exit(setwd(oldwd))
        setwd(destdir)
        ## FIXME: not documented, no arg for user to pass in,
        ##        number of cores should not exceed number of files
        pieces <- detectCores()
        args <- sprintf("-e 'pget -n %s %s;quit'", pieces, SourceUrl)
        tryCatch({
            system2("lftp", args)
        }, error=function(e){
                flog(ERROR, "Error downloading %s: %s",
                    SourceUrl, conditionMessage(e))
        })
    } else if (protocol %in% c("http", "https")) {
        ## FIXME be more sophisticated in deciding how to download
        ## (e.g. use parallel download for bigger files)
        tryCatch({
            download.file(SourceUrl, destfile, quiet=TRUE)
        }, error=function(e) {
            flog(ERROR, "Error downloading %s: %s",
                 SourceUrl, conditionMessage(e))
        })
    }
}

## adapted from formerly internal function 'processAhm'

setGeneric("runRecipes", signature="metadata",
    function(metadata, hubroot, ...)
        standardGeneric("runRecipes")
)

setMethod("runRecipes", "AnnotationHubMetadata",
    function(metadata, hubroot,
             bucket = getOption("ANNOTATION_HUB_BUCKET_NAME", "annotationhub"),
             download = TRUE, uploadToS3 = TRUE, ...)
    {
        ## FIXME: (1) use of 'download' unclear
        ##        (2) HubRoot / AnnotationHubRoot should already be set
        metadata(metadata)$AnnotationHubRoot <- hubroot
        metadata(metadata)$HubRoot <- hubroot

        ## TODO: better way needed for deciding this:
        provider <- metadata(metadata)$DataProvider
        if (grepl("http://inparanoid", provider, fixed=TRUE) ||
            grepl("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/", provider, fixed=TRUE))
        {
            download <- FALSE
        }

        ## download
        if (download)
            downloadResource(metadata, downloadIfExists=FALSE)

        ## run recipe
        tryCatch({
            metadata <- run(metadata)
        }, error=function(e) {
            flog(ERROR, "error processing %s: %s", 
                 basename(metadata(metadata)$SourceUrl),
                 conditionMessage(e))
        })

        ## upload to S3
        if (uploadToS3) {
            fileToUpload <- file.path(metadata(metadata)$HubRoot,
                                      metadata(metadata)$RDataPath)
            remotePath <- sub("^/", "", metadata(metadata)$RDataPath)
            res <- upload_to_S3(fileToUpload, remotePath, bucket, ...)
            ## If successful, delete local file
            system(paste0("rm ", fileToUpload))
        }
    }
)

updateResources <- function(AnnotationHubRoot, BiocVersion=biocVersion(),
                            preparerClasses=getImportPreparerClasses(),
                            metadataOnly=TRUE, insert=FALSE,
                            justRunUnitTest=FALSE, ...) {

    if (insert) {
        if(is.null(url <- getOption("AH_SERVER_POST_URL")))
            stop(wmsg(paste0("When 'insert=TRUE' option AH_SERVER_POST_URL ",
                             "must be set in .Rprofile")))
    }

    ## create metadata by invoking newResources() method
    allAhms <- list()
    for (preparerClass in preparerClasses)
    {
        flog(INFO, "Preparer Class: %s", preparerClass)
        if (exists(preparerClass)) {
            args <- list()
            if ("annotationHubRoot" %in% names(formals(preparerClass)))
                args$annotationHubRoot <- AnnotationHubRoot
            preparerInstance <- do.call(preparerClass, args)

        } else {
            preparerInstance <- do.call(new, list(preparerClass))
            ## NOTE: 'currentMetadata' arg is in generic but not used
            ahms <- newResources(preparerInstance, 
                                 justRunUnitTest=justRunUnitTest,
                                 BiocVersion=package_version(BiocVersion), ...)
            allAhms <- append(allAhms, ahms)
        }
    }

    ## download, process and push data to appropriate location
    if (!metadataOnly)
        allAhms <- pushResources(allAhms, AnnotationHubRoot, ...)


    ## if data push was successful insert metadata in db
    if (insert)
        pushMetadata(allAhms, url)

    allAhms
}


###########################################################################
## The following was basically a bust because it takes
## FAR TOO LONG (like 30 minutes!) to make ~ a million S4 objects.
###########################################################################
## I also need a getCurrentResources() function - can define it here.
## It basically needs to use the existing AHM DB to make all the
## records into AHMs.

## Helper to just define SQL needed for all relevant tables together
.allMajorTablesSQL <- function(){
    SQL <- "SELECT * FROM resources, rdatapaths, biocversions, input_sources,
            recipes, tags  WHERE
            resources.id = rdatapaths.resource_id AND
            resources.id = biocversions.resource_id AND
            resources.id = input_sources.resource_id AND
            resources.recipe_id = recipes.id AND
            resources.id = tags.resource_id"
    SQL
}
## input_sources, rdatapaths, resources, tags, biocversions


## The basic issue here is that this function needs to talk to the
## 'new' version of AnnotationHub (and possibly both versions?)...
getCurrentResources <- function(version){
    ## Call the thing in AnnotationHub that gets a DB conn to the cached meta

    ah <- AnnotationHub()
    con <- AnnotationHub:::.db_connection(ah)
    ## Then send a massive SQL query to extract all the metadata as
    ## one horrific data.frame
    SQL <- .allMajorTablesSQL()
    meta <- dbGetQuery(con, SQL)
    ahroot <- rep("NA", times=dim(meta)[1])
    tags <- rep("NA", times=dim(meta)[1]) ## leave tags out (for now)
    ## Then call Map and pass in the columns from the data.frame
    message("Generating all existing AnnotationHubMetadata objects.  This will take a long time")
    ## This might be too slow a way to do this...
    ## Might want instead to just check certain fields and not use objects for this.
    Map(AnnotationHubMetadata,
        AnnotationHubRoot=ahroot,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceurl,
        SourceVersion=meta$sourceversion, ##
        Species=meta$species,
        TaxonomyId=meta$taxonomyid,
        Title=meta$title,
        RDataPath=meta$rdatapath,
        Coordinate_1_based = as.logical(meta$coordinate_1_based),
        DataProvider = meta$dataprovider,
        Maintainer = meta$maintainer,
        RDataClass = meta$rdataclass,
        Recipe = meta$recipe,
        Tags = tags)
}





##########################################################################
## Need a helper to delete one record.  I could do this as a direct DB
## query, but Dan already has ruby code that does this, so we will
## recycle that functionality like so:

.removeOneRecord <- function(id){
    if(!exists('id')){
        stop("You can't delete a record randomly. You need an id.")}
    if(!is.integer(id)){stop("id must be an integer")}
    url <- paste0("http://gamay:9393/resource/",id)
    DELETE(url)
}

## and a vectorized function for removing records
deleteResources <- function(id) {
    sapply(id, function(x){
        .removeOneRecord(as.integer(sub("AH", "", x)))
    })
}



##########################################################################
## Need another helper to help clean up the tables after redundant records
## have been inserted.
## This will need to happen to several tables, so I basically need to go
## through the following tables and remove redundant fields.
## input_sources, rdatapaths, resources, tags, biocversions


## SELECT * FROM (SELECT id, tag, resource_id FROM tags WHERE resource_id=7226) AS tags GROUP BY tags.tag, tags.resource_id HAVING count(tags.id) > 1  limit 10;
## This is pretty close to what I would need here:
## SELECT * FROM (SELECT id, tag, resource_id FROM tags WHERE resource_id=7226) AS tags GROUP BY tags.tag, tags.resource_id  limit 10;

## But it feels like the operation to remove dups might be easier if done in R?  (using duplicated)
## That is, I could pull the contents into R, ID the dups and then the query is more straightforward to debug.

## So the following code should extract what I need for a test set

## Helper to get connection to the roundup DB
.getAnnotHubCon <- function(){
    pswd <- getOption("gamayAHUserPSWD")
    if(!exists('pswd')){
        stop("You need to set a password for the issue tracker in .Rprofile")
    }

    dbConnect(dbDriver('MySQL'),
              host='localhost',  ## we always will run this locally anyhow
              dbname='annotationhub',
              user='ahuser',
              pass=pswd)
}

## helper to clean one table from MySQL DB (on gamay)
## This is for cases where a table (or some tables have become contaminated with
## redundant entries...)
## For the resources table this function may not be strict enough someday IF
## someday we get to the point where we have records that are identical 'except'
## for say their sourceURLs (for example).  But it should be plenty picky for now

## BUT: lets be super-duper safe ANYWAYS and (when considering resources), lets
## join all the relevant tables together and look at all that data.

## helper that returns ids that have been duplicated.
## Do I want to add argument to return "sets" of dups? - yes I DO
.getDuplicatedRowIds <- function(res, getALLIds=FALSE){
    ## work out which rows are duplicated
    colIdx <- !(colnames(res) %in% 'id')
    idColIdx <- colnames(res) %in% 'id'
    if(getALLIds){
        idx1 <- duplicated(res[,colIdx])
        idx2 <- duplicated(res[,colIdx],fromLast=TRUE)
        idx <- idx1 | idx2
    }else{
        idx <- duplicated(res[,colIdx])
    }
    ids <- res[idx,'id'] ## yes, we always want just the column named 'id'
    ids
}

.getOtherTableDupIDs <- function(tbl, ids, con){
    idsFmt <- paste(ids,collapse="','")
    res <- dbGetQuery(con, paste0("SELECT * FROM ",tbl," WHERE resource_id IN ('",idsFmt,"')"))
    .getDuplicatedRowIds(res, getALLIds=TRUE)
}


.cleanOneTable <- function(tbl, reallyDeleteRows=FALSE){
    con <- .getAnnotHubCon()
    ## And if tbl is 'resources' then we have to ignore two columns (not just one)
    ## And this also means a much 'larger' query.
    if(tbl=='resources'){
        res <- dbGetQuery(con, "SELECT * FROM resources")
        res <- res[,!(colnames(res) %in% c('ah_id'))]
        #dids <- .getDuplicatedRowIds(res)
        ids <- .getDuplicatedRowIds(res,getALLIds=TRUE)
        ## Now we have to check the other tables...
        ## TODO (lapply through rdatapaths, input_sources, tags, biocversions
        ##res2 <- .getOtherTableDupIDs('rdatapaths',ids)
        tables <- c('rdatapaths', 'input_sources', 'tags', 'biocversions')
        AllDupIds <- lapply(tables, .getOtherTableDupIDs, ids=ids, con=con)
        ## Then take do the intersect for each of the list elements on the next one over...
        if(any(sapply(AllDupIds,function(x){length(x)==0}))){
            message("No duplicates are possible for resources.")
        }else{
            message("Bad news: you will have to add more code to .cleanOneTable()
                    to intersect and filter the ids that have been found to be
                    duplicated in all the relevant sub tables. for tidying of
                    the resources table.")
            ## ADD code to intersect ids for all the sub tables here, and then
            ## filter it using dids (commented above)
        }
    }else{
        sql <- paste0("SELECT * FROM ",tbl)
        res <- dbGetQuery(con, sql)
        ids <- .getDuplicatedRowIds(res)
        idsFmt <- paste(ids,collapse="','")
        message('We just found ',length(ids),' duplicated records from the ', tbl, ' table.')
    }
    ## then delete things (if appropriate)
    if(reallyDeleteRows && length(ids)>0){
        if(tbl=='resources'){
            deleteResources(ids)
        }else{
            sql2 <- paste0("DELETE FROM ",tbl," WHERE id IN ('",idsFmt,"')")
            dbGetQuery(con, sql2)
        }
    }
}

## Then call the above function twice on each table that we want to clean.
## 1st to see if if finds anything and then again to really remove duplicates...
## usage: AnnotationHubData:::.cleanOneTable('tags') ## Doing this only revealed dups in 'tags' table.
## usage: AnnotationHubData:::.cleanOneTable('tags', reallyDeleteRows=TRUE)

## library(AnnotationHub);debug(AnnotationHubData:::.cleanOneTable);AnnotationHubData:::.cleanOneTable('resources')

## usage: AnnotationHubData:::.cleanOneTable('resources') ## Doing this only revealed dups in 'tags' table.

