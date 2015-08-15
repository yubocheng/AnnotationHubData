################################################################################
## This is where the new functions for updating resources on the new
## back end are going to live
################################################################################
## After metadata is pushed up to the back end.  The back end will then do 
## the following to 'fetch' records based on their IDs:
## 
# get '/fetch/:id' do
#   rp = Rdatapath.find(:id=>params[:id])
#   path = rp.rdatapath
#   resource = rp.resource
#   prefix = resource.location_prefix.location_prefix
#   url = prefix + path
#   # TODO do some logging here....
#   redirect url
# end
##
## That means that the formula for looking up a resource will always be :
## location_prefix + rdatapath
################################################################################

getImportPreparerClasses <- function()
{
    subclasses <- names(getClassDef("ImportPreparer")@subclasses)
    dont.use <- c("UCSCFullTrackImportPreparer") ## revisit this
    subclasses[!subclasses %in% dont.use]

}


.pushMetadata <- function(jsons,
                          url=getOption("AH_SERVER_POST_URL")){
    if(is.null(getOption("AH_SERVER_POST_URL"))){
        stop(wmsg(paste0("If you really want to push to the server then you",
                         " need to set option AH_SERVER_POST_URL in .Rprofile",
                         ", otherwise use insert=FALSE.")))
    }
    h = handle(url)
    lapply(jsons, function(x) {
        result <- POST(handle=h, body=list(payload=x))
        print(result)
        print(content(result))
        result
    })
}

## this helper is adapted from formerly internal function 'processAhm'
.runRecipes <- function(ahm){
    metadata(ahm)$AnnotationHubRoot <- ahroot
    needs.download <- TRUE ## FIXME
    provider <- metadata(ahm)$DataProvider
    ## Make sure we always have a local dir for saving too (based on
    ## contents of the outputFile(ahm)
    localDir <- unique(dirname(outputFile(ahm)))
    if(!file.exists(localDir)) {
        dir.create(localDir, recursive=TRUE)
    }
    
    ## TODO: better way needed for deciding this:
    if (grepl("http://inparanoid", provider, fixed=TRUE) ||
        grepl("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/", provider, fixed=TRUE))
    {
        needs.download <- FALSE
    }
    
    if (needs.download)
    {
        ## downloadResource(ahm, downloadIfExists)
        downloadResource(ahm, downloadIfExists=FALSE)
    }
    needs.recipe <- TRUE ## FIXME
    if (needs.recipe)
        ## FIXME this means we might need to download
        ## but not run recipe. Don't understand how that could happen.
    {
        tryCatch(ahm <- run(ahm), error=function(e){
            flog(ERROR, "error processing %s: %s",
                 metadata(ahm)$SourceUrl,
                 conditionMessage(e))
        })
        ## upload to S3
        ## dante
        if (!getOption("AnnotationHub_Use_Disk", FALSE))
        {
            fileToUpload <- file.path(metadata(ahm)$AnnotationHubRoot,
                                      metadata(ahm)$RDataPath)
            remotePath <- sub("^/", "", metadata(ahm)$RDataPath)
            res <- upload_to_S3(fileToUpload, remotePath)
            ## TODO - if download is successful, delete local file?
        }
    }
}

## Here is the main function and what it is responsible for:
## 1) spawning the AHMs
## 2) making them into JSON
## 3) send metadata off to the back end
## 4) call the recipe and push the results of that off to the right place

updateResources <- function(AnnotationHubRoot, BiocVersion=biocVersion(),
                            preparerClasses=getImportPreparerClasses(),
                            insert=FALSE, metadataOnly=TRUE,
                            justRunUnitTest=FALSE){
    
    ## 1 spawning the AHMs is about calling the newResources method
    ## defined for them.  The newResources method takes a class that
    ## it needs to spawn up AHMs for, AND a list of existing AHMs to
    ## not spawn (and it returns the list of AHMs that don't exist
    ## already).
    ## So should look like this
    BiocVersion <- package_version(BiocVersion)
    allAhms <- list()
    for (preparerClass in preparerClasses)
    {
        flog(INFO, "Preparer Class: %s", preparerClass)
        if (exists(preparerClass))
        {
            args <- list()
            if ("annotationHubRoot" %in% names(formals(preparerClass)))
                args$annotationHubRoot <- AnnotationHubRoot
            preparerInstance <- do.call(preparerClass, args)

        } else {
            preparerInstance <- do.call(new, list(preparerClass))
            ahms <- newResources(preparerInstance, listOfExistingResources,
                                 justRunUnitTest=justRunUnitTest,
                                 BiocVersion=BiocVersion)
            allAhms <- append(allAhms, ahms)
        }
    }
    
    ## Running this function with filtering=FALSE for some older recipes
    ## usually resuilts in no AHMs, which raises the question of
    ## whether or not filterAHMs is really even needed?
    ## I think is is necessary since we can't rely on the recipe to
    ## filter out existing records for us.
    
    ## 2 make into JSON
    jsons = lapply(allAhms,ahmToJson)

    ## 3 send metadata off
    if(insert==TRUE){
        .pushMetadata(jsons)
    }
    
    ## 4 call the recipes
    if(metadataOnly==FALSE){
        lapply(allAhms, .runRecipes)
    }

    
    return(allAhms)
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


downloadResource <- function(ahm, downloadIfExists)
{
    with(metadata(ahm), {
        flog(INFO, "in downloadResource(), url is : %s", SourceUrl)
        ## the [1] assumes that all files in this resource
        ## have the same protocol:
        protocol <- tolower(URL_parts(SourceUrl)[,'protocol'])[1]
        filename <- basename(URL_parts(SourceUrl)[,'path'])
##      dest0 <- file.path(tempdir(), AnnotationHubRoot, RDataPath) ## new
        dest0 <- file.path(AnnotationHubRoot, RDataPath) ## ori/works
        destdir <- dirname(dest0) 
        dir.create(unique(destdir), recursive=TRUE) ## new
        destfile <- file.path(destdir, filename)

        if (file.exists(destfile) && (!downloadIfExists))
        {
            flog(INFO, "%s exists, skipping...", destfile)
            return()
        }

        for (dir in unique(destdir))
            if (!file.exists(dir))
                dir.create(dir, recursive=TRUE)
        if (protocol == "rtracklayer")
            return ## recipe will do necessary downloading
        if (protocol == "ftp")
        {
            oldwd <- getwd()
            on.exit(setwd(oldwd))
            setwd(destdir)
            pieces <- detectCores()
            ## FIXME - this leads to oversubscription if
            ## parallelCores is not NULL in updateAllResources
            args <- sprintf("-e 'pget -n %s %s;quit'",
                pieces, SourceUrl)
            tryCatch(ret <- system2("lftp", args),
                error=function(e){
                    flog(ERROR, "Error downloading %s: %s",
                        SourceUrl, conditionMessage(e))
                    })
            if (!getOption("AnnotationHub_Use_Disk", FALSE))
            {
                res <- upload_to_S3(destfile, RDataPath)
                # TODO - if download is successful, delete local file?
            }
        } else if (protocol %in% c("http", "https")){
            ## FIXME be more sophisticated in deciding how to download
            ## (e.g. use parallel download for bigger files)
            tryCatch(download.file(SourceUrl, destfile, quiet=TRUE),
                error=function(e){
                    flog(ERROR, "Error downloading %s: %s",
                        SourceUrl, conditionMessage(e))
                    })
            # if (!getOption("AnnotationHub_Use_Disk", FALSE))
            # {
            #     fileToUpload <- file.path(AnnotationHubRoot, RDataPath)
            #     res <- upload_to_S3(fileToUpload, sub("^/", "", RDataPath))
            #     # TODO - if download is successful, delete local file?
            # }
        }
    })
}

