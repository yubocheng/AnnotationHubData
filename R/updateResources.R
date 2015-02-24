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
    if(!file.exists(dirname(outputFile(ahm)))){
        dir.create(dirname(outputFile(ahm)), recursive=TRUE)
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
        ## AskDan:: What is downloadIfExists?
        ## AskDan:: What is insertOnlyIfRDataExists? - only used by
        ## insertAHM (which we bypass)
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

## Here is the main function it is responsible for:
## 1) spawning the AHMs
## 2) making them into JSON
## 3) send metadata off to the back end
## 4) call the recipe and push the results of that off to the right place

## pre-steps: get all the existing resources. For now - just an
## empty list, but later I want to call a version of
## getExistingResources(), (getCurrentResources).
## listOfExistingResources <- list()
## listOfExistingResources <- getCurrentResources(BiocVersion)

updateResources <- function(ahroot, BiocVersion,
                            preparerClasses=getImportPreparerClasses(),
                            listOfExistingResources=list(),
                   ## listOfExistingResources=getCurrentResources(BiocVersion),
                            insert=FALSE, metadataOnly=TRUE,
                            filtering=TRUE){
    
    ## 1 spawning the AHMs is about calling the newResources method
    ## defined for them.  The newResources method takes a class that
    ## it needs to spawn up AHMs for, AND a list of existing AHMs to
    ## not spawn (and it returns the list of AHMs that don't exist
    ## already).
    ## So should look like this
    allAhms <- list()
    for (preparerClass in preparerClasses)
    {
        flog(INFO, "Preparer Class: %s", preparerClass)
        if (exists(preparerClass))
        {
            args <- list()
            if ("annotationHubRoot" %in% names(formals(preparerClass)))
                args$annotationHubRoot <- ahroot
            preparerInstance <- do.call(preparerClass, args)

        } else {
            preparerInstance <- do.call(new, list(preparerClass))
            ahms <- newResources(preparerInstance, listOfExistingResources)
            allAhms <- append(allAhms, ahms)
        }
    }
    
    ## 1.5 Filter out allAhms that already exist in the DB!
    ## So this function needs to take the allAhms thing from above and then
    ## remove any of the AHMs that are already in the DB.  This requires that I 
    ## have an updated DB already, which I can now get thanks to an upgraded 
    ## AnnotationHub ()
    if(filtering==TRUE){
        allAhms <- filterAHMs(allAhms)
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
## Now I need a function that will 1) take a list of AHMs, 
## 2) thow away ones that we already have and then 
## 3) return only the AHMs that are 'new'
## For now: do the filtering based on the rdatapath, but as we get more 
## sophisticated, I think we should start to filter based on MD5 
## (and make one if not present) to ensure that things match (when relevant)etc.

## UPDATED PLAN TO include both version AND name.  1st step: get all rows that match the name.  2nd step: see if the version number and name match for each.  If there is a match for both then return


filterAHMs <- function(ahms){
    ## we need certain data from the DB (but not *all* of it)
    require(RSQLite)
    require(AnnotationHub)
    ah <- AnnotationHub()
    conn <- ah@.db_connection
    ## compared ahm@RDataVersion[1] and ahm@SourceUrl[1]

    ## One option is that I could just get key data out of SQLite
    SQL <- paste0("SELECT res.ah_id, res.rdatadateadded, ",
                  "iso.sourceurl, bcv.biocversion ",
                  "FROM resources AS res, input_sources AS iso, ",
                  "biocversions AS bcv ",
                  "WHERE res.id=iso.resource_id AND res.id=bcv.resource_id") 
    existingMetadata <- dbGetQuery(conn, SQL)
    
    ## helper to test if something was NOT in existingMetadata
    .isNew <- function(ahm, em){
        ahmSrc <- ahm@SourceUrl[1]
        ahmBcv <- ahm@BiocVersion
        ## Work out whether true or false: "have we seen this before?"
        ## TODO: 1st subset based on srcUrl and then check the other things.
        ## Below can't work because the question needs to be asked for each specific AHM.
        ## IOW they have to *all* match (or not)        
        
        ## next change this so that the checked values are on the same row.
        ## res <- ahmSrc %in% em$sourceurl ## & ahmBcv %in% em$biocversion

        ## BUT FOR NOW:
        ## For just getting the ensembl fasta files, the following should be enough:
        res <- ahmSrc %in% em$sourceurl

        ## But it won't be enough for *everything* (UCSC would end up with no updates)
        
        res
    }
    
    ## Then I could compare this data to my list of AHMs using an lapply 
    idx <- unlist(lapply(ahms, FUN=.isNew, em=existingMetadata))
    ahms[!idx]
}

## This raises some questions to discuss with Martin: do we want to search on 
## BiocVersion too?  Does that help?  What about rdataversion (which seems 
## like an uninformative field?)  So far the only thing that seems to 
## 'definitely' matter is the sourceUrl...  So why am I extracting this other 
## stuff?

## Consider the Fasta ones: Do we only want to update them if they have changed?  
## Or do we want to just check if we have processed a file called that?
## I think we want to get them again if their sourceMD5 sum has changed.  
## But we don't always have that filled in.

## Basically we need to talk about what the specific goals are for this project 
## in terms of updates for older and newer runs of the recipes.

## When exactly do we want to re-run a recipe?


###############################################################
## So after talking with Martin there are a few things to consider
## 1) Not all resoures are the same scenario (UCSC vs ensembl -> not versioned vs versioned resources)
## 2) Some of our resources are NOT stored in S3 (and therefore can't be versioned ever).  Like chain files.  What the user gets will always just be what is online.  Does this mean that we should not make new records (or always make them?)
## 3) Is a UCSC file that is out of date call for a new record?  What about an ensembl file?  How about a chain file?  How do we know what the rules will be for each case?  Do we need to indicate this in the metadata?  Or just follow a heuristic?


## Talk to Dan and then discuss it with Martin to get clear rules/specifications in place...








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
    require(RSQLite) 
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


## Unfortunately: Martins MyHub prototype is probably missing some
## stuff, and is also depending on the back end to create a sqlite DB
## port for it ahead of time (which it then downloads as needed).  But
## the current backend on gamay doesn't do that yet.





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
    require(RMySQL)
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

