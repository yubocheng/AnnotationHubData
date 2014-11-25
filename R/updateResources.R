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
                            insert=FALSE, metadataOnly=TRUE){
    
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
    ## allAhms <- filterAHMs(allAhms)
    
    
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

filterAHMs <- function(ahms){
    ## we need certain data from the DB (but not *all* of it)
    require(RSQLite)
    ah <- AnnotationHub()
    conn <- ah@.db_connection
    ## compared ahm@RDataVersion[1] and ahm@SourceUrl[1]

    ## One option is that I could just get key data out of SQLite
    SQL <- paste0("SELECT res.ah_id, res.rdatadateadded, res.rdataversion, ",
                  "iso.sourceurl, bcv.biocversion ",
                  "FROM resources AS res, input_sources AS iso, ",
                  "biocversions AS bcv ",
                  "WHERE res.id=iso.resource_id AND res.id=bcv.resource_id") 
    existingMetadata <- dbGetQuery(conn, SQL)
    
    ## helper to test if something was NOT in existingMetadata
    .isNew <- function(ahm, em){
        ahmVers <- ahm@RDataVersion[1]
        ahmSrc <- ahm@SourceUrl[1]
        ahmBcv <- ahm@BiocVersion
        ## Work out if true or false that we "have seen this before?"
        ## TODO: 1st subset based on srcUrl and then check the other things.
        res <- ahmVers %in% em$rdataversion & ahmSrc %in% em$sourceurl & 
                ahmBcv %in% em$biocversion
        res        
    }
    
    ## Then I could compare this data to my list of AHMs using an lapply 
    idx <- lapply(ahms, FUN=.isNew, em=existingMetadata)
    ahms[idx]
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











###########################################################################
## The following was basically a bust because it takes
## FAR TOO LONG (like 30 minutes!) to make ~ a million S4 objects.
###########################################################################
## I also need a getCurrentResources() function - can define it here.
## It basically needs to use the existing AHM DB to make all the
## records into AHMs.

## The basic issue here is that this function needs to talk to the
## 'new' version of AnnotationHub (and possibly both versions?)...
getCurrentResources <- function(version){
    ## Call the thing in AnnotationHub that gets a DB conn to the cached meta
    require(RSQLite) 
    ah <- AnnotationHub()
    con <- AnnotationHub:::.db_connection(ah)
    ## Then send a massive SQL query to extract all the metadata as 
    ## one horrific data.frame
    SQL <- "SELECT * FROM resources, rdatapaths, biocVersions, input_sources, 
            recipes, tags  WHERE 
            resources.id = rdatapaths.resource_id AND 
            resources.id = biocVersions.resource_id AND
            resources.id = input_sources.resource_id AND
            resources.recipe_id = recipes.id AND
            resources.id = tags.resource_id"
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
        SourceFile=meta$sourcefile, 
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
        RDataDateAdded = meta$rdatadateadded,
        RDataVersion = meta$rdataversion,
        Recipe = meta$recipe,
        Tags = tags)
}


## Unfortunately: Martins MyHub prototype is probably missing some
## stuff, and is also depending on the back end to create a sqlite DB
## port for it ahead of time (which it then downloads as needed).  But
## the current backend on gamay doesn't do that yet.


