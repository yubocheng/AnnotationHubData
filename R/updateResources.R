## This is where the new functions for updating resources on the new
## back end are going to live


.pushMetadata <- function(jsons,
                          handle=handle("http://gamay:9393/new_resource")){
    lapply(jsons, function(x) {
        result <- POST(handle=handle, body=list(payload=x))
        print(result)
        result
    })
}

## this helper is adapted from formerly internal function 'processAhm'
.runRecipes <- function(ahm){
    metadata(ahm)$AnnotationHubRoot <- ahroot
    needs.download <- TRUE ## FIXME
    provider <- metadata(ahm)$DataProvider
    ## TODO: better way needed for deciding this:
    if (grepl("http://inparanoid", provider, fixed=TRUE) ||
        grepl("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/", provider, fixed=TRUE))
    {
        needs.download <- FALSE
    }
    
    if (needs.download)
    {
        downloadResource(ahm, downloadIfExists)
## AskDan:: What is downloadIfExists?
## AskDan:: What is insertOnlyIfRDataExists?
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

updateResources <- function(ahroot, BiocVersion,
                            preparerClasses=getImportPreparerClasses(),
                            insert=TRUE, metadataOnly=FALSE){

    ## pre-steps: get all the existing resources (for now - just an
    ## empty list, but later I want to call a version of
    ## getExistingResources())
    listofExistingResources <- list()    
    
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

        } else
            preparerInstance <- do.call(new, list(preparerClass))
        ahms <- newResources(preparerInstance, existingResources)
        allAhms <- append(allAhms, ahms)
    }
    
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

## AskDan: What was metadataFilter?
    
    return(allAhms)
}


