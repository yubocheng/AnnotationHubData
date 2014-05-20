## Code for creating json records from the sqlite DB.

## RIGHT NOW the json looks sort of record centric.  So I need to make
## code that takes AHMs and makes them into JSON.


## So 1st I need an exemplar AHM (will save one for now in inst/extdata

## So for testing: 
## load(system.file('extdata','inpDrosPsuedo.rda', package='AnnotationHubData'))
## ahm


## helper to do cleanup and make sure things are present:
cleanupLst <- function(lst){
    if(length(lst[["recipe"]])==1){
        lst[["recipe"]][[2]] <- "AnnotationHubData"
    }
    ## Unfortunately, I have no recipe args (so I can't fix that field)
    lst
}



## Dan suggests jsonlite
ahmToJson <- function(ahm){
    lst <- metadata(ahm)    
    require('jsonlite')
    ## casting on elements that toJSON can't handle
    lst[[2]] <- as.character(lst[[2]])
    lst[[15]] <- as.character(lst[[15]])
    ## lower case all the names
    names(lst) <- tolower(names(lst))
    
    ##TEMP cleanup the ahm (in future we want to stop using this!)
    lst <- cleanupLst(lst)
    
    rdatapaths <- Map(list,
                      rdatapath=lst[['rdatapath']],
                      rdataclass=lst[['rdataclass']],
                      rdatasize=lst[['rdatasize']]
                      )
    ## using Map puts unwanted labels on things...
    names(rdatapaths) <- NULL 

    input_sources <- Map(list,
                         sourcefile=lst[['sourcefile']],
                         sourcesize=lst[['sourcesize']],
                         sourceurl=lst[['sourceurl']],
                         sourceversion=lst[['sourceversion']]
                         )
    ## using Map puts unwanted labels on things...
    names(input_sources) <- NULL 
    
    ## TODO: I need to have Map make lists but not have them be named horribly.
    ## So multiplexed like Map on rdatapaths below, but with result
    ## that looks like input_sources
    
    ## Now just need to re-arrange things a bit
    base <- list(title=lst[['title']],
                 dataprovider=lst[['dataprovider']],
                 species=lst[['species']],
                 taxonomyid=lst[['taxonomyid']],
                 genome=lst[['genome']],
                 description=lst[['description']],
                 coordinate_1_based=lst[['coordinate_1_based']],
                 maintainer=lst[['maintainer']],
                 status=lst[['status']],
                 location_prefix=lst[['location_prefix']],
                 rdataversion=lst[['rdataversion']],
                 rdatadateadded=lst[['rdatadateadded']],
                 recipe=lst[['recipe']][1],
                 recipe_package=lst[['recipe']][[2]],
                 rdatapaths=rdatapaths,                 
                 input_sources=input_sources,
                 tags=lst[['tags']],
                 biocversions=lst[['biocversion']]
                 )
    
    ## then make JSON
    toJSON(base, auto_unbox=TRUE)
    ## STILL: some issues here with no boxing where we want it (around
    ## sub-sets like 'versions'
    ## AND: some name-mangling in the tags...
    
}

