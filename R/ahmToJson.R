## Code for creating json records from the sqlite DB.

## RIGHT NOW the json looks sort of record centric.  So I need to make
## code that takes AHMs and makes them into JSON.


## So 1st I need an exemplar AHM (will save one for now in inst/extdata

## So for testing: 
## load(system.file('extdata','inpDrosPsuedo.rda', package='AnnotationHubData'))
## ahm


## try to make to JSON using rjson
## library(rjson)
## foo <- metadata(ahm)  ## makes a list
## toJSON(foo)           ## close? - makes something that at least looks ok-ish.


## Dan suggests jsonlite
## library(jsonlite)
## toJSON(foo)
## Has problems with some of the types...

ahmToJson <- function(ahm){
    lst <- metadata(ahm)
    require('jsonlite')
    ## casting on elements that toJSON can't handle
    lst[[2]] <- as.character(lst[[2]])
    lst[[15]] <- as.character(lst[[15]])
    ## lower case all the names
    names(lst) <- tolower(names(lst))

    tagList <- as.list(lst[['tags']])
    names(tagList) <- rep('tag', length(tagList))
    versList <- as.list(lst[['biocversion']])
    names(versList) <- rep('biocversion', length(versList))

    
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
                 versions=list(
                      rdataversion=lst[['rdataversion']],
                      rdatadateadded=lst[['rdatadateadded']]
                      ),
                 rdatapaths=list(
                      rdatapath=lst[['rdatapath']],
                      rdataclass=lst[['rdataclass']],
                      rdatasize=lst[['rdatasize']]
                      ),
                 input_sources=list(
                      sourcefile=lst[['sourcefile']],
                      sourcesize=lst[['sourcesize']],
                      sourceurl=lst[['sourceurl']],
                      sourceversion=lst[['sourceversion']]
                      ),
                 tags=tagList,
                 biocversions=versList,
                 recipes=list(
                      recipe=lst[['recipe']][1],
                      package=lst[['recipe']][[2]] ##,
##                      recipeargs=lst[['recipeargs']] ## remove this from class
                      )
                 )
    
    ## then make JSON
    toJSON(base, auto_unbox=TRUE)
    ## STILL: some issues here with no boxing where we want it (around
    ## sub-sets like 'versions'
    ## AND: some name-mangling in the tags...
    
}

