## these classes are used for dispatch only

setClass("ImportPreparer")              # VIRTUAL

setMethod(show, "ImportPreparer", function(object) {
    cat("class:", class(object), "\n")
})

setGeneric("newResources",
    function(importPreparer, currentMetadata)
        standardGeneric("newResources")
)
