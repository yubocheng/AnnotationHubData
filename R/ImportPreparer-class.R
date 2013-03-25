## these classes are used for dispatch only

setClass("ImportPreparer", representation="VIRTUAL")

setMethod(show, "ImportPreparer", function(object) {
    cat("class:", class(object), "\n")
})

setGeneric("newResources", signature="importPreparer",
    function(importPreparer, currentMetadata = list(), ...)
        standardGeneric("newResources")
)
