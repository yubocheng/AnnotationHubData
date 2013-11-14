## these classes are used for dispatch only

setClass("ImportPreparer", representation="VIRTUAL")

setMethod(show, "ImportPreparer", function(object) {
    cat("class:", class(object), "\n")
})

setGeneric("newResources", signature="importPreparer",
    function(importPreparer, currentMetadata = list(), ...)
        standardGeneric("newResources")
)

setGeneric("annotationHubRoot", signature="object",
           function(object)
           standardGeneric("annotationHubRoot"))

setGeneric("metadataList", signature="object",
           function(object)
           standardGeneric ("metadataList"))

setGeneric("metadataTable", signature="object",
           function(object)
           standardGeneric ("metadataTable"))

setGeneric("sourceUrls", signature="object",
           function(object)
           standardGeneric("sourceUrls"))

