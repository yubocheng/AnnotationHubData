## these classes are used for dispatch only

setClass("DataProvider")                # VIRTUAL

EncodeDCCProvider <- 
    setClass("EncodeDCCProvider", contains="DataProvider")

EnsemblGtfProvider <-
    setClass("EnsemblGtfProvider", contains="DataProvider")

UCSCTableHubProvider <-
    setClass("UCSCTableHubProvider", contains="DataProvider")

setMethod(show, "DataProvider", function(object) {
    cat("class:", class(object), "\n")
})

setGeneric("newResources",
    function(object, ahmeta) standardGeneric("newResources"))
