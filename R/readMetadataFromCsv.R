### =========================================================================
### readMetadataFromCsv()
### -------------------------------------------------------------------------
###

## High level helper used to check metadata in 'Hub' packages.
readMetadataFromCsv <- function(pathToPackage) 
{
    meta <- read.csv(file.path(pathToPackage, "inst/extdata/metadata.csv"),
                     colClasses="character", stringsAsFactors=FALSE)
    ## Check columns
    mat <- rbind(c("Title", "character"),
                 c("Description", "character"),
                 c("BiocVersion", "character"),
                 c("Genome", "character"),
                 c("SourceType", "character"),
                 c("SourceUrl", "character"),
                 c("SourceVersion", "character"),
                 c("Species", "character"),
                 c("TaxonomyId", "integer"),
                 c("Coordinate_1_based", "logical"),
                 c("DataProvider", "character"),
                 c("Maintainer", "character"),
                 c("RDataClass", "character"),
                 c("DispatchClass", "character"),
                 c("ResourceName", "character"))

    expected <- mat[,1]
    missing <- !expected %in% names(meta)
    if (any(missing))
        stop(paste0("missing fields in metadata.csv: ", 
                    paste(expected[missing], collapse=", ")))
    extra<- !names(meta) %in% expected 
    if (any(extra))
        message(paste0("extra fields in metadata.csv will be ignored: ", 
                    paste(names(meta)[extra], collapse=", ")))

    ## All fields length 1
    apply(meta, 1, 
        function(xx) {
            valid <- sapply(xx, function(field) length(field) == 1L)
            if (any(!valid))
                stop(paste0("all fields in metadata.csv must be a character ",
                     "string of length 1"))
        }
    )
}
