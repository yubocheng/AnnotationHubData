txdb <- GenomeInfoDb::loadTaxonomyDb()
txdb <- rbind(txdb, c(NA, NA, ""))

test_getSpeciesList <- function(){
    list <- getSpeciesList()
    checkTrue(length(list) == dim(txdb)[1])
}

test_validSpecies <- function(){

    checkTrue(validSpecies("Homo sapiens", verbose=FALSE))
    checkTrue(!validSpecies("Homo Sapiens", verbose=FALSE))
    checkTrue(validSpecies(NA_character_))
}

test_suggestSpecies <- function(){

    vl1 <- Reduce(`|`, lapply(txdb[2:3], grepl, pattern = "Dictyoglomus",
        ignore.case=TRUE))
    vl2 <- Reduce(`|`, lapply(txdb[2:3], grepl, pattern = "immobile",
        ignore.case=TRUE))

    out <- suggestSpecies(c("Dictyoglomus", "immobile"))
    checkTrue((length(which(vl1)) + length(which((vl2)))) == dim(out)[1])
}

test_validTaxId <- function(){

    checkTrue(is.null(AnnotationHubData::checkSpeciesTaxId(9606,
                                                           "Homo sapiens")))
    options(warn=2)
    checkException(AnnotationHubData::checkSpeciesTaxId(9999, "Homo sapiens"))
    options(warn=0)
}
