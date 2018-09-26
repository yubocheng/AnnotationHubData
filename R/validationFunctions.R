getSpeciesList <- function(verbose=FALSE){
    if (!requireNamespace("GenomeInfoDbData", quietly = TRUE))
        stop("Requires GenomeInfoDbData.  Please run:\n",
             "    BiocManager::install('GenomeInfoDbData')")
    if (verbose) message("Loading valid species information.")
    txdb <- loadTaxonomyDb()
    species <- trimws(paste(txdb$genus, txdb$species))
}

validSpecies <- function(species, verbose=TRUE){
    speciesList <- getSpeciesList(verbose=verbose)
    res <- species %in% speciesList
    if (any(!res) & verbose){
        message("Found invalid species.\n")
        print(species[!res])
        message("\nFor complete list of acceptable species run\n",
                "    'getSpeciesList()'\n",
                "For suggestions try\n",
                "    'suggestSpecies()'\n")
     }
    all(res)
}

suggestSpecies <- function(query, verbose=FALSE, op=c("|", "&")){
    op = match.arg(op)
    if (!requireNamespace("GenomeInfoDbData", quietly = TRUE))
        stop("Requires GenomeInfoDbData.  Please run:\n",
             "    BiocManager::install('GenomeInfoDbData')")
    if (verbose) message("Loading valid species information.")
    txdb <- loadTaxonomyDb()
    sd <- txdb
    combo <- trimws(paste(txdb$genus, txdb$species))
    sd$combo = combo
    if( op == "|"){
        keep <- FALSE
        for (q in query)
            keep <- keep | Reduce(`|`, lapply(sd[2:4], grepl, pattern = q,
                                              ignore.case=TRUE))
    }else {
        keep <- TRUE
        for (q in query)
            keep <- keep & Reduce(`|`, lapply(sd[2:4], grepl, pattern = q,
                                              ignore.case=TRUE))
    }
    data.frame(taxonomyId = sd$tax_id[keep], species=sd$combo[keep])
}
