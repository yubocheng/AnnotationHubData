getSpeciesList <- function(verbose=FALSE){
    if (!requireNamespace("GenomeInfoDbData", quietly = TRUE))
        stop("Requires GenomeInfoDbData.  Please run:\n",
             "    BiocManager::install('GenomeInfoDbData')")
    if (verbose) message("Loading valid species information.")
    txdb <- GenomeInfoDb::loadTaxonomyDb()
    txdb <- rbind(txdb, c(NA, NA, ""))
    species <- trimws(paste(txdb$genus, txdb$species))
    species
}

validSpecies <- function(species, verbose=TRUE){
    speciesList <- getSpeciesList(verbose=verbose)
    res <- species %in% speciesList
    if (any(is.na(species)))
        res[is.na(species)] = TRUE
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

getLicensesList <- function(verbose=FALSE){
    if (verbose) message("Loading valid licenses information.")
    ah <- AnnotationHub()
    conn <- dbConnect(RSQLite::SQLite(), dbname = ah@.db_path)
    licenses <- dbGetQuery(conn, "SELECT DISTINCT SSS FROM licenses")
    licenses <- trimws(licenses$SSS)
    licenses
}

validLicenses <- function(licenses, verbose=TRUE){
    licensesList <- getLicensesList(verbose=verbose)
    licenses <- strsplit(licenses, ":")
    res <- setdiff(licenses, licensesList)
    if (any(is.na(licenses)))
        res[is.na(licenses)] = TRUE
    if (any(!res) & verbose){
        message("Found invalid licenses.\n")
        print(res)
        message("\nFor complete list of acceptable licenses run\n",
                "    'getLicensesList()'\n",
                "For suggestions try\n",
                "    'getLicensesList()'\n")
    }
    all(res)
}

suggestSpecies <- function(query, verbose=FALSE, op=c("|", "&")){
    op = match.arg(op)
    if (!requireNamespace("GenomeInfoDbData", quietly = TRUE))
        stop("Requires GenomeInfoDbData.  Please run:\n",
             "    BiocManager::install('GenomeInfoDbData')")
    if (verbose) message("Loading valid species information.")
    txdb <- GenomeInfoDb::loadTaxonomyDb()
    txdb <- rbind(txdb, c(NA, NA, ""))
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

getValidSourceTypes <- function(){

    # alphabetical
    expectedSourceTypes <- c("BAI", "BAM", "BED", "BigWig", "BioPax",
                             "BioPaxLevel2", "BioPaxLevel3", "BLOB", "CEL",
                             "CDF", "Chain", "CSV",
                             "ensembl", "FASTA", "FASTQ", "FCS", "GFF", "GRASP",
                             "GSEMatrix", "GTF", "HDF5", "HIC", "IDAT", "Inparanoid",
                             "JSON", "MTX", "mtx.gz", "MySQL", "mzid", "mzML", "mzTab",
                             "mzXML", "Multiple", "NCBI/blast2GO", "NCBI/ensembl",
                             "NCBI/UniProt", "PNG", "RDA", "RData", "RDS", "Simulated", "tab",
                             "tar.gz", "TIFF", "TSV", "TwoBit", "TXT", "UCSC track",
                             "VCF", "XLS/XLSX", "XML", "Zip")

    expectedSourceTypes

}

validDispatchClass <- function(dc, verbose=TRUE){

    mat <- AnnotationHub::DispatchClassList()
    res <- dc %in% as.character(mat[,1])
    if (any(!res) & verbose){
        message("Found invalid DispatchClass.\n")
        print(dc[!res])
        message("\nFor currently available DispatchClass run\n",
                "    'AnnotationHub::DispatchClassList()'\n")
    }
    all(res)
}
