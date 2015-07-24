test_EnsemblImportPreparer_genomeFromUrl <- function() {
    FUN <- AnnotationHubData:::.ensemblMetadataFromUrl

    exp <- c("ailMel1", "AnoCar2", "UMD3", "WBcel215",
             "C_jacchus3.2.1", "CanFam3")

    urls0 <-
        c("ailuropoda_melanoleuca/Ailuropoda_melanoleuca.ailMel1.69.gtf.gz", 
          "anolis_carolinensis/Anolis_carolinensis.AnoCar2.0.69.gtf.gz", 
          "bos_taurus/Bos_taurus.UMD3.1.69.gtf.gz", 
          "caenorhabditis_elegans/Caenorhabditis_elegans.WBcel215.69.gtf.gz", 
          "callithrix_jacchus/Callithrix_jacchus.C_jacchus3.2.1.69.gtf.gz", 
          "canis_familiaris/Canis_familiaris.CanFam3.1.69.gtf.gz")
    urls <- paste0("ftp://ftp.ensembl.org/pub/release-69/", urls0)
    checkIdentical(exp, FUN(urls)$genome)
}
