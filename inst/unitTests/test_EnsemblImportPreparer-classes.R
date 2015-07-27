test_EnsemblImportPreparer_genomeFromUrl <- function() {
    FUN <- AnnotationHubData:::.ensemblMetadataFromUrl
    exp <- c("ailMel1", "AnoCar2")
    urls0 <-
        c("ailuropoda_melanoleuca/Ailuropoda_melanoleuca.ailMel1.69.gtf.gz", 
          "anolis_carolinensis/Anolis_carolinensis.AnoCar2.0.69.gtf.gz")
    urls <- paste0("ftp://ftp.ensembl.org/pub/release-69/", urls0)
    ans <- FUN(urls)$genome
    checkIdentical(exp, ans)
}
