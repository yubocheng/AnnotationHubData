### =========================================================================
### makeEnsemblTwoBit()
### -------------------------------------------------------------------------
###

.ensemblTwoBitTypes <-
    c("cdna\\.all", "dna_rm\\.(primary_assembly|toplevel)",
      "dna_sm\\.(primary_assembly|toplevel)",
      "dna\\.(primary_assembly|toplevel)", "ncrna")

## Metadata generator
## 'release' should be a single integer, e.g., 84
makeEnsemblTwoBitToAHM <- # TODO: Add man page for this function
    function(currentMetadata=getwd(), baseUrl = "ftp://ftp.ensembl.org/pub/",
             baseDir = "fasta/", release,
             justRunUnitTest = FALSE, BiocVersion = biocVersion())
{
    time1 <- Sys.time()
    regex <- paste0(".*release-", release)
    sourceUrl <- .ensemblFastaSourceUrls(baseUrl, baseDir, regex,
        baseTypes=.ensemblTwoBitTypes)
    if (justRunUnitTest)
        sourceUrl <- sourceUrl[1:5]

    sourceFile <- sub(baseUrl, "ensembl/", sourceUrl)
    meta <- .ensemblMetadataFromUrl(sourceUrl, twobit=TRUE)
    dnaType <- local({
        x <- basename(dirname(sourceFile))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("TwoBit", dnaType, "sequence for", meta$species)

    rdataPath <- sub("\\.fa\\.gz$", ".2bit", sourceFile)

    Map(AnnotationHubMetadata,
        Description=description,
        Genome=meta$genome,
        RDataPath=rdataPath,
        SourceUrl=sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        SourceSize=meta$sourceSize,
        SourceLastModifiedDate=meta$sourceLastModifiedDate,
        MoreArgs=list(
            BiocVersion=package_version(BiocVersion),
            Coordinate_1_based = TRUE,
            DataProvider="Ensembl",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
            SourceType="FASTA",
            DispatchClass="TwoBitFile",
            RDataClass="TwoBitFile",
            RDataDateAdded=Sys.time(),
            Recipe="AnnotationHubData:::ensemblFastaToTwoBitFile",
            Tags=c("TwoBit", "ensembl", "sequence", "2bit", "FASTA")))
}

ensemblFastaToTwoBitFile <- function(ahm)
{
    ## Convert .fa file to .2bit
    twobitOut <- outputFile(ahm)[[1]]
    srcFile <- sub('\\.2bit','.fa.gz', twobitOut)
    dna <- import(srcFile, "FASTA")

    ## ID as name
    ids <- sub(" .*", "", names(dna)) 
    stopifnot(length(ids) == length(dna))
    names(dna) <- ids 
    export(dna, twobitOut, "TwoBit")
    ## remove .fa file
    system(paste0("rm ", srcFile)) 
}

## create the class and newResources() method
makeAnnotationHubResource("EnsemblTwoBitPreparer", makeEnsemblTwoBitToAHM)
