### =========================================================================
### makeEnsemblTwoBit()
### -------------------------------------------------------------------------
###

.ensemblTwoBitTypes <-
    c("cdna\\.all", "dna_rm\\.(primary_assembly|toplevel)",
      "dna_sm\\.(primary_assembly|toplevel)",
      "dna\\.(primary_assembly|toplevel)", "ncrna")

## Metadata generator
makeEnsemblTwoBitToAHM <-
    function(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
             baseDir = "fasta/", release,
             justRunUnitTest = FALSE, BiocVersion = BiocManager::version())
{
    if (length(release) > 1L)
        stop("'release' must be a single integer")
    if (length(BiocVersion) > 1L)
        stop("BiocVersion must be a single version")
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
        AnnotationHubRoot=currentMetadata$AnnotationHubRoot,
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
    gc()
    twobitOut <- outputFile(ahm)[[1]]
    srcFile <- sub('\\.2bit','.fa.gz', twobitOut)
    dna <- import(srcFile, "FASTA")
    gc()

    tryCatch({
        ## ID as name
        ids <- sub(" .*", "", names(dna))
        stopifnot(length(ids) == length(dna))
        names(dna) <- ids
        dna <- Biostrings::replaceAmbiguities(dna)
        export(dna, twobitOut, "TwoBit")
    }, error = function(err) {
        message("conversion failed",
                "\n  file: ",  sQuote(srcFile),
                "\n  reason: ",  conditionMessage(err),
                call.=FALSE)
    }, finally = function(){
        ## remove .fa file
        if (exists("dna")){
            rm("dna")
            gc()
        }
        system(paste0("rm ", srcFile))
        gc()
    })

    ## remove .fa file
    if (exists("dna")){
        rm("dna")
        gc()
    }
    system(paste0("rm ", srcFile))
    gc()
}

## create the class and newResources() method
makeAnnotationHubResource("EnsemblTwoBitPreparer", makeEnsemblTwoBitToAHM)
