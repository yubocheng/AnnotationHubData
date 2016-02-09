### =========================================================================
### makeEnsemblTwoBit()
### -------------------------------------------------------------------------
###

## Adjust this expression in order to save painful-reprocessing of older files.
## .ensemblReleaseRegex <- ".*release-(69|7[[:digit:]]|8[[:digit:]])"
## .ensemblReleaseRegex <- ".*release-(79|8[[:digit:]])"
## for a speed run just do one set
## .ensemblReleaseRegex <- ".*release-82"

## Much of the code needed by the following functions is already in "makeEnsemblFasta.R".

# .ensemblTwoBitTypes <-
#     c("cdna.all", "dna_rm.toplevel", "dna_sm.toplevel",
#       "dna.toplevel", "ncrna")
.ensemblTwoBitTypes <-
    c("cdna\\.all", "dna_rm\\.(primary_assembly|toplevel)",
      "dna_sm\\.(primary_assembly|toplevel)",
      "dna\\.(primary_assembly|toplevel)", "ncrna")

## For testing
#.ensemblTwoBitTypes <- c("primary_assembly")
#.ensemblTwoBitTypes <- c("Oryctolagus_cuniculus.+?cdna\\.all")

## metadata generator
makeEnsemblTwoBitToAHM <- # TODO: Add man page for this function
    function(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
             baseDir = "fasta/", regex,
             justRunUnitTest = FALSE, BiocVersion = biocVersion())
{
    time1 <- Sys.time()
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
            BiocVersion=BiocVersion,
            Coordinate_1_based = TRUE,
            DataProvider="Ensembl",
            Maintainer="<maintainer@bioconductor.org>",
            SourceType="FASTA",
            DispatchClass="TwoBitFile",
            RDataClass="TwoBitFile",
            RDataDateAdded=Sys.time(),
            Recipe="AnnotationHubData:::ensemblFastaToTwoBitFile",
            Tags=c("TwoBit", "ensembl", "sequence", "2bit", "FASTA")))
}

## Recipe: Convert .fa file to .2bit
ensemblFastaToTwoBitFile <- function(ahm)
{
    twobitOut <- outputFile(ahm)[[1]]  ## target out file
    srcFile <- sub('\\.2bit','.fa.gz', twobitOut)
    fastaFile <- sub('\\.gz','', srcFile)
    #GEOquery::gunzip(srcFile, destname=fastaFile, remove=FALSE) # Don't need
    # to unzip; use ".gz" files directly.
    ## Convert.
    export(import(srcFile, "FASTA"), twobitOut, "TwoBit")
}

## create the class and newResources() method
makeAnnotationHubResource("EnsemblTwoBitPreparer", makeEnsemblToTwoBitAHM)
