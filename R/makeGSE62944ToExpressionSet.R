AHMRecord <- function(currentMetadata, justRunUnitTest, BiocVersion) {
    list(AnnotationHubMetadata(
        Title="RNA-Sequencing and clinical data for 7706 tumor samples from The Cancer Genome Atlas",
        Description=.expandLine("TCGA RNA-seq Rsubread-summarized raw count data for 7706 tumor
                 samples, represented as an R / Bioconductor Biobase
                 ExpressionSet. R data representation derived from GEO accession
                 GSE62944."),
        BiocVersion=BiocVersion,
        Genome="hg19",
        SourceType="tar.gz",
        SourceUrl="http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE62944",
        SourceVersion="Jan 28 2015",
        Species="Homo sapiens",
        TaxonomyId=9606L,
        RDataPath="geo/GSE62944/GSE62944_GSM1536837_TCGA_20.Illumina.tumor_Rsubread_FeatureCounts.ExpressionSet.Rda",
        Coordinate_1_based=TRUE,
        DataProvider="GEO",
        Maintainer= "Martin Morgan <mtmorgan@fredhutch.org>",
        RDataClass= "ExpressionSet",
        RDataDateAdded=Sys.time(),
        Location_Prefix=.amazonBaseUrl,
        Recipe="AnnotationHubData:::GSE62944ToExpressionSet",
        DispatchClass="ExpressionSet",
        Tags=c("TCGA", "RNA-seq", "Expression", "Count")
        ))
}

makeAnnotationHubResource("GSE62944ToExpressionSetPreparer",
                          AHMRecord)

GSE62944ToExpressionSet <- function(ahm) {
    ## Imports: GEOquery, Biobase
    rootDir <- metadata(ahm)$AnnotationHubRoot 
    acc <- "GSE62944"
    outputPath <- file.path(rootDir, metadata(ahm)$RDataPath)
   
    if (file.exists(outputPath))
       return(outputFile(ahm))

    GEOquery::getGEOSuppFiles(acc, baseDir=file.path(rootDir, "geo", acc))
    clinvar <- local({
        message("clinvar")
        fl <- "GSE62944_TCGA_20_420_Clinical_Variables_7706_Samples.txt.gz"
        m <- scan(fl, what=character(), sep="\t", quote="")
        m <- matrix(m, 7707)
        dimnames(m) <- list(m[,1], m[1,])
        df <- as.data.frame(m[-1, -1])

        fl <- "GSE62944_TCGA_20_CancerType_Samples.txt.gz"
        ct <- read.delim(fl, header=FALSE,
                         colClasses=c("character", "factor"),
                         col.names=c("sample", "type"))
        idx <- match(rownames(df), ct$sample)
        stopifnot(!anyNA(idx))
        df$CancerType <- ct$type[idx]
        df
    })

    counts <- local({
        message("counts")
        fl <- "GSM1536837_TCGA_20.Illumina.tumor_Rsubread_FeatureCounts.txt.gz"
        if (!file.exists(fl))
            untar("GSE62944_RAW.tar", fl)
        m <- scan(fl, what=character(), sep="\t", quote="")
        m <- matrix(m, 7707)
        dimnames(m) <- list(m[,1], m[1,])
        m <- t(m[-1, -1])
        mode(m) <- "integer"
        m
    })

    adf <- Biobase::AnnotatedDataFrame(clinvar)
    eset <- Biobase::ExpressionSet(counts, adf)

    save(eset, file=outputPath)
    outputFile(ahm)
}

