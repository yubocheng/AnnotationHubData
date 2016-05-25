## Community contributed resources.

## -----------------------------------------------------------------------
## Timothee Flutre's GRanges from GFF 
## -----------------------------------------------------------------------
metadata <- AnnotationHubMetadata(
    Description="Gene Annotation for Vitis vinifera",
    Genome="IGGP12Xv0",
    Species="Vitis vinifera",
    SourceUrl="http://genomes.cribi.unipd.it/DATA/V2/V2.1/V2.1.gff3",
    SourceLastModifiedDate=as.POSIXct("2014-04-17"),
    SourceVersion="2.1",
    RDataPath="community/tflutre/",
    TaxonomyId=29760L, 
    Title="Vvinifera_CRIBI_IGGP12Xv0_V2.1.gff3.Rdata",
    BiocVersion=package_version("3.3"),
    Coordinate_1_based=TRUE,
    DataProvider="CRIBI",
    Maintainer="Timothée Flutre <timothee.flutre@supagro.inra.fr",
    RDataClass="GRanges",
    DispatchClass="GRanges",
    SourceType="GFF",
    RDataDateAdded=as.POSIXct(Sys.time()),
    Recipe=NA_character_,
    PreparerClass="None",
    Tags=c("GFF", "CRIBI", "Gene", "Transcript", "Annotation"),
    Notes="chrUn renamed to chrUkn"
)

## upload to S3
file <- "Vvinifera_CRIBI_IGGP12Xv0_V2.1.gff3.Rdata"
bucket <- getOption("ANNOTATION_HUB_BUCKET_NAME", "annotationhub")
remotePath <- paste0(metadata(metadata)$RDataPath, metadata(metadata)$Title)
res <- upload_to_S3(file, remotePath, bucket)

## insert metadata
url <- getOption("AH_SERVER_POST_URL")
pushMetadata(list(metadata), url)
