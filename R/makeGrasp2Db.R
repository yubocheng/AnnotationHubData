## STEP 1: make function to process metadata into AHMs
.makeAnnotationHubRecord <-
    function(currentMetadata)
{
    list(AnnotationHubData::AnnotationHubMetadata(
        SourceFile="GraspFullDataset2.zip",
        SourceUrl="https://s3.amazonaws.com/NHLBI_Public/GRASP/GraspFullDataset2.zip",
        SourceVersion="2.0.0.0",

        SourceMd5="186677bd29e7043520bb9f6137014a12",
        SourceSize=475457834,
        DataProvider="NHLBI",
        Title="Bioconductor distribution of grasp2 v. 2.0.0.0",
        Description=gsub("[[:space:]]{2,}", " ",
           "Build 2.0.0.0 of the grasp2 data base, with
            2,082 GWAS studies, 8,872,472 genotype-phenotype results,
            188,362 unique phenotypes, 177 broad phenotype categories"),

        Species="Homo sapiens",
        TaxonomyId=9606L,
        Genome="hg19",
        Tags=c("SNP", "Annotation", "GRASP2"),
        Recipe=c(".grasp2ToAnnotationHub", package="grasp2db"),
        RDataClass="GRASP",
        RDataDateAdded = Sys.time(),                                    
        RDataVersion="2.0.0",
        RDataPath="NHLBI_Public/GRASP/BiocGRASP2.sqlite",
        Maintainer="Bioconductor Package Maintainer <maintainer@bioconductor.org>",
        Coordinate_1_based = FALSE)) ## Are you sure?  (default should be true)
}

## STEP 2: Make a recipe function
.grasp2ToAnnotationHub <-
    function(ahm)
{
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    fl <- system.file(package="grasp2db", "scripts", "grasp2AnnotationHub.R")
    source(fl, local=TRUE)
    file.copy("BiocGRASP2.sqlite", outputFile(ahm))
    outputFile(ahm)
}

## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("Grasp2ImportPreparer",
                          .makeAnnotationHubRecord)
