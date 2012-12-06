library(AnnotationHubData)
library(RUnit)

runTests <- function()
{
    test_empty_constructor()
    test_filename_constructor()
}

test_empty_constructor <- function()
{
    print ("--- test_empty_constructor")
    metadata <- AnnotationHubMetadata()
    checkTrue(validObject(metadata))
}

test_filename_constructor <- function()
{
    print ("--- test_filename_constructor")
    filename <- system.file ('extdata', 'wgEncodeRikenCageCd20CellPapTssHmm.top50.dcf',
                              package='AnnotationHubData')
    metadata <- AnnotationHubMetadata(filename)
    checkTrue(validObject(metadata))
    checkEquals(metadataTitle(metadata),
                "CD20 CAGE defined Transcriptional Start Sites (50 lines only, for testing)")

}
