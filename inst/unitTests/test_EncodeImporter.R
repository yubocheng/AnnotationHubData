library(AnnotationHubData)
library(RUnit)
#-------------------------------------------------------------------------------
runTests <- function()
{
  test_loadMetadata()
  test_assemblParams()
  test_createEncodeResource.1()
  
} # runTests
#-------------------------------------------------------------------------------
test_loadMetadata <- function ()
{
    print('--- test_loadMetadata')
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)
    checkEquals(dim(tbl.md), c(24396, 52))

} # test_loadMetadata
#-------------------------------------------------------------------------------
test_assemblParams <- function()
{
    print('--- test_assembleParams')

    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)
    
    annotationHubRoot <- "/foo/bar"
    webSiteRoot <- "http://hgdownload.cse.ucsc.edu"
        # check this against info, but don't always believe it!
    genomeVersion <- "hg19"
    dataFileName <- "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz"


    experiment.md <- as.list(tbl.md[dataFileName,])
    projectName <- experiment.md$dataDir
    projectPath <- file.path("goldenpath", genomeVersion, "encodeDCC", projectName)
    localStorageDirectory <- file.path(annotationHubRoot, projectPath)
    webSiteSourceDirectory <- file.path(webSiteRoot, projectPath)
    
    checkEquals(localStorageDirectory, "/foo/bar/goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt")
    checkEquals(projectPath, "goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt")

    params <- assembleParams(importer, experiment.md, webSiteSourceDirectory,
                             annotationHubRoot, projectPath, genomeVersion,
                             dataFileName)
    
    with(params, {
        checkEquals(Species, "Homo sapiens");
        checkEquals(Genome, "hg19");
        checkEquals(Recipe, "extendedBedToGRanges");
        checkTrue(is.list(RecipeArgs))
        checkEquals(RDataClass, "GRanges");
        checkEquals(RDataVersion, "0.0.1");
        checkEquals(Maintainer, "Paul Shannon <pshannon@fhcrc.org>");
        checkEquals(DataProvider, "hgdownload.cse.ucsc.edu");
        checkEquals(Coordinate_1_based, FALSE);
        checkEquals(RDataDateAdded, as.character(Sys.Date()))
        checkEquals(AnnotationHubRoot, annotationHubRoot)
        checkEquals(SourceFile,
              "goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt/wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz");
        checkEquals(Title, "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna");
        checkEquals(Description,
              "RbpAssocRna broadPeak H1-hESC GSM787620 wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna RipGeneSt wgEncodeEH001235 SunyAlbany");
        checkEquals(SourceUrl,
              "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeSunyAlbanyGeneSt/wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz");
        checkEquals(SourceVersion, "Nov_69522,Mil_AB3790 antibodies");
        })
    

} # test_assembleParams
#-------------------------------------------------------------------------------
test_createEncodeResource.1 <- function()
{
    print('--- test_createEncodeResource.1')
    importer <- EncodeImporter()
    tbl.md <- metadataTable(importer)
    
    dataFileName <- "wgEncodeSunyAlbanyGeneStH1hescT7tagRbpAssocRna.broadPeak.gz"
    checkTrue(dataFileName %in% rownames(tbl.md))
    
    directory <- tbl.md[dataFileName, "dataDir"]
    experiment.metadata <- as.list(tbl.md[dataFileName,])

    annotationHubRoot.tmp <- tempdir()

    webSiteRoot <- "http://hgdownload.cse.ucsc.edu"

    checkEquals(experiment.metadata$origAssembly, "hg18")
    genomeVersion <- "hg19"

    RDataFilename <- createResource(importer, annotationHubRoot.tmp,
                                    webSiteRoot, genomeVersion,
                                    dataFileName, experiment.metadata)

    load(RDataFilename)
    checkTrue(exists('gr'))
    checkEquals(length(gr), 147)
    md <- mcols(gr)
    checkEquals(dim(md), c(147, 5))
    checkEquals(colnames(md), c("name", "score", "signalValue", "pValue", "qValue"))

    TRUE

} # test_createEncodeResource.1 
#-------------------------------------------------------------------------------
