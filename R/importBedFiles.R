#library(AnnotationHubData)


bedCommon <- function()
{
    l <- list()
    l$Species <- "Homo sapiens"
    l$Genome <- "hg19"
    l$Recipe <- "unknown" # FIXME
    l$RecipeArgs <- list() # FIXME
    l$RDataClass <- "GRanges"
    l$RDataVersion <- "0.0.1"
    l$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    l$DataProvider <- "hgdownload.cse.ucsc.edu"
    l$Coordinate_1_based <- TRUE
    l$RDataDateAdded <- "2013-01-01 00:00:00"

    l
}

import_wgEncodeRegDnaseClustered <- function(ahroot)
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    rp <- "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered"
    files <- file.path(rp, c("wgEncodeRegDnaseClustered.bed.gz",
        "wgEncodeRegDnaseClusteredInputs.tab"))
    params$SourceFile <- files

    #c("goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab",
    #    "goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab")
    params$Title <- "ENCODE Composite DNaseI Hypersensitivity Regions"
    params$Description <- "999,988 DNaseI hypersensitivity regions, combined from 75 cell types"
    params$SourceUrl <- c("http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClusteredInputs.tab.gz",
        "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegDnaseClustered/wgEncodeRegDnaseClustered.bed.gz")
    params$SourceVersion <- "dateSubmitted=2011-04-28"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene regulation"
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}

import_wgEncodeRegTfbsClusteredV2 <- function(ahroot) # v2?
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    rp <- "goldenpath/hg19/encodeDCC/wgEncodeRegTfbsClustered"
    files <- file.path(rp, "wgEncodeRegTfbsClusteredV2.bed.gz")
    params$SourceFile <- files
    params$Title <- "ENCODE Composite Transcription Factor Binding Regions, V2"
    params$Description <- "2750489 TFBS from ChIP-seq, for 95 cell-lines, 148 TFs, 425 UCSC tracks"
    params$SourceUrl <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRegTfbsClustered/wgEncodeRegTfbsClusteredV2.bed.gz"
    params$SourceVersion <- "dateUnrestricted=2011-10-21"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene regulation"
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}


import_stamConnectivity <- function(ahroot)
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    params$SourceFile <- "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/openchrom/jan2011/dhs_gene_connectivity/genomewideCorrs_above0.7_promoterPlusMinus500kb_withGeneNames_32celltypeCategories.bed8.gz"
    params$Title <- "stamConnectivity"
    params$Description <- "Associations between regulatory regions and target genes, based upon similar, eg, methylation patterns across cell types and conditions"
    params$SourceUrl <- "ftp://ftp.ebi.ac.uk/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/openchrom/jan2011/dhs_gene_connectivity/genomewideCorrs_above0.7_promoterPlusMinus500kb_withGeneNames_32celltypeCategories.bed8.gz"
    params$SourceVersion <- "unknown"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene regulation"
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}

import_stamFootprints <- function(ahroot)
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    params$SourceFile <- "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz"
    params$Title <- "DNase1 footprints from the stamlab"
    params$Description <- "45M subsites within DNase1 sensitivity regions, presumed to be transcription factor binding sites"
    params$SourceUrl <- "ftp://ftp.ebi.ac.uk/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz"
    params$SourceVersion <- "unknown"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene regulation"
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}


import_stamH3K4me3ProfilePromoters <- function(ahroot)
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    rp <- "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/openchrom/jan2011/promoter_predictions"
    files <- file.path(rp, c("master_known.bed","master_novel.bed"))
    params$SourceFile <- files
    params$Title <- "DNase1 footprints from the stamlab"
    params$Description <- "45M subsites within DNase1 sensitivity regions, presumed to be transcription factor binding sites"
    params$SourceUrl <- c("ftp://ftp.ebi.ac.uk/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/openchrom/jan2011/promoter_predictions/master_known.bed",
        "ftp://ftp.ebi.ac.uk/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/openchrom/jan2011/promoter_predictions/master_novel.bed")
    params$SourceVersion <- "unknown"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene regulation"
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}

import_wgEncodeRikenCageCd20CellPapTssHmm <- function(ahroot)
{
    params <- bedCommon()
    params$Recipe <- "extendedBedToGRanges"
    params$AnnotationHubRoot <- ahroot
    params$SourceFile <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements"
    params$Title <- "CD20 CAGE defined Transcriptional Start Sites"
    params$Description <- "120785  TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD20 cells, from Timo Lassmann"
    params$SourceUrl <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements"
    params$SourceVersion <- "unknown"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene regulation"
    params$Notes <- paste("9 total columns in the bed file, 8 of which are presented here ('empty' is omitted)",
        "  (a) Chromosome ",
        "  (b) Start",
        "  (c) End",
        "  (d) (coordinates):(paraclu cluster strength):(TSS prediction strength)",
        "  (e) empty ",
        "  (f) Strand",
        "  (g) level - expression level in tpm",
        "  (h) signif - currently empty - will be IDR",
        "  (i) score2 - raw number of reads",
        "   wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements.gz project=wgEncode; grant=Gingeras; lab=RIKEN; composite=wgEncodeRikenCage; dataType=Cage; view=TssHmm; cell=CD20+; localization=cell; rnaExtract=longPolyA; readType=1x50; donorId=RO01794,RO01778; dataVersion=ENCODE Mar 2012 Freeze; dccAccession=wgEncodeEH002738; dateSubmitted=2012-03-30; dateUnrestricted=2012-12-30; subId=6744; geoSampleAccession=GSM979634; labExpId=CThi10023,CThi10024; bioRep=041WC,042WC; seqPlatform=Illumina_HiSeq_2000; tableName=wgEncodeRikenCageCd20CellPapTssHmm; type=bedRnaElements; md5sum=c69036e9a1bf0eb39d0b73687fc31ec1; size=2.5M",
    collapse="\n", sep="")
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}


import_wgEncodeRikenCageCd34mobilizedCellPapTssHmm <- function(ahroot)
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    params$SourceFile <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd34mobilizedCellPapTssHmm.bedRnaElements"
    params$Title <- "CD34 CAGE defined Transcriptional Start Sites"
    params$Description <- "102445 TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD14 cells, from Timo Lassmann"
    params$SourceUrl <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageCd34mobilizedCellPapTssHmm.bedRnaElements"
    params$SourceVersion <- "unknown"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene model"
    params$Notes <- paste("9 total columns in the bed file, 8 of which are presented here ('empty' is omitted)",
"  (a) Chromosome ",
"  (b) Start",
"  (c) End",
"  (d) (coordinates):(paraclu cluster strength):(TSS prediction strength)",
"  (e) empty ",
"  (f) Strand",
"  (g) level - expression level in tpm",
"  (h) signif - currently empty - will be IDR",
"  (i) score2 - raw number of reads",
"ENCODE metadata: wgEncodeRikenCageCd34mobilizedCellPapTssHmm.bedRnaElements.gz project=wgEncode; grant=Gingeras; lab=RIKEN; composite=wgEncodeRikenCage; dataType=Cage; view=TssHmm; cell=CD34+_Mobilized; localization=cell; rnaExtract=longPolyA; readType=1x50; dataVersion=ENCODE Mar 2012 Freeze; dccAccession=wgEncodeEH002739; dateSubmitted=2012-03-30; dateUnrestricted=2012-12-30; subId=6725; geoSampleAccession=GSM979635; labExpId=CThi10058; bioRep=043WC; seqPlatform=Illumina_HiSeq_2000; tableName=wgEncodeRikenCageCd34mobilizedCellPapTssHmm; type=bedRnaElements; md5sum=ab8da53f0160caed5bf387c3f1f376d0; size=1.9M",
collapse="\n", sep="")
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}

import_wgEncodeRikenCageMonocd14CellPapTssHmm <- function(ahroot)
{
    params <- bedCommon()
    params$AnnotationHubRoot <- ahroot
    params$SourceFile <- "goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageMonocd14CellPapTssHmm.bedRnaElements"
    params$Title <- "CD14 CAGE defined Transcriptional Start Sites"
    params$Description <- "132882 TSS sites predicted by CAGE (Capped Analysis of GeneExpression) in CD14 cells, from Timo Lassmann"
    params$SourceUrl <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/wgEncodeRikenCage/wgEncodeRikenCageMonocd14CellPapTssHmm.bedRnaElements"
    params$SourceVersion <- "unknown"
    params$Maintainer <- "Paul Shannon <pshannon@fhcrc.org>"
    params$Tags <- "gene model"
    params$Notes <- paste("9 total columns in the bed file, 8 of which are presented here ('empty' is omitted)",
"  (a) Chromosome ",
"  (b) Start",
"  (c) End",
"  (d) (coordinates):(paraclu cluster strength):(TSS prediction strength)",
"  (e) empty ",
"  (f) Strand",
"  (g) level - expression level in tpm",
"  (h) signif - currently empty - will be IDR",
"  (i) score2 - raw number of reads",
"   wgEncodeRikenCageMonocd14CellPapTssHmm.bedRnaElements.gz project=wgEncode; grant=Gingeras; lab=RIKEN; composite=wgEncodeRikenCage; dataType=Cage; view=TssHmm; cell=Monocytes-CD14+; localization=cell; rnaExtract=longPolyA; readType=1x50; donorId=RO01746; dataVersion=ENCODE Mar 2012 Freeze; dccAccession=wgEncodeEH002757; dateSubmitted=2012-03-30; dateUnrestricted=2012-12-30; subId=6715; geoSampleAccession=GSM979655; labExpId=CThi10021,CThi10022; bioRep=039WC,040WC; seqPlatform=Illumina_HiSeq_2000; tableName=wgEncodeRikenCageMonocd14CellPapTssHmm; type=bedRnaElements; md5sum=e44371b91fd0bb68b25c4231941fe096; size=2.7M",
        collapse="\n", sep="")
    x <- do.call(AnnotationHubMetadata, params)
    x <- postProcessMetadata(ahroot, metadata(x)$RDataVersion,
        metadata(x)$SourceFile)
    x
}


importAllBedFiles <- function(ahroot)
{
    for (f in c(
    "import_wgEncodeRegDnaseClustered",
    "import_wgEncodeRegTfbsClusteredV2",
    "import_stamConnectivity",
    "import_stamFootprints",
    "import_stamH3K4me3ProfilePromoters",
    "import_wgEncodeRikenCageCd20CellPapTssHmm",
    "import_wgEncodeRikenCageCd34mobilizedCellPapTssHmm",
    "import_wgEncodeRikenCageMonocd14CellPapTssHmm"))
    {
        .printf("Running %s()", f)
        do.call(f, list(ahroot))
    }
}

