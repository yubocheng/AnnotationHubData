dbSNPVCFImportPreparer <- 
    setClass("dbSNPVCFImportPreparer", contains="ImportPreparer")

.dbSNPBaseUrl <-"ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/VCF/"

.dbSNPVCFSourceUrls <-
    function(baseUrl)
{
    baseUrl <- sprintf("%s%s", baseUrl, c("", "ByChromosome/"))
    unlist(lapply(baseUrl, function(url) {
        listing <- strsplit(getURL(url, dirlistonly=TRUE), "\n")[[1]]
        drop <- grepl("latest.vcf.gz$", listing) | ("00-All.vcf.gz" == listing)
        sprintf("%s%s", url, grep("vcf.gz$", listing[!drop], value=TRUE))
    }))
}

.dbSNPVCFSourcePathFromUrl <-
    function(baseUrl, sourceUrl)
{
    sub("ftp://ftp.ncbi.nih.gov/snp", "dbSNP", sourceUrl)
}

.dbSNPVCFMetadata <-
    function(baseUrl, sourceUrl)
{
    n <- length(sourceUrl)
    sourceVersion <- "137"
    title <- sub(".*/([[:alnum:]_-]+.vcf).gz$", "\\1", sourceUrl)

    sourceFile <- .dbSNPVCFSourcePathFromUrl(baseUrl, sourceUrl)
    ## expand to include .tbi
    withTbi <-
        grepl("^(00-All|common_all|common_no_known_medical_impact_).*",
              title)
    sourceUrl[withTbi] <-
        Map(c, sourceUrl[withTbi], sprintf("%s.tbi", sourceUrl[withTbi]))
    sourceFile[withTbi] <-
        Map(c, sourceFile[withTbi], sprintf("%s.tbi", sourceFile[withTbi]))

    recipe <-
        rep(list(c("dbSNPVCFToVCF", package="AnnotationHubData")), n)
    recipe[withTbi] <-
        list(c("dbSNPVCFToTabixFile", package="AnnotationHubData"))

    rDataClass <- rep("VCF", n)
    rDataClass[withTbi] <- "TabixFile"

    rDataPath <- sourceFile
    rDataPath[!withTbi] <- sub(".vcf.gz$", ".RData", rDataPath[!withTbi])

    ## description
    description <- character(n)
    map <- c(`00-All` = "VCF of all variations that meet the criteria to be in a VCF file.  This file is created once per dbSNP build.",
             `common_all` = "VCF of all variations that are polymorphic in a least one population the 1000 Genomes project or any of the following handles: 1000GENOMES, CSHL-HAPMAP, EGP_SNPS NHLBI-ESP, PGA-UW-FHCRC. A variation is polymorphic if the minor allele frequency is at least 0.01 and the minor allele is present in at least two samples.",
             `clinvar` = "VCF of variations from clinvar where 'YYYYMMDD' represents the date the file was created. This file is created weekly.",
             `common_and_clinical` = "Variations from common_all.vcf.gz that are clinical.  A clinical variation is one the appears in clinvar_YYYYMMDD.vcf.gz with at least one of the following clinical significance codes: 4 - probable-pathogenic, 5 - pathogenic, 6 - drug-response, 7 - histocompatibility, 255 - other, This file is created weekly.",
             `common_no_known_medical_impact` = "Variations from common_all.vcf.gz that do not meet the clinical criteria described above.  This file is created weekly.")
    idx <- match(sub("_[[:digit:]]+$", "", title), names(map))
    isNA <- is.na(idx)
    description[!isNA] <- sprintf("dbSNP build %s %s", sourceVersion,
                                  unname(map[idx[!isNA]]))
    regex <- "^([[:digit:]XY(MT)]+)-([[:digit:]]+)-([A-Z]{3}).vcf$"
    description[isNA] <-
        sprintf("dbSNP build %s VCF with genotypes and genotype freqencies listed by chromosome and population ID. Chromosome %s population %s (%s)",
                sourceVersion, sQuote(sub(regex, "\\1", title[isNA])),
                sub(regex, "\\2", title[isNA]), sub(regex, "\\3", title[isNA]))

    ## tags
    tags <- rep(list(c("dbSNP", "VCF")), n)
    tags[isNA] <- Map(c, tags[isNA], Chromosome=sub(regex, "\\1", title[isNA]),
                      PopulationId=sub(regex, "\\2", title[isNA]),
                      PopulationCode=sub(regex, "\\3", title[isNA]))

    unname(Map(AnnotationHubMetadata,
        Description=description, RDataPath=rDataPath,
        RDataClass = rDataClass, SourceFile=sourceFile,
        SourceUrl=sourceUrl, Title=title, Tags=tags, Recipe=recipe,
        MoreArgs=list(
          AnnotationHubRoot = NA_character_,
          Genome = "GRCh37",
          SourceVersion = sourceVersion,
          Species = "Homo sapiens",
          TaxonomyId = "9606",
          Coordinate_1_based = TRUE,
          DataProvider = "ftp://ftp.ncbi.nih.gov/snp",
          Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
          RDataDateAdded = Sys.time(),
          RDataVersion = "0.0.1")))
}

setMethod(newResources, signature="dbSNPVCFImportPreparer",
    function(importPreparer, currentMetadata = list(), ...)
{
    sourceUrls <- .dbSNPVCFSourceUrls(.dbSNPBaseUrl)
    knownUrls <- sapply(currentMetadata, function(elt) metadata(elt)$SourceUrl)
    sourceUrls <- sourceUrls[!sourceUrls %in% knownUrls]
    .dbSNPVCFMetadata(.dbSNPBaseUrl, sourceUrls)
})

dbSNPVCFToVCF <-
    function(ahm)
{
    inp <- inputFiles(ahm)
    genome <- metadata(ahm)$Genome
    vcf <- VariantAnnotation::readVcf(inp, genome)
    save(vcf, file=outputFile(ahm))
    outputFile(ahm)
}

dbSNPVCFToTabixFile <-
    function(ahm)
{
    outputFile(ahm)
}
