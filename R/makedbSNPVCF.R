### link to data description:
### http://www.ncbi.nlm.nih.gov/variation/docs/human_variation_vcf/

### ----------------------------------------------------------------------
### March 2016

### Files in AH that point to ftp.ncbi.nih.gov/snp/organisms/*
### are no longer available:

### We have 38 of them:

### > length(query(hub, c("dbsnp", "vcf")))
### [1] 38
### 
### 
### Of the 38 full urls seen with query(hub, c("dbsnp", "vcf"))$sourceurl,
### there are 5 unique base directories:
### 
### ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/
### ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606_b141_GRCh37p13/
### ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606_b142_GRCh37p13/
### ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606_b142_GRCh38/
### ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606_b141_GRCh38/
### 
### On the web site (ftp://ftp.ncbi.nih.gov/snp/organisms/) there 7 base
### directories. b141 is no longer there and b144, b146 have been added.

### The recipe has been updated to look in 
###    ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/
###    ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/
### ----------------------------------------------------------------------

.dbSNPBaseUrl <-"ftp://ftp.ncbi.nlm.nih.gov/"

## Chosing files from archive so all files have a date stamp.
## It looks like files with no stamp in the current
## directory are either 'in progress' or 'subject to change'.
.getdbSNP <- function(justRunUnitTest) {
    baseUrl <- paste0(.dbSNPBaseUrl, "pub/clinvar/")
    paths <- c(GRCh37="vcf_GRCh37/archive_1.0/2016/", 
               GRCh38="vcf_GRCh38/archive_1.0/2016/")
    files <- c("clinvar_20160203", "clinvar_20160203_papu", 
               "common_and_clinical_20160203", 
               "common_no_known_medical_impact_20160203")
    urls <- setNames(paste0(baseUrl, paths), names(paths))

    if (justRunUnitTest)
        urls <- urls[1]
  
    genome <- rep(names(urls), each=length(files))
    df <- .ftpFileInfo(url=urls, extension=paste0(files, ".vcf.gz"))
    df$genome <- gsub("GRCh37clinical", "GRCh37", genome)
    df <- cbind(df, title=basename(df$fileurl), stringsAsFactors = FALSE)
    rownames(df) <- NULL
 
    map <- c(
        `All` = .expandLine("VCF of all variations that meet the criteria
        to be in a  VCF file.  This file is created once per dbSNP build."),
        `All_papu` = .expandLine("VCF of all variations found in the 
        psuedoautosomal region (PAR), alternate loci, patch sequences and 
        unlocalized or unplaced contigs(papu)"),
        `common_all` = .expandLine("VCF of all variations that are polymorphic
        in a least one population the 1000 Genomes project or any of the 
        following handles: 1000GENOMES, CSHL-HAPMAP, EGP_SNPS NHLBI-ESP,
        PGA-UW-FHCRC. A variation is polymorphic if the minor allele 
        frequency is at least 0.01 and the minor allele is present in 
        at least two samples."),
        `clinvar` = .expandLine("VCF of variations from clinvar where 'YYYYMMDD' 
        represents the date the file was created. This file is created 
        weekly."), 
        `common_and_clinical` = .expandLine("Variations from common_all.vcf.gz
        that are clinical.  A clinical variation is one the appears in 
        clinvar_YYYYMMDD.vcf.gz with at least one of the following clinical
        significance codes: 4 - probable-pathogenic, 5 - pathogenic, 
        6 - drug-response, 7 - histocompatibility, 255 - other, 
        This file is created weekly."),
        `common_no_known_medical_impact` = .expandLine("Variations from 
        common_all.vcf.gz that do not meet the clinical criteria described 
        above.  This file is created weekly."))
 
    description <- character(length(title))
    for (i in seq_along(map))
        description[grep(names(map)[i], df$title)] <- map[[i]]

    cbind(df, description, stringsAsFactors = FALSE)
}

makedbSNPVCF <- function(currentMetadata, justRunUnitTest=TRUE,
                         BiocVersion=biocVersion()) {
    rsrc <- .getdbSNP(justRunUnitTest)

    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
 
    ## resources table
    title <- rsrc$title
    description <- rsrc$description
    genome <- rsrc$genome
 
    ## rdatapath should have 2 entries -for the VCF and its TabixFile
    rdatapath <- sub(.dbSNPBaseUrl, "", rsrc$fileurl)
    rdps <- rep(rdatapath, each=2) 
    rdatapaths <- split(rdps, f=as.factor(rep(seq_along(rdatapath),each=2)))
    rdatapaths <- lapply(rdatapaths,
                         function(x){x[2] <- paste0(x[2],".tbi") ; return(x)}) 
 
    tags <- lapply(genome, 
        function(tag) c("dbSNP", tag, "VCF")
    )
 
    Map(AnnotationHubMetadata,
        SourceSize=sourceSize,
        SourceUrl=sourceUrls,
        SourceVersion=sourceVersion,
        SourceLastModifiedDate=sourceLastModifiedDate,
 
        Description=description,
        Title=title,
        Genome=genome,
        Tags=tags,
        RDataPath=rdatapaths,
 
        MoreArgs=list(
            BiocVersion=BiocVersion,
            # input sources 
            SourceType= "VCF",
 
            # resources
            Species="Homo sapiens",
            TaxonomyId=9606L, 
            DataProvider = "dbSNP",
            Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
            Coordinate_1_based = FALSE,
            Location_Prefix = .dbSNPBaseUrl,
            RDataDateAdded = Sys.time(),

            #rdata table
            DispatchClass= "dbSNPVCFFile" ,
            RDataClass = c("VcfFile", "VcfFile"),

            Recipe = "AnnotationHubData:::ncbi_dbSNPVCFFile"))
}


## recipe
ncbi_dbSNPVCFFile <- function(ahm)
{
    ## The tbi file exists online, just download it.
    faIn <- normalizePath(inputFiles(ahm))  # file on ftp site
    faOut1 <- normalizePath(outputFile(ahm))[1] # vcf.gz file on localDir
    faOut2 <- outputFile(ahm)[2]  # vcf.gz.tbi file on localDir
 
    if(!file.exists(faOut2)) {
        tbiFile <- paste0(metadata(ahm)$Location_Prefix, 
                          metadata(ahm)$RDataPath[2]) 
        tbi <- download.file(tbiFile, faOut2)
    }
    faOut2
}

makeAnnotationHubResource("dbSNPVCFPreparer", makedbSNPVCF, quiet=TRUE)

