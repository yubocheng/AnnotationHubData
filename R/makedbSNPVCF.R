.dbSNPBaseUrl <-"ftp://ftp.ncbi.nih.gov/"

.getdbSNP <- function(justRunUnitTest) {
    paths <- c(GRCh37="human_9606/VCF/", 
               GRCh38_b142="human_9606_b142_GRCh38/VCF/",
               GRCh38_b141="human_9606_b141_GRCh38/VCF/")
    
    baseUrl <- paste0(.dbSNPBaseUrl, "snp/organisms/")
        
    urls <- setNames(paste0(baseUrl, paths), names(paths))

    if(justRunUnitTest)
	urls <- urls[1]

    df <- do.call(rbind, 
                Map(.ftpFileInfo, urls, filename="vcf.gz", tag=names(urls)))
    title <- basename(df$fileurl)
        
    n <- length(title)
        
    map <- c(`All` = .expandLine("VCF of all variations that meet the criteria
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
    
    description <- character(n)
    for (i in seq_along(map))
        description[grep(names(map)[i], title)] <- map[[i]]
    
    cbind(df, title, description, stringsAsFactors = FALSE)
}

makedbSNPVCF <- function(currentMetadata, justRunUnitTest=FALSE) {
    rsrc <- .getdbSNP(justRunUnitTest)
   
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) 
    sourceLastModifiedDate <- rsrc$date
    
    ## resources table
    title <- rsrc$title
    description <- rsrc$description
    
    ## rdatapath should have 2 entries -for the VCF and its TabixFile
    rdatapath <- sub(.dbSNPBaseUrl, "", rsrc$fileurl)
    rdps <- rep(rdatapath, each=2) 
    rdatapaths <- split(rdps, f=as.factor(rep(seq_along(rdatapath),each=2)))
    rdatapaths <- lapply(rdatapaths,
                         function(x){x[2] <- paste0(x[2],".tbi") ; return(x)}) 
    
    Tags <- lapply(rsrc$genome, function(tag) {
        c("dbSNP", tag, "VCF")
    })
        
    Map(AnnotationHubMetadata,
        SourceSize=sourceSize,
        SourceUrl=sourceUrls,
        SourceVersion=sourceVersion,
        SourceLastModifiedDate=sourceLastModifiedDate,
                     
        Description=description,
        Title=title,
        Tags=Tags,
         
        RDataPath=rdatapaths,
                 
        MoreArgs=list(
            # input sources 
            SourceType= "VCF",
                              
            # resources
            Species="Homo sapiens",
            Genome="hg19",
            TaxonomyId=9606L, 
            DataProvider = "dbSNP",
            Maintainer =  "Sonali Arora <sarora@fredhutch.org>",
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

    faIn <- normalizePath(inputFiles(ahm))  # this is file on ftp site
    faOut1 <- normalizePath(outputFile(ahm))[1] # this is vcf.gz file on localDir
    faOut2 <- outputFile(ahm)[2]  # This is the vcf.gz.tbi file on localDir
    
    if(!file.exists(faOut2)) {
        tbiFile <- paste0(metadata(ahm)$Location_Prefix, metadata(ahm)$RDataPath[2]) 
        tbi <- download.file(tbiFile, faOut2)
    }
    faOut2
}

makeAnnotationHubResource("dbSNPVCFPreparer", makedbSNPVCF, quiet=TRUE)

