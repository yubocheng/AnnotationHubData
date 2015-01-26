.dbSNPBaseUrl <-"ftp://ftp.ncbi.nih.gov/snp/organisms/"

.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

.fileInfo <- function(url, tag) {
    tryCatch({
        df2 <- strsplit(getURL(url, dirlistonly=TRUE), "\r\n")[[1]]
        
        df2 <- df2[grep(".vcf.gz", df2)]
        drop <-  grepl("latest", df2) | grepl("00-", df2)
        df2 <- df2[!drop]
        df2 <- paste0(url, df2)
        
        #result <- lapply(df2, function(x){
        #   ok <- system(paste("curl -I", x), intern=TRUE)
        #   ok = GET(x, config=config(nobody=TRUE, filetime=TRUE)) 
        #   date <- ok[grep("Last-Modified", ok)]
        #   date <- sub("(.*?)Last-Modified:", "", date)
        #   size <- sub("Content-Length:", "", ok[grep("Content-Length:", ok)])
        #   list(date=.trim(date), size=.trim(size))
        #})
        #size <- as.numeric(sapply(result, "[[", "size"))
        #date <- strptime(sapply(result, "[[", "date"),
        #   "%a, %d %b %Y %H:%M:%S", tz="GMT")
        
        result <- lapply(df2, function(x){
            h = GET(x, config=config(nobody=TRUE, filetime=TRUE)) 
            headers(h)[c("last-modified", "content-length")] 
        })
        
        size <- as.numeric(sapply(result, "[[", "content-length"))
        date <- strptime(sapply(result, "[[", "last-modified"),
                         "%a, %d %b %Y %H:%M:%S", tz="GMT")
        
        data.frame(fileurl=df2, date, size, genome=tag, stringsAsFactors=FALSE)
    }, error=function(err) {
        warning(basename(dirname(url)), ": ", conditionMessage(err))
        data.frame(url=character(), version=character(), 
                   stringsAsFactors=FALSE)
    })
}

.getdbSNP <- function() {
    paths <- c(GRCh37="human_9606/VCF/", 
               GRCh38_b142="human_9606_b142_GRCh38/VCF/",
               GRCh38_b141="human_9606_b141_GRCh38/VCF/")
    
    urls <- setNames(paste0(.dbSNPBaseUrl, paths), names(paths))
    
    df <- do.call(rbind, Map(.fileInfo, urls, names(urls)))
    title <- basename(df$fileurl)
    sourceUrl <- sub(.dbSNPBaseUrl, "", df$fileurl)
    
    n <- length(title)
    withTbi <- grep(".tbi$",title)
    
    map <- c(`All_` = "VCF of all variations that meet the criteria to be in a VCF file.  This file is created once per dbSNP build.",
        `common_all` = "VCF of all variations that are polymorphic in a least one population the 1000 Genomes project or any of the following handles: 1000GENOMES, CSHL-HAPMAP, EGP_SNPS NHLBI-ESP, PGA-UW-FHCRC. A variation is polymorphic if the minor allele frequency is at least 0.01 and the minor allele is present in at least two samples.",
        `clinvar` = "VCF of variations from clinvar where 'YYYYMMDD' represents the date the file was created. This file is created weekly.", 
        `common_and_clinical` = "Variations from common_all.vcf.gz that are clinical.  A clinical variation is one the appears in clinvar_YYYYMMDD.vcf.gz with at least one of the following clinical significance codes: 4 - probable-pathogenic, 5 - pathogenic, 6 - drug-response, 7 - histocompatibility, 255 - other, This file is created weekly.",
        `common_no_known_medical_impact` = "Variations from common_all.vcf.gz that do not meet the clinical criteria described above.  This file is created weekly.")
    
    description <- character(n)
    for (i in seq_along(map))
        description[grep(names(map)[i], title)] <- map[[i]]
    
    cbind(df, title, sourceUrl, description, stringsAsFactors = FALSE)
    
}

makedbSNPVCF <- function(currentMetadata) {
    rsrc <- .getdbSNP()
    
    description <- rsrc$description
    genome <- rsrc$genome
    sourceFile <- rsrc$title
    title <- rsrc$title
    sourceUrls <- rsrc$sourceUrl
    sourceVersion <- sapply(rsrc$date, function(y) gsub(" ","_",y)) 
    ## should be character
    species <- rep("Homo sapiens", nrow(rsrc))
    taxonomyId <- rep(9606L, nrow(rsrc))
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    Tags <- lapply(rsrc$genome, function(tag) {
        c("dbSNP", tag, "VCF")
    })
       
    Map(AnnotationHubMetadata,
        Description=description, Genome=genome,
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion, Species=species,
        TaxonomyId=taxonomyId, Title=title, Tags=Tags,
        MoreArgs=list(
            Coordinate_1_based = FALSE,
            DataProvider = "ftp://ftp.ncbi.nih.gov/snp",
            Location_Prefix = .dbSNPBaseUrl,
            Maintainer = "Martin Morgan <mtmorgan@fhcrc.org>",
            RDataClass = "dbSNPVCFFile",
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.1",
            Recipe = NA_character_))
}

makeAnnotationHubResource("dbSNPVCFPreparer", makedbSNPVCF)


