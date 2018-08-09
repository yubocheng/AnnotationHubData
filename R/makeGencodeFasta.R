### =========================================================================
### makeGencodeFastaToAHM() and gencodeFastaToFaFile()
### -------------------------------------------------------------------------
###

### Recipe for human and mouse fasta files.
### http://www.gencodegenes.org/releases/
### ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human
### Files downloaded are listed in AnnotationHubData:::.gencodeDescription().

### metadata generator
makeGencodeFastaToAHM <- function(currentMetadata, 
                                baseUrl="ftp://ftp.sanger.ac.uk/pub/gencode/",
                                species=c("Human", "Mouse"), release, 
                                justRunUnitTest=FALSE,
                                BiocVersion=BiocManager::version())
{
    species <- match.arg(species)
    rsrc <- .gencodeSourceUrls(species, release, filetype="fasta", 
                               justRunUnitTest)
 
    rdatapath <- rsrc$rdatapath
    rdps <- rep(rdatapath, each=2)
    rdatapaths <- split(rdps, f=as.factor(rep(seq_along(rdatapath),each=2)))
    rdatapath <- lapply(rdatapaths, 
        function(x){
            x[1] <- sub("gz","rz", x[1])
            x[2] <- paste0(x[1],".fai")  
            x
        })

    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    genome <- rsrc$genome
    sourceUrls <- rsrc$fileurl
    sourceVersion <- as.character(rsrc$date) ## should be character
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    tags <- strsplit(rsrc$tag, ",")
    species <- rsrc$species
    taxid <- rsrc$taxid
    
    Map(AnnotationHubMetadata,
        Description=description, 
        Genome=genome,
        SourceUrl=sourceUrls,
        SourceSize=SourceSize,
        SourceLastModifiedDate=SourceLastModifiedDate,
        SourceVersion=sourceVersion,
        Species=species,
        RDataPath=rdatapath,
        TaxonomyId=taxid, 
        Title=title,
	Tags=tags, 
        MoreArgs=list(
          Coordinate_1_based = TRUE,
          DataProvider = "Gencode",
          Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
          RDataClass = c("FaFile", "FaFile"),
          DispatchClass="FaFile",
          SourceType="FASTA",  
          Location_Prefix="http://s3.amazonaws.com/annotationhub/",
          RDataDateAdded = Sys.time(),
          Recipe="AnnotationHubData:::gencodeFastaToFaFile")) 
}

gencodeFastaToFaFile <- function(ahm)
{
    .fastaToFaFile(ahm)
}

## create dispatch class and newResources() method
makeAnnotationHubResource("GencodeFastaImportPreparer", makeGencodeFastaToAHM)
