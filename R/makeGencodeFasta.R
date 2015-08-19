# recipe to get GFF3 files from Genecode. 
# importtant links 
#http://www.gencodegenes.org/releases/
#ftp site: ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human
# readme file for genecode project 
#ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human/_README.TXT

# FOR FATSA FILES
# gencode.vX.pc_transcripts.fa.gz
# gencode.vX.pc_translations.fa.gz
# gencode.vX.lncRNA_transcripts.fa.gz

# all helper functions can be found in : 
# AnnotationHubData/R/makeGenecodeGFF.R



## STEP 1: make function to process metadata into AHMs
makeGencodeFastaToAHMs <- function(currentMetadata, justRunUnitTest=FALSE, 
     BiocVersion=biocVersion()){

    ## important - here you need to know which species and release you want to 
    ## add files for.  
    #rsrc <- .gencodeGffSourceUrls(species="Human", release="23", 
    #   filetype="fasta", justRunUnitTest)
    rsrc <- .gencodeGffSourceUrls(species="Mouse", release="M6",
       filetype="fasta", justRunUnitTest)
 
    rdatapath <- rsrc$rdatapath
    rdps <- rep(rdatapath, each=2)
    rdatapaths <- split(rdps, f=as.factor(rep(seq_along(rdatapath),each=2)))
    rdatapath <- lapply(rdatapaths, function(x){
           x[1] <- sub("gz","rz", x[1])
           x[2] <- paste0(x[1],".fai")  
           return(x)})

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
          RDataClass = c("FaFile", "FaFile"), # changes
          DispatchClass="FaFile",  # changes!!
          SourceType="FASTA",  # changes!  
          Location_Prefix=.amazonBaseUrl,
          RDataDateAdded = Sys.time(),
          Recipe="AnnotationHubData:::gencodeFastaToFaFile"))   # changes! 
}

## recipe
gencodeFastaToFaFile <- function(ahm)
{
    faOut1 = outputFile(ahm)[[1]]
    faOut2 = outputFile(ahm)[[2]]

    if(!(all(file.exists(faOut1)& file.exists(faOut2)))){
       faOut <- normalizePath(outputFile(ahm)[[1]] )
       ## from which we 'know' the name of the source file that will be present...
       srcFile <- sub('.rz$','.gz',faOut)
       razip(srcFile)    ## which we unzip
       indexFa(faOut)    ## and index
    }
    
}


## STEP 2:  Call the helper to set up the newResources() method
makeAnnotationHubResource("GencodeFastaImportPreparer",
                          makeGencodeFastaToAHMs)



