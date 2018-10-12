# recipe to get GFF3 files from Genecode.
# importtant links
#http://www.gencodegenes.org/releases/
#ftp site: ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human
# readme file for genecode project
#ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human/_README.TXT

# for gff3 files
#gencode.vX.annotation.gff3.gz
#gencode.vX.chr_patch_hapl_scaff.annotation.gff3.gz
#gencode.vX.polyAs.gff3.gz:
#gencode.vX.polyAs.gff3.gz:
#gencode.vX.2wayconspseudos.gff3.gz:
# gencode.vX.long_noncoding_RNAs.gff3.gz
#gencode.vX.tRNAs.gff3.gz

# only gff3 files will be added - since both gtf and gff3 contain same
# data, but gff3 is better (Herve) .These files will not be stored as
# a GRanges on amazon s3.

.gencodeBaseUrl <- "ftp://ftp.sanger.ac.uk/pub/gencode/"

.gencodeFileFromUrl <- function(urls) {
    unlist(sapply(urls, function(url) {
        listing <- .ftpDirectoryInfo(url)
        listing = listing[grep("gencode", listing)]
        paste0(url, "gencode", sub(".*gencode","", listing ))
    }, USE.NAMES=FALSE))
}

.gencodeDescription <- function(fileurls){
    # add description map here.
    map <- c(
      annotation.gff3.gz=.expandLine("Gene annotations
          on reference chromosomes from Gencode"),
      chr_patch_hapl_scaff.annotation.=.expandLine("Gene annotation
          on reference-chromosomes/patches/scaffolds/haplotypes from Gencode"),
      polyAs=.expandLine("files contain polyA signals, polyA sites and
          pseudo polyAs manually annotated by HAVANA from only the refrence
          chromosome"),
      wayconspseudos=.expandLine("pseudogenes predicted by the Yale
          & UCSC pipelines, but not by Havana on reference chromosomes"),
      long_noncoding_RNAs=.expandLine("sub-set of the main annotation files
          on the reference chromosomes. They contain only the lncRNA genes.
          Long non-coding RNA genes are considered the genes with any of
          those biotypes: 'processed_transcript', 'lincRNA',
          '3prime_overlapping_ncrna', 'antisense', 'non_coding',
          'sense_intronic' , 'sense_overlapping' , 'TEC' , 'known_ncrna'."),
      tRNAs =.expandLine("tRNA structures predicted by tRNA-Scan on
          reference chromosomes"),
      transcripts.fa.gz=.expandLine("Protein-coding transcript sequences
          on reference chromosomes Fasta file"),
      translations.fa.gz=.expandLine("Translations of protein-coding
          transcripts on reference chromosomes Fasta file"),
      lncRNA_transcripts.fa.gz=.expandLine("Long non-coding RNA
          transcript sequences on reference chromosomes Fasta file.")
      )
    description <- character(length(fileurls))
    for (i in seq_along(map))
        description[grep(names(map)[i], fileurls)] <- map[[i]]

    description
}

.gencodeGenome <- function(species, release) {
    # this information is curated from Gencode's website
    # link - http://www.gencodegenes.org/releases/
    if (species=="Human")
      tblurl <- "https://www.gencodegenes.org/human/releases"
    else
      tblurl <- "https://www.gencodegenes.org/mouse/releases"

    ## read in the table
    http <- RCurl::getURL(tblurl)
    tbl <- XML::readHTMLTable(http, header=TRUE, stringsAsFactors=FALSE)
    tbl <- tbl[[1]]
    tblheader <- gsub("\n", "", colnames(tbl))
    tblheader = trimws(tblheader)
    colnames(tbl) = tblheader

    idx <- which(tbl[,"GENCODE release"]==release)
    tbl[idx,"Genome assembly version"]
}


# Helper to retrieve GTF & GFF3 file urls from Gencode
.gencodeSourceUrls <- function(species, release, filetype, justRunUnitTest)
{
    speciesUrl <- ifelse(species=="Human", "Gencode_human/", "Gencode_mouse/")
    dirurl = paste0(.gencodeBaseUrl, speciesUrl, "release_", release, "/")
    names(dirurl) <- paste0(species,"_", release)

    fileurls <-.gencodeFileFromUrl(dirurl)

    if (tolower(filetype)=="gff")
       idx <-  grep("gff3", fileurls)
    if(tolower(filetype)=="fasta")
       idx <-  grep("fa.gz", fileurls)
    fileurls <- fileurls[idx]

    if(length(idx)==0)
     stop("No files found.")

     if(justRunUnitTest)
        fileurls <- fileurls[1:2]

    ## tags
    filename <- basename(fileurls)
    filename <- sub(".gz","", filename)
    tags <- gsub("[.]",",",filename)

    ## description
    description <- .gencodeDescription(fileurls)

    ## rdatapath - these files will be made into GRanges and stored on S3.
    #rdatapath <- paste0("gencode/", species, "/release_", release,"/",
    #    basename(fileurls), ".Rda")

    rdatapath <- sub(.gencodeBaseUrl, "", fileurls)


    ## get date and size for files
    df <- .httrFileInfo(fileurls)
    rownames(df) <- NULL

    ## species, taxid, genome
    scSpecies <- ifelse(species=="Human", "Homo sapiens", "Mus musculus")
    taxid <- ifelse(species=="Human", 9606L, 1090L)
    genome <- .gencodeGenome(species, release)
    genome <- rep(genome, length(fileurls))
    scSpecies <- rep(scSpecies, length(fileurls))
    taxid <- rep(taxid, length(fileurls))
    genome <- .gencodeGenome(species, release)
    genome <- rep(genome, length(fileurls))

    cbind(df, rdatapath, description, tags, species=scSpecies, taxid, genome,
         stringsAsFactors=FALSE)
}


## STEP 1: make function to process metadata into AHMs
makeGencodeGFFsToAHMs <- function(currentMetadata, justRunUnitTest, BiocVersion){

    ## important - here you need to know which species and release you want to
    ## add files for.
    rsrc <- .gencodeSourceUrls(species="Human", release="23", filetype="gff",
         justRunUnitTest)

    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    genome <- rsrc$genome
    sourceUrls <- rsrc$fileurl
    sourceVersion <- as.character(rsrc$date) ## should be character
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    tags <- strsplit(rsrc$tag, ",")
    species <- rsrc$species
    rdatapath <- rsrc$rdatapath
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
          BiocVersion=BiocVersion,
          Coordinate_1_based = TRUE,
          DataProvider = "Gencode",
          Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
          RDataClass = "GRanges",
          DispatchClass="GFF3File",
          SourceType="GFF",
          Location_Prefix=.gencodeBaseUrl,
          RDataDateAdded = Sys.time(),
          Recipe=NA_character_))
}




## STEP 2:  Call the helper to set up the newResources() method
makeAnnotationHubResource("GencodeGffImportPreparer",
                          makeGencodeGFFsToAHMs)
