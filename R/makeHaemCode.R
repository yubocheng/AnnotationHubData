.haemcodeBaseUrl <- "http://haemcode.stemcells.cam.ac.uk/"

.getHaemCodeFileNames <- function(justRunUnitTest) {
    filename <- system.file("extdata", "haemCodeFileList.txt", 
                            package="AnnotationHubData")
    stopifnot(file.exists(filename))
    file.list <- scan(filename, what=character(0), sep="\n", quiet=TRUE)
    
    metadata.filename <-  system.file(package="AnnotationHubData", "extdata",
                                      "annotation_haemcode.tsv")
    stopifnot(file.exists(metadata.filename))
    tbl.md <- read.table(metadata.filename, sep="\t", header=TRUE, as.is=TRUE)
    metadata <- tbl.md[which(tbl.md$filename == file.list),]
    metadata <- apply(metadata,1, function(x) paste0(x, collapse=", "))
    metadata <- rep(metadata, 3)
    names(metadata) <- NULL
    
    paths <- c(bigWig="blood/BigWig/mm10",
               peaks="blood/Peaks/mm10",
               geneList="blood/geneList")
    
    urls <- paste0(.haemcodeBaseUrl, paths)
    
    file.types <- c("bw", "bed", "csv")
    
    fileurls <- mapply(function(x, y){
        paste0(x, "/", file.list,".",  y)
    }, urls, file.types, USE.NAMES=FALSE, SIMPLIFY=FALSE)
    fileurls <- unlist(fileurls) 
    
    if(justRunUnitTest) {
       fileurls <- fileurls[c(1,2,326,327, 638,639)]
       metadata <- metadata[c(1,2,326,327, 638,639)]
    }

    list(files= fileurls, metadata = metadata)
}

.getHaemCode <- function(justRunUnitTest=FALSE) {
    result <- .getHaemCodeFileNames(justRunUnitTest) 
    
    haemfiles <- result$files
    tags <- result$metadata
    
    if(length(haemfiles)==0) 
        stop(" File List not found! ")
    
    df <- .httrFileInfo(haemfiles, verbose=TRUE)
    title <- basename(haemfiles)
    type <- tools::file_ext(title)
    
    fileType <- sapply(type, function(x) 
        switch(x, bw="bigWig", bed="peak", csv="geneList"), 
        USE.NAMES =FALSE)
    
    description <- paste0(fileType, " file from Haemcode")
        
    dispatchclass <- sapply(type, function(x) 
        switch(x, bw="BigWigFile", bed="BEDFile", csv="CSVtoGranges"), 
        USE.NAMES =FALSE)
    
    sourcetype <- sapply(type, function(x) 
        switch(x, bw="BigWig", bed="BED", csv="CSV"), 
        USE.NAMES =FALSE)
   
    rdataclass <- sapply(type, function(x)
        switch(x, bw="BigWigFile", bed="GRanges", csv="GRanges"),
        USE.NAMES =FALSE)
   
  
    cbind(df, title,  description, fileType, tags, dispatchclass,
          sourcetype, rdataclass, stringsAsFactors=FALSE)
    
}

makeHaemCodeImporter <- function(currentMetadata, justRunUnitTest=FALSE) {
    rsrc <- .getHaemCode(justRunUnitTest)
        
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    sourceType <- rsrc$sourcetype
    rdataclass <- rsrc$rdataclass     

    ## resources table
    title <- rsrc$title
    # dataprovider, species, taxonomyid, genome are same for all files
    description <- rsrc$description
    # maintainer, cordinateBased, status_id, location_prefix, rdataadded, 
    # preparerclss are same for all files
    
    rdatapath <- sub(.haemcodeBaseUrl, "", sourceUrls)
    dispatchclass <- rsrc$dispatchclass
    
    tags <- strsplit(rsrc$tags, ", ")
    
    Map(AnnotationHubMetadata,
        
        SourceSize=sourceSize,
        SourceUrl=sourceUrls,
        SourceVersion=sourceVersion,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceType = sourceType,
        
        Description=description, 
        Title=title, 
                
        RDataPath=rdatapath,
        DispatchClass = dispatchclass,
        RDataClass = rdataclass,        

        Tags=tags,
        
        MoreArgs=list(
            DataProvider = "Haemcode",
            Species="Mus musculus",
            TaxonomyId=10090L,
            Genome= "mm10",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",            
            Coordinate_1_based = FALSE,
            Location_Prefix = .haemcodeBaseUrl,
            RDataDateAdded = Sys.time(),
            Recipe = NA_character_)
            )
}

makeAnnotationHubResource("HaemCodeImportPreparer", makeHaemCodeImporter)
