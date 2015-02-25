.haemcodeBaseUrl <- "http://haemcode.stemcells.cam.ac.uk/"

.getHaemCodeFileNames <- function() {
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
    
    list(files= unlist(fileurls), metadata = metadata)
}

.getHaemCode <- function() {
    result <- .getHaemCodeFileNames() 
    
    haemfiles <- result$files
    tags <- result$metadata
    
    haemfiles <- head(haemfiles, 10)
    tags <- head(tags,10)
 
    if(length(haemfiles)==0) 
        stop(" File List not found! ")
    
    df <- .httrFileInfo(haemfiles, verbose=TRUE)
    title <- basename(haemfiles)
    type <- tools::file_ext(title)
    
    fileType <- sapply(type, function(x) 
        switch(x, bw="bigWig", bed="peak", csv="geneList"), 
        USE.NAMES =FALSE)
    
    description <- paste0(fileType, " file from Haemcode")
        
    rdataclass <- sapply(type, function(x) 
        switch(x, bw="GRanges", bed="GRanges", csv="data.frame"), 
        USE.NAMES =FALSE)
    
    dispatchclass <- sapply(type, function(x) 
        switch(x, bw="importBigWig", bed="importBed", csv="data.frame"), 
        USE.NAMES =FALSE)
    
    sourcetype <- sapply(type, function(x) 
        switch(x, bw="BigWig file", bed="BED  file", csv="CSV file"), 
        USE.NAMES =FALSE)
    
    cbind(df, title,  description, fileType, tags, rdataclass, dispatchclass,
          sourcetype, stringsAsFactors=FALSE)
    
}

makeHaemCodeImporter <- function(currentMetadata) {
    rsrc <- .getHaemCode()
        
    ## input_sources table
    sourceSize <- as.numeric(rsrc$size)
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    sourceType <- rsrc$sourcetype
    
    ## resources table
    title <- rsrc$title
    # dataprovider, species, taxonomyid, genome are same for all files
    description <- rsrc$description
    # maintainer, cordinateBased, status_id, location_prefix, rdataadded, 
    # preparerclss are same for all files
    
    rdatapath <- sub(.haemcodeBaseUrl, "", sourceUrls)
    rdataclass <- rsrc$rdataclass   
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
        RDataClass = rdataclass,
        DispatchClass = dispatchclass,
        
        Tags=tags,
        
        MoreArgs=list(
            DataProvider = "Haemcode",
            Species="Mus musculus",
            TaxonomyId=10090L,
            Genome= "mm10",
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",            
            Coordinate_1_based = FALSE,
           ## status_id =2L, ## internal ID 
            Location_Prefix = .haemcodeBaseUrl,
            RDataDateAdded = Sys.time(),
           # PreparerClass = "HaemCodeImportPreparer", ## Automatic  ;)
            Recipe = NA_character_)
            )
}

makeAnnotationHubResource("HaemCodeImportPreparer", makeHaemCodeImporter)
