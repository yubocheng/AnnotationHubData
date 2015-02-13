.haemcodeBaseUrl <- "http://haemcode.stemcells.cam.ac.uk"

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
    
    paths <- c(bigWig="/blood/BigWig/mm10",
               peaks="/blood/Peaks/mm10",
               geneList="/blood/geneList")
    
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
    type <- file_ext(title)
    
    fileType <- sapply(type, function(x) 
        switch(x, bw="bigWig", bed="peak", csv="geneList"), 
        USE.NAMES =FALSE)
    
    description <- paste0(fileType, " file from Haemcode")
    
    #rdataclass <- # multiple types - 2 will use import, 1 will use data.frame?
    # dispatchclass
    
    cbind(df, title,  description, fileType, tags, #rdataclass, dispatchclass,
          stringsAsFactors=FALSE)
    
}

makeHaemCodeImporter <- function(currentMetadata) {
    rsrc <- .getHaemCode()
    
    description <- rsrc$description
    sourceFile <- rsrc$title
    title <- rsrc$title
    sourceUrls <- rsrc$fileurl
    sourceVersion <- gsub(" ", "_", rsrc$date) # should be character
    SourceLastModifiedDate <- rsrc$date  # should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    tags <- strsplit(rsrc$tags, ", ")
    rdataclass <- rsrc$rdataclass   
    
    Map(AnnotationHubMetadata,
        Description=description, 
        SourceFile=sourceFile, SourceUrl=sourceUrls,
        SourceLastModifiedDate = SourceLastModifiedDate,
        SourceSize = SourceSize,
        RDataPath=sourceUrls,
        SourceVersion=sourceVersion,
        Title=title, Tags=Tags,
        RDataClass = rdataclass,
        MoreArgs=list(
            Genome= "mm10",
            Species="Mus musculus",
            TaxonomyId=10090L,
            Coordinate_1_based = FALSE,
            DataProvider = "Haemcode",
            Location_Prefix = .haemcodeBaseUrl,
            Maintainer = "Sonali Arora <sarora@fredhutch.org>",
            RDataDateAdded = Sys.time(),
            RDataVersion = "0.0.2",
            Recipe = NA_character_))
}

makeAnnotationHubResource("HaemCodeImportPreparer", makeHaemCodeImporter)