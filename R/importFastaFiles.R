baseUrl <- "ftp://ftp.ensembl.org/pub/release-70/fasta/"

.printf <- function(...) print(noquote(sprintf(...)))

getItems <- function(txt, dirs=TRUE)
{
    lines <- unlist(strsplit(txt, "\n"))
    segs <- strsplit(lines, " ")
    unlist(sapply(segs, function(x){
    if (dirs)
    {
      if(grepl("^d", x[[1]]))
        x[[length(x)]]
    } else {
      if(!grepl("^d", x[[1]]))
        x[[length(x)]]
    }
    
    }))
 }

parseFileListing <- function(file)
{
  lines <- readLines(file)
  skip <- TRUE
  currentDir <- NULL
  urls <- c()
  sizes <- c()
  for (line in lines)
  {
      if (skip)
      {
          if(grepl("^d", line))
              next
          else
              skip <- FALSE
              next
      }
      if (line == "")
          next
      
      if(grepl("^\\.", line))
      {
          currentDir <- sub("^\\.", "", line)
          currentDir <- sub(":$", "", currentDir)
          .printf("currentdir is %s", currentDir)
          
      }
          if (grepl("\\.gz$", line))
          {
              print(line)
              segs <- unlist(strsplit(line, " "))
              filename <- segs[[length(segs)]]
              tmp <- setdiff(segs, "")
              size <- as.integer(tmp[5])
              url <- sprintf("%s/%s", currentDir, filename)
              urls <- c(urls, url)
              sizes <- c(sizes, size)
          }
  
  }
  names(sizes) <- urls
  sizes
}

filterFiles <- function(input)
{
    indices <- grep("_rm\\.|_sm\\.|_PATCH\\.", names(input), invert=TRUE)
    input[indices]
}

commonMetadata <- function(BiocVersion)
{
    params <- list()
    params$Recipe <- "ensembl.fasta"
    params$RecipeArgs <- list()
    params$RDataClass <- "fasta"
    params$SourceVersion <- "release-70"
    params$Maintainer <- "Dan Tenenbaum <dtenenba@fhcrc.org>"
    params$DataProvider <- "ftp.ensembl.org"
    params$Coordinate_1_based <- TRUE#logical(0)
    params$RDataDateAdded <- format(Sys.time(), "%Y-%m-%d")
    ##params$SourceLastModifiedDate <- "2012-12-18"
    ##params$BiocVersion <- BiocVersion
    params$Tags <- c("ensembl", "fasta")
    params
}

createMetadata <- function(ahroot, subtree="pub/release-70/fasta",
    RDataVersion="0.0.1", BiocVersion="2.12")
{
    
    path <- file.path(ahroot, subtree)
    gzfiles <- dir(path, pattern="*.gz$", recursive=TRUE)
    for (gzfile in gzfiles)
    {
        .printf("Processing %s...", gzfile)
        params <- commonMetadata(BiocVersion)
        params$RDataVersion <- RDataVersion
        params$SourceUrl <-
            sprintf("ftp://ftp.ensembl.org/pub/release-70/fasta/%s", gzfile)
        params$SourceFile <- file.path(subtree, gzfile)
        basename <- basename(gzfile)
        params$Title <- basename
        segs <- strsplit(basename, ".", fixed=TRUE)[[1]]
        params$Species <- sub("_", " ", segs[1])
        params$Genome <- segs[2]
        if(grepl("\\.chromosome\\.", basename))
        {
            chr <- which(segs == "chromosome")
            chromosome <- segs[chr+1]
            type <- segs[chr-1]
            params$Tags <- c(params$Tags, sprintf("chromosome %s", chromosome),
                             type)
        }
        params$Description <- sprintf("FASTA file for %s", params$Species)
        ## TODO - indexFa and set DerivedSource and size appropriately
        indexFa(file.path(ahroot, subtree, gzfile))
        index <- sprintf("%s.fai", gzfile)
        fullIndex <- file.path(ahroot, subtree, index)
        params$AnnotationHubRoot <- ahroot
        md <- do.call(AnnotationHubMetadata, params)
        metadata(md)$RDataPath <- file.path(subtree, gzfile)
        metadata(md)$DerivedMd5 <-
            unname(tools::md5sum(path.expand(fullIndex)))
        metadata(md)$RDataLastModifiedDate <-
            AnnotationHubData:::.getModificationTime(fullIndex)
        metadata(md)$RDataSize <- as.integer(file.info(fullIndex)$size)
        AnnotationHubData:::writeJSON(ahroot, md)
        
    }
}