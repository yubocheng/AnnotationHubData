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
    params$Maintainer <- "Dan Tenenbaum <dtenenba@fhcrc.org>"
    params$DataProvider <- "ftp.ensembl.org"
    params$Coordinate_1_based <- logical(0)
    params$RDataDateAdded <- format(Sys.time(), "%Y-%m-%d")
    params$SourceLastModifiedDate <- "2012-12-18"
    params$BiocVersion <- BiocVersion
    params$Tags <- c("ensembl", "fasta")
    params
}

createMetadata <- function(ahroot, subtree="pub/release-70/fasta",
    RDataVersion="0.0.1", BiocVersion="2.12")
{
    if (!exists("speciesMap")) data(speciesMap)
    
    path <- file.path(ahroot, subtree)
    gzfiles <- dir(path, pattern="*.gz$", recursive=TRUE)
    for (gzfile in gzfiles)
    {
        params <- commonMetadata(BiocVersion)
        params$RDataVersion <- RDataVersion
        params$SourceMd5 <- tools::md5sum(gzfile)
        params$SourceUrl <-
            sprintf("ftp://ftp.ensembl.org/pub/release-70/fasta/%s", gzfile)
        params$SourceFile <- gzfile
        basename <- basename(gzfile)
        params$Title <- basename
        segs <- strsplit(basename, ".", fixed=TRUE)[[1]]
        params$Species <- sub("_", " ", segs[1])
        params$TaxonomyId <-
            as.character(with(speciesMap, taxon[species == params$Species]))
        params$Genome <- segs[2]
        ## should there be a chromosome field? We can easily parse it from
        ## some of the fasta filenames.
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
    }
}