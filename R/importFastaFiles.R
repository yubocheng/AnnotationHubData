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
