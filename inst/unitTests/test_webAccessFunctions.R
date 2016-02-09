m6Url <- "ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_mouse/release_M6/"
hg19Url <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/"
hg19RnaSeqUrl <- paste0(hg19Url,"wgEncodeCshlLongRnaSeq/")
hg19MasterSitesUrl <- paste0(hg19Url,"wgEncodeAwgDnaseMasterSites/")

bioPaxUrl <- "ftp://ftp1.nci.nih.gov/pub/PID/BioPAX/"
bioPaxKeggUrl <- paste0(bioPaxUrl,"KEGG.bp2.owl.gz")
bioPaxCartaUrl <- paste0(bioPaxUrl,"BioCarta.owl.gz")
## We know the Bio Pax FTP server exists, and the following file does not
badBioPaxUrl <- paste0(bioPaxUrl,"FILE_DOES_NOT_EXIST.owl.gz")

rProjUrl <- "https://www.r-project.org/"
biocUrl <- "http://bioconductor.org/"
initialTimeout <- getOption("timeout")

setup <- function(){
  options(timeout=5*60)
}

tearDown <- function(){
  options(timeout=initialTimeout)
}

.httrRead <- AnnotationHubData:::.httrRead
test_httrRead <- function(){
  fileInfo <- .httrRead(hg19RnaSeqUrl, "//pre/a/text()")
  checkTrue(validObject(fileInfo))
  checkTrue(length(fileInfo[[1]]) > 0)

  fileInfo <- .httrRead(hg19MasterSitesUrl, "//pre/a/text()")
  checkTrue(validObject(fileInfo))
  checkTrue(length(fileInfo[[1]]) > 0)
}

test_ftpDirectoryInfo <- function(){
  setup()
  listOfFiles <- AnnotationHubData:::.ftpDirectoryInfo(m6Url)
  checkTrue(length(listOfFiles[[1]]) > 0)
  tearDown()
}

test_ftpFileInfo <- function(){
    setup()
    fileInfo <- AnnotationHubData:::.ftpFileInfo(bioPaxUrl, "BioCarta.owl.gz", "testgenome")
    checkTrue(validObject(fileInfo))
    checkEquals(length(fileInfo), 4)

    fileInfo2 <- AnnotationHubData:::.ftpFileInfo(bioPaxUrl, "NCI-Nature_Curated.owl.gz", "testgenome")
    checkTrue(validObject(fileInfo2))
    checkEquals(length(fileInfo2), 4)

    fileInfo3 <- AnnotationHubData:::.ftpFileInfo(bioPaxUrl, "FILE_THAT_DOESNT_EXIST.gz", "testgenome")
    checkTrue(is.na(fileInfo3["size"])[1])
    # checkException(.isComplete(invalid), silent=TRUE)
    # checkIdentical(ahm, ahm1)
    tearDown()
}

test_fileExistsOnline <- function(){
    result <- AnnotationHubData:::.fileExistsOnline(bioPaxKeggUrl)
    checkEquals(result[[1]], TRUE)

    failureResult <- AnnotationHubData:::.fileExistsOnline(badBioPaxUrl)
    checkEquals(failureResult[[1]], FALSE)
}

test_ftpDirectoryInfo <- function(){
  setup()
  res <- AnnotationHubData:::.ftpDirectoryInfo(bioPaxUrl)
  checkTrue(length(res) > 0)
  tearDown()
}

test_listRemoteFiles <- function(){
  setup()
  res <- AnnotationHubData:::.listRemoteFiles(bioPaxUrl)
  checkTrue(length(res) > 0)
  tearDown()
}

test_getGenomeAbbrevs <- function(){
  smallSample <- c("hg19", "hg18", "hg17")
  actualResult <- AnnotationHubData:::.getGenomeAbbrevs(smallSample)
  warning(paste0("\nResult from 'getGenomeAbbrevs': ",actualResult), immediate.=TRUE)
  expectedResult <- sort(smallSample)
  checkEquals(actualResult, expectedResult)

  # Viewing the FTP server content at ftp://hgdownload.cse.ucsc.edu/goldenPath/ , you'll
  # notice that some files are actually symlinks to other directories.  Since the
  # getGenomeAbbrevs function claims to handle symlinks, we must test that specific case.
  # The following are files that actually render the client a symlink. :
  ### cb1 -> cbJul2002
  ### ce1 -> ceMay2003
  ### hg15 -> 10april2003/"
  ### rn2 -> rnJan2003
  # You should notice, however that results are returned in a sorted order.
  sampleWithSymlink <- c("hg15", "cb1", "rn2", "ce1")
  resultWithSymlink <- AnnotationHubData:::.getGenomeAbbrevs(sampleWithSymlink)
  warning(paste0("\nResult from 'getGenomeAbbrevs': ",resultWithSymlink), immediate.=TRUE)
  expectedResult <- sort(sampleWithSymlink)
  checkEquals(resultWithSymlink, expectedResult)
}
