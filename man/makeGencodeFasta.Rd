\name{makeGencodeFasta}

\alias{makeGencodeFasta}
\alias{makeGencodeFastaToAHM}
\alias{gencodeFastaToFaFile}

\title{
  Recipe to add Gencode FASTA resources to AnnotationHub
}

\description{
  Create metadata and process raw Gencode FASTA files for inclusion in
  AnnotationHub
}

\usage{
makeGencodeFastaToAHM(currentMetadata, 
                      baseUrl="ftp://ftp.sanger.ac.uk/pub/gencode/",
                      species=c("Human", "Mouse"), release, 
                      justRunUnitTest=FALSE, 
                      BiocVersion=BiocManager::version())

gencodeFastaToFaFile(ahm)
}

\details{
  \itemize{
    \item{Documentation:}{
      http://www.gencodegenes.org/releases/
    }
    \item{File download location:}{
      ftp://ftp.sanger.ac.uk/pub/gencode/. Gencode_human and
      Gencode_mouse are used.
    }
    \item{Files downloaded:}{
      Code is currently specific for human and mouse. Files chosen for
      download are described in AnnotationHubData:::.gencodeDescription().
    }
  }
}

\arguments{
  \item{currentMetadata}{
    Currently not used. Intended to be a list of metadata to filter, i.e.,
    records that do not need to be processed again. Need to remove or fix.
  }
  \item{baseUrl}{
    ftp file location.
  } 
  \item{species}{
    A \code{character(1)} of the species. Currently "Human" and "Mouse"
    are supported.
  } 
  \item{release}{
    A \code{character} string of the release number.
  } 
  \item{justRunUnitTest}{
    A \code{logical}. When TRUE, a small number of records (usually 5) are
    processed instead of all.
  }
  \item{BiocVersion}{
    A \code{character} vector of Bioconductor versions the resources should be 
    available for.
  }
  \item{ahm}{
    List of \code{AnnotationHubMetadata} instances.
  }
}


\value{
  \code{makeGencodeFastaAHM} returns a list of \code{AnnotationHubMetadata}
  instances. \code{gencodeFastaToFaFile} returns nothing.
}

\seealso{
  \itemize{
    \item \link{updateResources}
    \item \link{AnnotationHubMetadata}
  }
}

\author{
  Bioconductor Core Team.
}

\examples{

## updateResources() generates metadata, process records and
## pushes files to AWS S3 buckets. 

## To run the GencodeFasta recipe specify 
## 'preparerClasses = GencodeFastaImportPreparer'. The 'species' and 'release' 
## arguments are passed to makeGencodeFastaAHM().
\dontrun{
meta <- updateResources("/local/path", 
                        BiocVersion = c("3.2", "3.3"),
                        preparerClasses = "GencodeFastaImportPreparer",
                        metadataOnly = TRUE, insert = FALSE,
                        justRunUnitTest = FALSE)

}
}

\keyword{methods}
