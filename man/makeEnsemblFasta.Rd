\name{makeEnsemblFasta}

\alias{makeEnsemblFasta}
\alias{makeEnsemblFastaToAHM}
\alias{makeEnsemblTwoBitToAHM}
\alias{ensemblFastaToFaFile}
\alias{ensemblFastaToTwoBitFile}

\title{
  Functions to convert Ensembl FASTA files to FaFile and TwoBitFile for
  inclusion in AnnotationHub.
}

\description{
  Transform an Ensembl FASTA file to a Bioconductor FaFile or ToBitFile.
}

\usage{
makeEnsemblFastaToAHM(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
                      baseDir = "fasta/", regex,
                      justRunUnitTest = FALSE, BiocVersion = biocVersion())

makeEnsemblTwoBitToAHM(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
                       baseDir = "fasta/", regex,
                       justRunUnitTest = FALSE, BiocVersion = biocVersion())

ensemblFastaToFaFile(ahm)

ensemblFastaToTwoBitFile(ahm)
}

\arguments{
  \item{currentMetadata}{
    Currently not used. Intended to be a list of metadata to filter, i.e.,
    records that do not need to be processed again. Need to remove or fix.
  }
  \item{baseUrl}{
    ftp file location.
  } 
  \item{baseDir}{
    ftp file directory.
  } 
  \item{regex}{
    Regular expression to match release version, e.g., "*.release-81".
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

\details{
  TODO
}

\value{
  TODO
}

\author{Bioconductor Core Team}

\seealso{
  \itemize{
    \item \link{updateResources}
    \item \link{AnnotationHubMetadata}
  }
}

\examples{
## updateResources() generates metadata, process records and
## pushes files to AWS S3 buckets. See ?updateResources for details.

## 'regex' is passed to makeEnsemblFastaToFaFile.
\dontrun{
meta <- updateResources("/local/path", 
                        BiocVersion = c("3.2", "3.3"),
                        preparerClasses = "EnsemblFastaImportPreparer",
                        metadataOnly = TRUE, insert = FALSE,
                        justRunUnitTest = FALSE, regex = ".*release-82")
}
}

\keyword{methods}
