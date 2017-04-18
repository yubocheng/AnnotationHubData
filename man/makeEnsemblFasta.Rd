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
                      baseDir = "fasta/", release,
                      justRunUnitTest = FALSE, BiocVersion = biocVersion())

makeEnsemblTwoBitToAHM(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
                       baseDir = "fasta/", release,
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
  \item{release}{
    Integer version number, e.g., "84".
  } 
  \item{justRunUnitTest}{
    A \code{logical}. When TRUE, a small number of records (usually 5) are
    processed instead of all.
  }
  \item{BiocVersion}{
    A \code{character(1)} Bioconductor version. The resource will be available
    in Bioconductor >= to this version. Default value is the current version,
    specified with biocVersion().
  }
  \item{ahm}{
    List of \code{AnnotationHubMetadata} instances.
  }
}

\details{
  \code{makeEnsemblFastaToAHM} and \code{makeEnsemblTwoBitToAHM} process
  metadata into a list of \code{AnnotationHubMetadata} objects. 

  \code{ensemblFastaToFaFile} unzips a .gz files, creates and index and
  writes out .rz and .rz.fai files to disk.
  \code{ensemblFastaToTwoBit} converts a fasta file to twobit format and
  writes the .2bit file out to disk.
}

\value{
  \code{makeEnsemblFastaToAHM} and \code{makeEnsemblTwoBitToAHM} return
  a list of \code{AnnotationHubMetadata} objects. 

  \code{ensemblFastaToFaFile} write out .rz and .rz.fai files to disk.
  \code{ensemblFastaToTwoBit} writes out a .2bit file to disk.
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

## 'release' is passed to makeEnsemblFastaToFaFile.
\dontrun{
meta <- updateResources("/local/path", 
                        BiocVersion = c("3.2", "3.3"),
                        preparerClasses = "EnsemblFastaImportPreparer",
                        metadataOnly = TRUE, insert = FALSE,
                        justRunUnitTest = FALSE, release = "83")
}
}

\keyword{methods}
