\name{makeAnnotationHubMetadata}

\alias{makeAnnotationHubMetadata}

\title{
  Make AnnotationHubMetadata objects from csv file of metadata
}

\description{
  Make AnnotationHubMetadata objects from metadata.csv file located in 
  the "inst/extdata/" package directory of an AnnotationHub package.
}

\usage{
  makeAnnotationHubMetadata(pathToPackage)
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including the package name; no trailing slash
  }
}

\details{
  \itemize{
    \item{makeAnnotationHubMetadata:}{
      Reads the resource metadata in the metadata.csv file into a
      \link{AnnotationHubMetadata} object. The \link{AnnotationHubMetadata}
      is inserted in the AnnotationHub database. Intended for internal
      use or package authors checking the validity of package metadata.
    }
  }
}

\value{
    A list of \code{AnnotationHubMetadata} objects.
}

\seealso{
  \itemize{
    \item \code{\link{updateResources}}
    \item \code{\link{readMetadataFromCsv}}
    \item \code{\link{AnnotationHubMetadata}} class
  }
}

\examples{
\dontrun{
makeAnnotationHubMetadata("path/to/mypackage")
}
}

\keyword{methods}
