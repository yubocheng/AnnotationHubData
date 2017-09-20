\name{makeAnnotationHubMetadata}

\alias{makeAnnotationHubMetadata}

\title{
  Make AnnotationHubMetadata objects from csv file of metadata
}

\description{
  Make AnnotationHubMetadata objects from .csv files located in 
  the "inst/extdata/" package directory of an AnnotationHub package.
}

\usage{
  makeAnnotationHubMetadata(pathToPackage, fileName=character())
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including the package name; no trailing slash
  }
  \item{fileName}{
    Name of metadata file(s) with csv extension. If none are provided, all
    files with .csv extension in "inst/extdata" will be processed.
  }
}

\details{
  \itemize{
    \item{makeAnnotationHubMetadata:}{
      Reads the resource metadata from .csv files into a
      \link{AnnotationHubMetadata} object. The \link{AnnotationHubMetadata}
      is inserted in the AnnotationHub database. Intended for internal
      use or package authors checking the validity of package metadata.
    }
  }
}

\value{
    A named list the length of \code{fileName}. Each element is a list of
    of \code{AnnotationHubMetadata} objects created from the .csv file.
}

\seealso{
  \itemize{
    \item \code{\link{updateResources}}
    \item \code{\link[AnnotationHub]{readMetadataFromCsv}}
    \item \code{\link{AnnotationHubMetadata}} class
  }
}

\examples{
\dontrun{
makeAnnotationHubMetadata("path/to/mypackage")
}
}

\keyword{methods}
