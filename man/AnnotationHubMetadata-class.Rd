\name{AnnotationHubMetadata-class}
\docType{class}
\alias{jsonPath}
\alias{AnnotationHubMetadataFromJson}
\alias{toJson}
\alias{constructAnnotationHubMetadataFromSourceFilePath}
\alias{constructMetadataFromJsonPath}
\alias{postProcessMetadata}
\alias{metadata}
\alias{metadata,AnnotationHubMetadata-method}
\alias{metadata,AnnotationHubMetadata,list-method}
\alias{metadata<-}
\alias{metadata<-,AnnotationHubMetadata-method}
\alias{metadata<-,AnnotationHubMetadata,list-method}


\title{Class \code{"AnnotationHubMetadata"} and methods}

\description{

  \code{AnnotationHubMetadata} is used to represent record(s) in the
  server data base.

}

\section{Objects from the Class}{

  Objects can be created by calls to the constructor,
  \code{AnnotationHubMetadata()}.

}

\usage{
jsonPath(x)
toJson(x)
constructAnnotationHubMetadataFromSourceFilePath(ahroot,
    RDataVersion, originalFile)
constructMetadataFromJsonPath(ahroot, jsonpath)
postProcessMetadata(ahm)
}

\arguments{
    \item{ahm}{An instance of class \code{AnnotationHubMetadata}.}
    \item{x}{An instance of class \code{AnnotationHubMetadata}.}
    \item{RDataVersion}{\code{numeric_version()} Version of resource}
    \item{jsonpath}{\code{character(1)} full path to a JSON representation
        of \code{AnnotationHubMetadata}-class.}
    \item{ahroot}{A \code{character(1)} representing the value of
      \code{AnnotationHubRoot} to be added to the returned instance.}
    \item{originalFile}{A\code{character(1)}}
}

\value{
  \code{jsonPath} returns a \code{character(1))} representation of the
  full path to the location of the \code{json} file associated with this
  record.

  \code{toJson} returns the JSON representation of the record.

  \code{fromJson} retuns an instance of the class, as parsed from the
  JSON file.
}

\author{Dan Tenenbaum}

\seealso{
\code{\linkS4class{AnnotationHubRecipe}}
}
\examples{
getClass("AnnotationHubMetadata")
}

\keyword{classes}
