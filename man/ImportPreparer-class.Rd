\name{ImportPreparer-class}
\docType{class}
\alias{ImportPreparer-class}
\alias{show,ImportPreparer-method}

\alias{UCSCTrackImportPreparer-class}
\alias{UCSCTrackImportPreparer}

\alias{newResources,UCSCTrackImportPreparer-method}
\alias{newResources,UCSCFullTrackImportPreparer-method}

\alias{newResources}
\alias{newResources,ImportPreparer-method}


\title{Class \code{ImportPreparer} and generic \code{newResources}}

\description{

  The \code{ImportPreparer} and derived classes are used for dispatch
  during data discovery (see \code{\link{newResources}}). There is one
  \code{ImportPreparer} class for each data source for
  \code{\link{AnnotationHubMetadata}}.

  \code{newResources} is a generic function; with methods implemented
  for each \code{ImportPreparer}.

}


\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
names(getClassDef("ImportPreparer")@subclasses)
}
\keyword{classes}




