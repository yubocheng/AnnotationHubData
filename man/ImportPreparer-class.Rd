\name{ImportPreparer-class}
\docType{class}
\alias{ImportPreparer-class}
\alias{show,ImportPreparer-method}

\alias{newResources}

\title{Class \code{ImportPreparer} and generic \code{newResources}}

\description{

  The \code{ImportPreparer} and derived classes are used for dispatch
  during data discovery (see \code{\link{newResources}}). There is one
  \code{ImportPreparer} class for each data source for
  \code{\link{AnnotationHubMetadata}}.

  \code{newResources} is a generic function; with methods implemented
  for each \code{ImportPreparer}.

}

\usage{
newResources(importPreparer, currentMetadata)
}

\arguments{

  \item{importPreparer}{An instance of \code{ImportPreparer}, used for
    method dispatch.}

  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the import preparer indicated by
    \code{importPreparer}.}

}

\value{

  \code{ImportPreparer} is a virtual class; derived classes return an
  instance of the corresponding class.

  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not in \code{currentMetadata}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
names(getClassDef("ImportPreparer")@subclasses)
}
\keyword{classes}




