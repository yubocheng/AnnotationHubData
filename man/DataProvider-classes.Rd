\name{DataProvider-class}
\docType{class}
\alias{DataProvider-class}
\alias{DataProvider}
\alias{EncodeDCCProvider-class}
\alias{EncodeDCCProvider}
\alias{UCSCTableHubProvider-class}
\alias{UCSCTableHubProvider}

\alias{show,DataProvider-method}

\alias{newResources}


\title{Class \code{DataProvider} and derived classes}

\description{
  The \code{DataProvider} and derived classes are used internally for
  dispatch during data discovery (see \code{\link{newResources}}). There
  is one \code{DataProvider} class for each distinct entry in the
  \code{DataProvider} slot of \code{\link{AnnotationHubMetadata}}.

  \code{newResources} is a generic function; with methods implemented
  for each \code{DataProvider}.

}

\usage{
EncodeDCCProvider(...)
UCSCTableHubProvider(...)

newResources(object, ahmeta)
}

\arguments{

  \item{\ldots}{Further arguments to the constructor; ignored.}

  \item{object}{An instance of \code{DataProvider}, used for method
    dispatch.}

  \item{ahmeta}{A list of \code{\linkS4class{AnnotationHubMetadata}}
    instances, representing already-curated data from the data
    provider indicated by \code{object}.}

}

\section{Objects from \code{DataProvider}-derived Class}{
  
  Objects can be created by calls of the form
  \code{EncodeDCCProvider()}; the \code{DataProvider} class is itself a
  virtual class.

}

\value{

  \code{DataProvider}-derived classes return an instance of the
  corresponding class.

  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not currently in the data base, or \code{list()} if
  there are no new resources.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
names(getClassDef("DataProvider")@subclasses)
EncodeDCCProvider()
}
\keyword{classes}




