\name{RefNetImportPreparer-class}
\docType{class}
\alias{RefNetImportPreparer-class}
\alias{RefNetImportPreparer}
\alias{annotationHubRoot,RefNetImportPreparer-method}
\alias{metadataList,RefNetImportPreparer-method}
\alias{metadataTable,RefNetImportPreparer-method}
\alias{sourceUrls,RefNetImportPreparer-method}

\alias{newResources,RefNetImportPreparer-method}

\alias{show,RefNetImportPreparer-method}

\title{Class \code{RefNetImportPreparer}}

\description{

  The \code{RefNetImportPreparer} is used to maintain AnnotationHub
  resources obtained from user-contributed molecular interactions.

}

\usage{
RefNetImportPreparer(annotationHubRoot, verbose=FALSE,
                        maxForTesting=NA)
\S4method{newResources}{RefNetImportPreparer}(importPreparer,
            currentMetadata=list(), ...)
}

\arguments{

  \item{annotationHubRoot}{a path}
  \item{verbose}{logical}
  \item{importPreparer}{An instance of \code{ImportPreparer}, used for
    method dispatch.}
   \item{maxForTesting}{An \code{integer} variable which can be used to
          limit the number of metadata files retrieved from RefNet.  Note that
          each metadata file can describe any number -- from a few to thousands
            -- of data files.}
  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the import preparer indicated by
    \code{importPreparer}.}
  \item{\dots}{Additional arguments, used by methods.}

}

\value{

  \code{RefNetImportPreparer} is the constructor; an object of the class
  is returned.
  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not in \code{currentMetadata}.

}

\details{

  Resources are discovered by visiting
  \url{http://s3.amazonaws.com/refnet-networks/}
  where tab-delimited files are stored, each with the same set of
  mandatory columns.

  A resource is \sQuote{new} if its URL is not present in the
  \code{SourceUrl} field of \code{ahmeta}.

  \code{annotationHubRoot}, \code{metadataTable}, \code{metadataList}
  and \code{sourceUrls} are all \emph{getters} on an instance of this class.
}

%%\value{
%%  \code{RefNetImportPreparer} returns an instance of the class.
%%  \code{\linkS4class{AnnotationHubMetadata}} objects representing
%%  resources that are not currently in the data base, or \code{list()} if
%%  there are no new resources.
%%}

\author{Paul Shannon \url{pshannon@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
RefNetImportPreparer("/tmp", maxForTesting=1)
}

\keyword{classes}




