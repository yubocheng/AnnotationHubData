\name{dbSNPVCFImportPreparer-class}
\docType{class}
\alias{dbSNPVCFImportPreparer-class}
\alias{dbSNPVCFImportPreparer}
\alias{annotationHubRoot,dbSNPVCFImportPreparer-method}
\alias{metadataList,dbSNPVCFImportPreparer-method}
\alias{metadataTable,dbSNPVCFImportPreparer-method}
\alias{sourceUrls,dbSNPVCFImportPreparer-method}

\alias{newResources,dbSNPVCFImportPreparer-method}

\alias{show,dbSNPVCFImportPreparer-method}

\title{Class \code{dbSNPVCFImportPreparer}}

\description{

  The \code{dbSNPVCFImportPreparer} is used to maintain AnnotationHub
  resources obtained from user-contributed molecular interactions.

}

\usage{
dbSNPVCFImportPreparer(annotationHubRoot, verbose=FALSE,
                        maxForTesting=NA)
\S4method{newResources}{dbSNPVCFImportPreparer}(importPreparer,
            currentMetadata=list(), ...)
}

\arguments{

  \item{annotationHubRoot}{a path}
  \item{verbose}{logical}
  \item{importPreparer}{An instance of \code{ImportPreparer}, used for
    method dispatch.}
   \item{maxForTesting}{An \code{integer} variable which can be used to
          limit the number of metadata files retrieved from dbSNPVCF.  Note that
          each metadata file can describe any number -- from a few to thousands
            -- of data files.}
  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the import preparer indicated by
    \code{importPreparer}.}
  \item{\dots}{Additional arguments, used by methods.}

}

\value{

  \code{dbSNPVCFImportPreparer} is the constructor; an object of the class
  is returned.
  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not in \code{currentMetadata}.

}

\details{

  Resources are discovered by visiting
  \url{http://dbSNPVCF.stemcells.cam.ac.uk/}.

  A resource is \sQuote{new} if its URL is not present in the
  \code{SourceUrl} field of \code{ahmeta}.

  \code{annotationHubRoot}, \code{metadataTable}, \code{metadataList}
  and \code{sourceUrls} are all \emph{getters} on an instance of this class.
}

%%\value{
%%  \code{dbSNPVCFImportPreparer} returns an instance of the class.
%%  \code{\linkS4class{AnnotationHubMetadata}} objects representing
%%  resources that are not currently in the data base, or \code{list()} if
%%  there are no new resources.
%%}

\author{Paul Shannon \url{pshannon@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
% dbSNPVCFImportPreparer("/tmp", maxForTesting=1)
}

\keyword{classes}




