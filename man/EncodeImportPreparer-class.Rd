\name{EncodeImportPreparer-class}
\docType{class}
\alias{EncodeImportPreparer-class}
\alias{EncodeImportPreparer}
\alias{annotationHubRoot}
\alias{annotationHubRoot,EncodeImportPreparer-method}
\alias{metadataList}
\alias{metadataList,EncodeImportPreparer-method}
\alias{metadataTable}
\alias{metadataTable,EncodeImportPreparer-method}
\alias{sourceUrls}
\alias{sourceUrls,EncodeImportPreparer-method}

\alias{newResources,EncodeImportPreparer-method}

\alias{show,EncodeImportPreparer-method}

\title{Class \code{EncodeImportPreparer}}

\description{

  The \code{EncodeImportPreparer} is used to maintain AnnotationHub
  resources obtained from the EncodeDCC (data curation center), which is
  hosted by UCSC.  Four formats are supported, all of which are
  variations on the well-known BED format.  This ImportPreparer assigns
  recipes for these formats which transform the data into GRanges.

}

\usage{
EncodeImportPreparer(annotationHubRoot, tbl.md=NULL, verbose=FALSE,
                        maxForTesting=NA)
\S4method{newResources}{EncodeImportPreparer}(importPreparer,
            currentMetadata=list(), ...)
}

\arguments{

  \item{annotationHubRoot}{a path}
   \item{tbl.md}{A data.frame with previously obtained and parsed
         EncodeDCC metadata.}

  \item{verbose}{logical}
  \item{importPreparer}{An instance of \code{ImportPreparer}, used for
    method dispatch.}
   \item{maxForTesting}{An \code{integer} variable which can be used to
          limit the number of metadata files retrieved from Encode.  Note that
          each metadata file can describe any number -- from a few to thousands
            -- of data files.}
  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the import preparer indicated by
    \code{importPreparer}.}
  \item{\dots}{Additional arguments, used by methods.}

}

\value{

  \code{EncodeImportPreparer} is the constructor; an object of the class
  is returned.
  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not in \code{currentMetadata}.

}

\details{

  Resources are discovered by visiting
  \url{http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/}
  searching for metadata files, of which we currently (March 2013) find
  55, one per directory.   Their contents are parsed, and used to construct metadata in
  the form needed for the AnnotationHub server.

  A resource is \sQuote{new} if its URL is not present in the
  \code{SourceUrl} field of \code{ahmeta}.

  \code{annotationHubRoot}, \code{metadataTable}, \code{metadataList}
  and \code{sourceUrls} are all \emph{getters} on an instance of this class.
}

%%\value{
%%  \code{EncodeImportPreparer} returns an instance of the class.
%%  \code{\linkS4class{AnnotationHubMetadata}} objects representing
%%  resources that are not currently in the data base, or \code{list()} if
%%  there are no new resources.
%%}

\author{Paul Shannon \url{pshannon@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
% EncodeImportPreparer("/tmp", maxForTesting=1)
}

\keyword{classes}




