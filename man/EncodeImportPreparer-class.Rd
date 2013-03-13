\name{EncodeImportPreparer-class}
\docType{class}
\alias{EncodeImportPreparer-class}
\alias{EncodeImportPreparer}
\alias{annotationHubRoot,EncodeImportPreparer-method}
\alias{metadataList,EncodeImportPreparer-method}
\alias{metadataTable,EncodeImportPreparer-method}
\alias{newResources,ImportPreparer,list-method}


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
   newResources(importPreparer, currentMetadata=list(), ...)
}

\arguments{

   \item{importPreparer}{An instance of \code{EncodeImportPreparer},
         used for method dispatch.}
   \item{tbl.md}{A data.frame with previously obtained and parsed
         EncodeDCC metadata.}

   \item{maxForTesting}{An \code{integer} variable which can be used to
          limit the number of metadata files retrieved from Encode.  Note that
          each metadata file can describe any number -- from a few to thousands
            -- of data files.}

  \item{verbose}{a \code{logical} variable.}
    
  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the data provider indicated by
    \code{object}.}
  
  \item{\ldots}{Further arguments to the constructor; ignored.}

}

\details{

  Resources are discovered by visiting
  \url{http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/}
  searching for metadata files, of which we currently (March 2013) find
  55, one per directory.   Their contents are parsed, and used to construct metadata in
  the form needed for the AnnotationHub server.

  A resource is \sQuote{new} if its URL is not present in the
  \code{SourceUrl} field of \code{ahmeta}.

  \code{annotationHubRoot}, \code{metadataTable} and \code{metadataList}
  are all \emph{getters} on an instance of this class.
}

\value{

  \code{EncodeImportPreparer} returns an instance of the class.

  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not currently in the data base, or \code{list()} if
  there are no new resources.

}

\author{Paul Shannon \url{pshannon@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
EncodeImportPreparer("/tmp", maxForTesting=1)
}
\keyword{classes}




