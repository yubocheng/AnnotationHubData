\name{EnsemblGtfImportPreparer-class}
\docType{class}
\alias{EnsemblGtfImportPreparer-class}
\alias{EnsemblGtfImportPreparer}

\alias{newResources,EnsemblGtfImportPreparer-method}

\title{Class \code{EnsemblGtfImportPreparer}}

\description{

  The \code{EnsemblGtfImportPreparer} is used to maintain AnnotationHub
  resources corresponding to GTF files provided by Ensembl. It
  implements \code{newResources,EnsemblGtfImportPreparer-method} for
  resource discovery, and uses \code{ensemblGtfToGRangesRecipe} to
  coerce GTF files to \code{GRanges} instances.

}

\usage{
EnsemblGtfImportPreparer(...)

\S4method{newResources}{EnsemblGtfImportPreparer}(importPreparer,
            currentMetadata=list(), ...)
}

\arguments{

  \item{\ldots}{Further arguments to the constructor; ignored.}

  \item{importPreparer}{An instance of \code{EnsemblGtfImportPreparer},
    used for method dispatch.}

  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the data provider indicated by
    \code{object}.}

}

\details{

  Resources are discovered by visiting \url{ftp://ftp.ensembl.org/pub/},
  searching for releases of version greater than or equal to 69,
  locating a subdirectory gtf, and finding all subdirectories with files
  ending in \sQuote{gtf.gz}.

  A resource is \sQuote{new} if its URL is not present in the
  \code{SourceUrl} field of \code{ahmeta}.

  Each \code{AnnotationHubMetadata} object returned by
  \code{newResources} indicates that the recipe
  \code{ensemblGtfToGRangesRecipe} be used to coerce to \code{RData}
  type.

}

\value{

  \code{EnsemblGtfImportPreparer} returns an instance of the class.

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
% EnsemblGtfImportPreparer()
}
\keyword{classes}




