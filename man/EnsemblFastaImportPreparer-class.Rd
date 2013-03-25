\name{EnsemblFastaImportPreparer-class}
\docType{class}
\alias{EnsemblFastaImportPreparer-class}
\alias{EnsemblFastaImportPreparer}

\alias{newResources,EnsemblFastaImportPreparer-method}

\title{Class \code{EnsemblFastaImportPreparer}}

\description{

  The \code{EnsemblFastaImportPreparer} is used to maintain AnnotationHub
  resources corresponding to Fasta files provided by Ensembl. It
  implements \code{newResources,EnsemblFastaImportPreparer-method} for
  resource discovery, and uses the \code{importFastaFiles} recipe.

}

\usage{
   EnsemblFastaImportPreparer(...)

\S4method{newResources}{EnsemblFastaImportPreparer}(importPreparer,
          currentMetadata=list(), ...)
}

\arguments{

  \item{\ldots}{Further arguments to the constructor; ignored.}

  \item{importPreparer}{An instance of \code{EnsemblFastaImportPreparer},
    used for method dispatch.}

  \item{currentMetadata}{A list of
    \code{\linkS4class{AnnotationHubMetadata}} instances, representing
    already-curated data from the data provider indicated by
    \code{object}.}

}

\details{

  Resources are discovered by visiting \url{ftp://ftp.ensembl.org/pub/},
  searching for releases of version greater than or equal to 69,
  locating a subdirectory fasta, and finding all subdirectories with files
  ending in \sQuote{fasta.gz}.

  A resource is \sQuote{new} if its URL is not present in the
  \code{SourceUrl} field of \code{ahmeta}.

  Each \code{AnnotationHubMetadata} object returned by
  \code{newResources} indicates that the recipe
  \code{ensemblFastaToGRangesRecipe} be used to coerce to \code{RData}
  type.

}

\value{

  \code{EnsemblFastaImportPreparer} returns an instance of the class.

  \code{newResources} returns a list of
  \code{\linkS4class{AnnotationHubMetadata}} objects representing
  resources that are not currently in the data base, or \code{list()} if
  there are no new resources.

}

\author{Marc Carlson \url{mcarlson@fhcrc.org}}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}
\examples{
EnsemblFastaImportPreparer()
}
\keyword{classes}




