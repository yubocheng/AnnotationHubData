\name{ImportPreparer-class}
\docType{class}
\alias{newResources}
\alias{getImportPreparer}

\alias{ImportPreparer-class}
\alias{show,ImportPreparer-method}
\alias{newResources,ImportPreparer-method}

\alias{UCSCTrackImportPreparer-class}
\alias{UCSCTrackImportPreparer}
\alias{newResources,UCSCTrackImportPreparer-method}
\alias{newResources,UCSCFullTrackImportPreparer-method}

\alias{HaemCodeImportPreparer-class}
\alias{HaemCodeImportPreparer}
\alias{newResources,HaemCodeImportPreparer-method}

\alias{EncodeImportPreparer-class}
\alias{EncodeImportPreparer}
\alias{newResources,EncodeImportPreparer-method}

\alias{annotationHubRoot}
\alias{metadataList}
\alias{metadataTable}
\alias{sourceUrls}

\alias{EnsemblFastaImportPreparer-class}
\alias{EnsemblFastaImportPreparer}
\alias{newResources,EnsemblFastaImportPreparer-method}

\alias{EnsemblGtfImportPreparer-class}
\alias{EnsemblGtfImportPreparer}
\alias{newResources,EnsemblGtfImportPreparer-method}

\alias{RefNetImportPreparer-class}
\alias{RefNetImportPreparer}
\alias{newResources,RefNetImportPreparer-method}

\alias{dbSNPVCFImportPreparer-class}
\alias{dbSNPVCFImportPreparer}
\alias{newResources,dbSNPVCFImportPreparer-method}

\alias{Grasp2ImportPreparer-class}
\alias{Grasp2ImportPreparer}
\alias{newResources,Grasp2ImportPreparer-method}

\alias{Inparanoid8ImportPreparer-class}
\alias{Inparanoid8ImportPreparer}
\alias{newResources,Inparanoid8ImportPreparer-method}

\alias{NCBIImportPreparer-class}
\alias{NCBIImportPreparer}
\alias{newResources,NCBIImportPreparer-method}

\alias{UCSCChainPreparer-class}
\alias{UCSCChainPreparer}
\alias{newResources,UCSCChainPreparer-method}


\title{Class \code{ImportPreparer} and generic \code{newResources}}

\description{

  The \code{ImportPreparer} and derived classes are used for dispatch
  during data discovery (see \code{\link{newResources}}). There is one
  \code{ImportPreparer} class for each data source for
  \code{\link{AnnotationHubMetadata}}.

  \code{newResources} is a generic function; with methods implemented
  for each \code{ImportPreparer}.

}

\author{Martin Morgan}

\seealso{
  \code{\linkS4class{AnnotationHubMetadata}}.
}

\examples{
getImportPreparerClasses()
}

\keyword{classes}




