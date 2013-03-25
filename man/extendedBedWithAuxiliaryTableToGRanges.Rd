\name{extendedBedWithAuxiliaryTableToGRanges}
\alias{extendedBedWithAuxiliaryTableToGRanges}
\title{extendedBedWithAuxiliaryTableToGRanges}
\description{
Transform an 'extended' BED file (with possibly more than the canonical
three columns) into a Bioconductor GRanges object.
}
\usage{
extendedBedWithAuxiliaryTableToGRanges(recipe)
}
\arguments{
  \item{recipe}{a \code{AnnotationHubRecipe} object. }
}

\value{
   "File path of the created RData file."
}
\author{Paul Shannon}

\seealso{
   extendedBedToGRanges,
   ensemblGtfToGRanges,
   AnnotationHubRecipe

}


\keyword{classes}
