\name{extendedBedToGRanges}
\alias{extendedBedToGRanges}
\title{extendedBedToGRanges}
\description{
Transform an 'extended' BED file (with possibly more than the canonical
three columns) into a Bioconductor GRanges object.
}
\usage{
extendedBedToGRanges(recipe)
}
\arguments{
  \item{recipe}{a \code{AnnotationHubRecipe} object. }
}

\value{
File path of the created RData file.
}
\author{Paul Shannon}

\seealso{
   ensemblGtfToGRanges,
   extendedBedWithAuxiliaryTableToGRanges,
   AnnotationHubRecipe
}


\keyword{classes}
