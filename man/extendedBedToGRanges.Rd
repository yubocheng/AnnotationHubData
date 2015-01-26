\name{extendedBedToGRanges}
\alias{extendedBedToGRanges}
\title{extendedBedToGRanges}
\description{
Transform an 'extended' BED file (with possibly more than the canonical
three columns) into a Bioconductor GRanges object.
}
\usage{
extendedBedToGRanges(ahm, colClasses)
}
\arguments{
  \item{ahm}{an \code{AnnotationHub} object. }
  \item{colClasses}{ . }
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
