\name{ensemblGtfToGRanges}
\alias{ensemblGtfToGRanges}
\title{ensemblGtfToGRanges}

\description{

  Transform an Ensembl GTF file to a Bioconductor GRanges object.

}

\usage{
ensemblGtfToGRanges(recipe)
}

\arguments{
  \item{recipe}{An \code{AnnotationHubRecipe} object.}
}

\details{

  The recipe takes the source file as specified in
  \code{metadata(ahmeta)$SourceFile}, using the \code{import} function
  of rtracklayer to transform the GTF to GRanges. The output is stored
  in the \code{metadata(ahmeta)$RDataPath} location.

}

\value{
  File path of the created RData file, \code{metadata(ahmeta)$RDataPath}.
}

\author{Martin Morgan}

\seealso{
   extendedBedWithAuxiliaryTableToGRanges,
   extendedBedToGRanges,
   AnnotationHubRecipe
}


\keyword{classes}
