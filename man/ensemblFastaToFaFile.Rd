\name{ensemblFastaToFaFile}
\alias{ensemblFastaToFaFile}
\title{ensemblFastaToFaFile}

\description{

  Transform an Ensembl fasta file to a Bioconductor Rsamtools fa file.

}

\usage{
ensemblFastaToFaFile(recipe)
}

\arguments{
  \item{recipe}{An \code{AnnotationHubRecipe} object.}
}

\details{

  The recipe takes the source file as specified in
  \code{metadata(ahmeta)$SourceFile}, indexes the file, and returns the
  path of the index file.

}

\value{
  File path of the created RData file, \code{metadata(ahmeta)$RDataPath}.
}

\author{Martin Morgan}

\seealso{
   ensemblGtfToGRanges,
   extendedBedWithAuxiliaryTableToGRanges,
   extendedBedToGRanges,
   AnnotationHubRecipe
}


\keyword{classes}
