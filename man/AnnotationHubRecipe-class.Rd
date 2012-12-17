\name{AnnotationHubRecipes}
\docType{class}
\alias{AnnotationHubRecipes--class}

% constructor
\alias{AnnotationHubRecipe}

%
\alias{run}
\alias{runWild}

% accessors
\alias{recipeName}
\alias{inputFiles}
\alias{outputFile}
\alias{metadata}

\alias{show}

\title{Build and Run AnnotationHubRecipes}

\description{Constructed with a specific  AnnotationHubMetadata object, instances
  of this class assemble input, output, and recipe in order to transform
  the input into the desired output form.  The input/s are typically
  tab-delimited data files; the output is a standard Bioconductor data
  object, typically a GRanges.

}
\section{Constructor}{
  \code{AnnotationHubRecipes(metadata)}
}
\arguments{

  \item{metadata}{The name of a DCF file}.
  \item{...}{As yet unused}
}


\keyword{classes}
