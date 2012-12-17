\name{AnnotationHubRecipe-class}
\Rdversion{1.1}
\docType{class}

\alias{AnnotationHubRecipe-class}

\alias{inputFiles}
\alias{inputFiles,AnnotationHubRecipe-method}

\alias{metadata}
\alias{metadata,AnnotationHubRecipe-method}

\alias{outputFile}
\alias{outputFile,AnnotationHubRecipe-method}

\alias{recipeName}
\alias{recipeName,AnnotationHubRecipe-method}

\alias{run}
\alias{run,AnnotationHubRecipe-method}

\alias{show}
\alias{show,AnnotationHubRecipe-method}


\title{Class \code{"AnnotationHubRecipe"}}
\description{
Objects of this class are constructed with an AnnotationHubMetadata
object, and perform data-driven transformation of input data into
standard Bioconductor data types (e.g., GRanges), in which the data and
recipe specifics are contained in the AnnotationHubMetadata.
}
\usage{
\S4method{runWild}{AnnotationDataRecipe}()
}

\section{Objects from the Class}{
Objects can be created by calls to the class constructor \code{AnnotationHubRecipe(metadata)}.
}
\section{Slots}{
  \describe{
    \item{\code{metadata}:}{Object of class \code{"AnnotationHubMetadata"} ~~ }
    \item{\code{recipeName}:}{Object of class \code{"character"} ~~ }
    \item{\code{inputFiles}:}{A list of class \code{"character"} ~~ }
    \item{\code{outputFile}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{inputFiles}{\code{signature(object = "AnnotationHubRecipe")}: ... }
    \item{md}{\code{signature(object = "AnnotationHubRecipe")}: ... }
    \item{outputFile}{\code{signature(object = "AnnotationHubRecipe")}: ... }
    \item{recipeName}{\code{signature(object = "AnnotationHubRecipe")}: ... }
    \item{run}{\code{signature(object = "AnnotationHubRecipe")}: ... }
    \item{runWild}{\code{signature(object = "AnnotationHubRecipe")}:
      This method evaluates a recipe function which need not be in the
      packae NAMESPACE.}
    \item{show}{\code{signature(object = "AnnotationHubRecipe")}: ... }
	 }
}
\author{
Paul Shannon
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\linkS4class{AnnotationHubMetadata}} 
}
\examples{
showClass("AnnotationHubRecipe")
}
\keyword{classes}


