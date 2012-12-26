\name{AnnotationHubRecipe-class}
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

\alias{AnnotationHubRecipe}
\alias{constructSeqInfo}

\title{Class \code{"AnnotationHubRecipe"}}

\description{
Instances of this class perform data-driven transformation of public annotation data into
standard Bioconductor data types (e.g., GRanges).  Each is constructed from an AnnotationHubMetadata
object, which specifies where to find, how to transform, and where to write the data.
}

\usage{
AnnotationHubRecipe(metadata)
constructSeqInfo(species, genome)
inputFiles(object)
outputFile(object)
}

\arguments{
  \item{object}{An AnnotationHubRecipe instance.}
  \item{metadata}{An instance of the \code{AnnotationHubMetadata} class.}
  \item{species}{The organism, e.g., "Homo sapiens".}
  \item{genome}{The genome build, e.g., "hg19".}
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
    \item{show}{\code{signature(object = "AnnotationHubRecipe")}: ... }
	 }
}
\author{
Paul Shannon
}

\seealso{
   \code{\linkS4class{AnnotationHubMetadata}} 
}

\examples{
jsonFile <- "wgEncodeRikenCageCd20CellPapTssHmm.bedRnaElements_0.0.1.json"
resourcePath <- 'goldenpath/hg19/encodeDCC/wgEncodeRikenCage'
jsonPath <- file.path(resourcePath, jsonFile)

sourceDirectory <- system.file('extdata', package='AnnotationHubData')
workingDirectory <- AnnotationHubData:::.createWorkingDirectory(sourceDirectory)
annotationHubRoot <- workingDirectory

md <- constructMetadataFromJsonPath(annotationHubRoot, jsonPath)
recipe <- AnnotationHubRecipe(md)

# once developed and added to the package (and thus into the package
# NAMESPACE), a specific recipe is identified and run by its name (a
# character string) being specified in the recipe, usually through
# being the value of that field in the JSON metadata.
# But for demonstration purposes here, we show how to define an ad
# hoc function just as one might do when developing a recipe function.
# Note how this ad hoc function is passed -- as a function object --
# to the run method, in the optional second argument to the run
# method.

# a simple function which calculates the length of the first input
# file's name; saves that result in an RData file (as specified by
# the metadata used to construct the AnnotationHubRecipe object,
# above), then returns that value.  More typically, a recipe function
# returns the full path of the transformed output file it has just
# written and save to disk.

adhoc <- function(recipe) {
    result <- nchar(inputFiles(recipe)[1])
    save(result, file=outputFile(recipe))
    result
    }

run(recipe, adhoc)
}


\keyword{classes}


