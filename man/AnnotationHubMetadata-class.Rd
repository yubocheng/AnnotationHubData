\name{AnnotationHubMetadata-class}
\docType{class}
\alias{AnnotationHubMetadata}
\alias{AnnotationHubMetadata-class}


\title{Class \code{"AnnotationHubMetadata"}}
\description{
description will go here
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AnnotationHubMetadata", ...)}.
%%  ~~ describe objects here ~~ 
}
\usage{
  AnnotationHubMetadata(AnnotationHubRoot, SourceFile, SourceUrl, Title,
    Description,
    Species, Genome, Recipe, RecipeArgs=list(), Tags, RDataClass,
    RDataVersion, SourceVersion, Coordinate_1_based, Maintainer,
    DataProvider,
    Notes="")
}

\arguments{
    \item{AnnotationHubRoot}{Absolute path to directory structure containing
        resources to be added to AnnotationHub}
    \item{SourceFile}{A path, relative to \code{AnnotationHubRoot}, to a
        resource to be added to AnnotationHub.}
    \item{SourceUrl}{URL where resource can be found}
    \item{Title}{Title for this resource}
    \item{Description}{Description of the resource}
    \item{Species}{Species name}
    \item{Genome}{Name of genome build}
    \item{Recipe}{Name of recipe function}
    \item{RecipeArgs}{Arguments to recipe function}
    \item{Tags}{Free-form tags}
    \item{RDataClass}{Class of derived object (e.g. GRanges)}
    \item{Version}{Version of resource}
    \item{SourceVersion}{Version of original file}
    \item{Coordinate_1_based}{Do coordinates start with 1 or 0?}
    \item{Maintainer}{Maintainer name and email address}
    \item{DataProvider}{Where did this resource come from?}
    \item{Notes}{Notes about the resource.}
}






\author{
Dan Tenenbaum
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{AnnotationHubRecipe}}
}
\examples{
showClass("AnnotationHubMetadata")
}
\keyword{classes}




