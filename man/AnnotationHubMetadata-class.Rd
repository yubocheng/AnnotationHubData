\name{AnnotationHubMetadata-class}
\docType{class}
\alias{AnnotationHubMetadata}
\alias{AnnotationHubMetadata-class}

\alias{AnnotationHubRoot}
\alias{AnnotationHubRoot<-}
\alias{AnnotationHubRoot,AnnotationHubMetadata-method}
\alias{AnnotationHubRoot<-,AnnotationHubMetadata-method}

\alias{BiocVersion}
\alias{BiocVersion<-}
\alias{BiocVersion,AnnotationHubMetadata-method}
\alias{BiocVersion<-,AnnotationHubMetadata-method}

\alias{Coordinate_1_based}
\alias{Coordinate_1_based<-}
\alias{Coordinate_1_based,AnnotationHubMetadata-method}
\alias{Coordinate_1_based<-,AnnotationHubMetadata-method}

\alias{DataProvider}
\alias{DataProvider<-}
\alias{DataProvider,AnnotationHubMetadata-method}
\alias{DataProvider<-,AnnotationHubMetadata-method}

\alias{Derived.md5}
\alias{Derived.md5<-}
\alias{Derived.md5,AnnotationHubMetadata-method}
\alias{Derived.md5<-,AnnotationHubMetadata-method}

\alias{DerivedLastModifiedDate}
\alias{DerivedLastModifiedDate<-}
\alias{DerivedLastModifiedDate,AnnotationHubMetadata-method}
\alias{DerivedLastModifiedDate<-,AnnotationHubMetadata-method}

\alias{DerivedSize}
\alias{DerivedSize<-}
\alias{DerivedSize,AnnotationHubMetadata-method}
\alias{DerivedSize<-,AnnotationHubMetadata-method}

\alias{Description}
\alias{Description<-}
\alias{Description,AnnotationHubMetadata-method}
\alias{Description<-,AnnotationHubMetadata-method}

\alias{Genome}
\alias{Genome<-}
\alias{Genome,AnnotationHubMetadata-method}
\alias{Genome<-,AnnotationHubMetadata-method}

\alias{Maintainer}
\alias{Maintainer<-}
\alias{Maintainer,AnnotationHubMetadata-method}
\alias{Maintainer<-,AnnotationHubMetadata-method}

\alias{Md5}
\alias{Md5<-}
\alias{Md5,AnnotationHubMetadata-method}
\alias{Md5<-,AnnotationHubMetadata-method}

\alias{Notes}
\alias{Notes<-}
\alias{Notes,AnnotationHubMetadata-method}
\alias{Notes<-,AnnotationHubMetadata-method}

\alias{OriginalFile}
\alias{OriginalFile<-}
\alias{OriginalFile,AnnotationHubMetadata-method}
\alias{OriginalFile<-,AnnotationHubMetadata-method}

\alias{Recipe}
\alias{Recipe<-}
\alias{Recipe,AnnotationHubMetadata-method}
\alias{Recipe<-,AnnotationHubMetadata-method}

\alias{RecipeArgs}
\alias{RecipeArgs<-}
\alias{RecipeArgs,AnnotationHubMetadata-method}
\alias{RecipeArgs<-,AnnotationHubMetadata-method}

\alias{ResourceClass}
\alias{ResourceClass<-}
\alias{ResourceClass,AnnotationHubMetadata-method}
\alias{ResourceClass<-,AnnotationHubMetadata-method}

\alias{ResourcePath}
\alias{ResourcePath<-}
\alias{ResourcePath,AnnotationHubMetadata-method}
\alias{ResourcePath<-,AnnotationHubMetadata-method}

\alias{SourceLastModifiedDate}
\alias{SourceLastModifiedDate<-}
\alias{SourceLastModifiedDate,AnnotationHubMetadata-method}
\alias{SourceLastModifiedDate<-,AnnotationHubMetadata-method}

\alias{SourceSize}
\alias{SourceSize<-}
\alias{SourceSize,AnnotationHubMetadata-method}
\alias{SourceSize<-,AnnotationHubMetadata-method}

\alias{SourceVersion}
\alias{SourceVersion<-}
\alias{SourceVersion,AnnotationHubMetadata-method}
\alias{SourceVersion<-,AnnotationHubMetadata-method}

\alias{Species}
\alias{Species<-}
\alias{Species,AnnotationHubMetadata-method}
\alias{Species<-,AnnotationHubMetadata-method}

\alias{Tags}
\alias{Tags<-}
\alias{Tags,AnnotationHubMetadata-method}
\alias{Tags<-,AnnotationHubMetadata-method}

\alias{TaxonomyId}
\alias{TaxonomyId<-}
\alias{TaxonomyId,AnnotationHubMetadata-method}
\alias{TaxonomyId<-,AnnotationHubMetadata-method}

\alias{Title}
\alias{Title<-}
\alias{Title,AnnotationHubMetadata-method}
\alias{Title<-,AnnotationHubMetadata-method}

\alias{Url}
\alias{Url<-}
\alias{Url,AnnotationHubMetadata-method}
\alias{Url<-,AnnotationHubMetadata-method}

\alias{Version}
\alias{Version<-}
\alias{Version,AnnotationHubMetadata-method}
\alias{Version<-,AnnotationHubMetadata-method}

\alias{constructAnnotationHubMetadataFromOriginalFilePath}
\alias{constructMetadataFromJsonPath}
\alias{postProcessMetadata}


\title{Class \code{"AnnotationHubMetadata"}}
\description{
description will go here
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AnnotationHubMetadata", ...)}.
%%  ~~ describe objects here ~~ 
}
\usage{
  AnnotationHubMetadata(AnnotationHubRoot, OriginalFile, Url, Title,
    Description,
    Species, Genome, Recipe, RecipeArgs=list(), Tags, ResourceClass,
    Version, SourceVersion, Coordinate_1_based, Maintainer,
    DataProvider,
    Notes="")
}

\arguments{
    \item{AnnotationHubRoot}{Absolute path to directory structure containing
        resources to be added to AnnotationHub}
    \item{OriginalFile}{A path, relative to \code{AnnotationHubRoot}, to a
        resource to be added to AnnotationHub.}
    \item{Url}{URL where resource can be found}
    \item{Title}{Title for this resource}
    \item{Description}{Description of the resource}
    \item{Species}{Species name}
    \item{Genome}{Name of genome build}
    \item{Recipe}{Name of recipe function}
    \item{RecipeArgs}{Arguments to recipe function}
    \item{Tags}{Free-form tags}
    \item{ResourceClass}{Class of derived object (e.g. GRanges)}
    \item{Version}{Version of resource}
    \item{SourceVersion}{Version of original file}
    \item{Coordinate_1_based}{Do coordinates start with 1 or 0?}
    \item{Maintainer}{Maintainer name and email address}
    \item{DataProvider}{Where did this resource come from?}
    \item{Notes}{Notes about the resource.}
}



\section{Accessors}{
  \describe{
    \item{}{
      \code{RecipeArgs}, \code{RecipeArgs(x) <- value}:
         Gets or sets the recipe arguments.
       }
    }
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




