\name{AnnotationHubMetadata-class}
\docType{class}
\alias{AnnotationHubMetadata}
\alias{AnnotationHubMetadata-class}
\alias{jsonPath}
\alias{AnnotationHubMetadataFromJson}
\alias{toJson}

\alias{show,AnnotationHubMetadata-method}

\title{Class \code{"AnnotationHubMetadata"} and methods}

\description{

  \code{AnnotationHubMetadata} is used to represent record(s) in the
  server data base.

}

\section{Objects from the Class}{

  Objects can be created by calls to the constructor,
  \code{AnnotationHubMetadata()}.

}

\usage{

AnnotationHubMetadata(AnnotationHubRoot, SourceFile, SourceUrl,
    SourceVersion, DataProvider, Title, Description, Species, Genome,
    Tags, Recipe, RecipeArgs=list(), RDataClass, RDataVersion,
    RDataDateAdded, Maintainer, ..., Coordinate_1_based = TRUE,
    Notes=NA_character_)

jsonPath(x)
toJson(x)
AnnotationHubMetadataFromJson(path, ahroot=NA_character_)
}

\arguments{
    \item{AnnotationHubRoot}{\code{character(1)} Absolute path to
        directory structure containing resources to be added to
        AnnotationHub}

    \item{SourceFile}{\code{character(1)} A path, relative to
        \code{AnnotationHubRoot}, to a resource to be added to
        AnnotationHub.}
    \item{SourceUrl}{\code{character()} URL where resource(s) can be
        found}
    \item{SourceVersion}{\code{character(1)} Version of original file}
    \item{DataProvider}{\code{character(1)} Where did this resource come
        from?}

    \item{Title}{\code{character(1)} Title for this resource}
    \item{Description}{\code{character(1)} Description of the resource}
    \item{Species}{\code{character(1)} Species name}
    \item{Genome}{\code{character(1)} Name of genome build}
    \item{Tags}{\code{character()} Free-form tags}

    \item{Recipe}{\code{character(1)} Name of recipe function}
    \item{RecipeArgs}{\code{list()} Arguments to recipe function}
    \item{RDataClass}{\code{character(1)} Class of derived object
        (e.g. \sQuote{GRanges})}
    \item{RDataVersion}{\code{numeric_version()} Version of resource}
    \item{RDataDateAdded}{\code{POSIXct()} Date added to
        AnnotationHub. Used to determine snapshots.}
    \item{Maintainer}{\code{character(1)} Maintainer name and email
        address, \sQuote{A Maintainer \url{a.maintainer@email.addr}}}

    \item{Coordinate_1_based}{\code{logical(1)} Do coordinates start
        with 1 or 0?}
    \item{Notes}{\code{character()} Notes about the resource.}

    \item{...}{Additional arguments, passed directly to the class
      initialize method.}

    \item{x}{An instance of class \code{AnnotationHubMetadata}.}
    \item{path}{\code{character(1)} full path to a JSON representation
        of \code{AnnotationHubMetadata}-class.}
    \item{ahroot}{A \code{character(1)} representing the value of
        \code{AnnotationHubRoot} to be added to the returned instance.}
}

\value{
  \code{AnnotationHubMetadata} returns an instance of the class.

  \code{jsonPath} returns a \code{character(1))} representation of the
  full path to the location of the \code{json} file associated with this
  record.

  \code{toJson} returns the JSON representation of the record.

  \code{fromJson} retuns an instance of the class, as parsed from the
  JSON file.
}

\author{Dan Tenenbaum}

\seealso{
\code{\linkS4class{AnnotationHubRecipe}}
}
\examples{
getClass("AnnotationHubMetadata")
}

\keyword{classes}
