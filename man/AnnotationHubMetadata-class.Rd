\name{AnnotationHubMetadata-class}
\docType{class}
\alias{jsonPath}
\alias{AnnotationHubMetadataFromJson}
\alias{toJson}
\alias{constructAnnotationHubMetadataFromSourceFilePath}
\alias{constructMetadataFromJsonPath}
\alias{postProcessMetadata}
\alias{metadata}
\alias{metadata,AnnotationHubMetadata-method}
\alias{metadata,AnnotationHubMetadata,list-method}
\alias{metadata<-}
\alias{metadata<-,AnnotationHubMetadata-method}
\alias{metadata<-,AnnotationHubMetadata,list-method}

\alias{AnnotationHubMetadata}
\alias{AnnotationHubMetadata-class}
\alias{show,AnnotationHubMetadata-method}

\alias{inputFiles}
\alias{inputFiles,AnnotationHubMetadata-method}
\alias{outputFile}
\alias{outputFile,AnnotationHubMetadata-method}
\alias{recipeName}
\alias{recipeName,AnnotationHubMetadata-method}
\alias{run}
\alias{run,AnnotationHubMetadata-method}
\alias{show}
\alias{show,AnnotationHubMetadata-method}
\alias{constructSeqInfo}

\alias{ahmToJson}
\alias{deleteResources}
\alias{getImportPreparerClasses}
\alias{makeAnnotationHubResource}
\alias{tracksToUpdate}



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
AnnotationHubMetadata(AnnotationHubRoot, SourceUrl, SourceType,
                 SourceVersion, SourceLastModifiedDate, SourceMd5 =
                 NA_character_, SourceSize, DataProvider, Title,
                 Description, Species, TaxonomyId, Genome, Tags,
                 Recipe, RDataClass, RDataDateAdded, RDataPath,
                 Maintainer, ..., BiocVersion = biocVersion(),
                 Coordinate_1_based = TRUE, Notes = NA_character_,
                 DispatchClass, Location_Prefix =
                 "http://s3.amazonaws.com/annotationhub/")

jsonPath(x)
toJson(x)
constructAnnotationHubMetadataFromSourceFilePath(ahroot, originalFile)
constructMetadataFromJsonPath(ahroot, jsonpath)
postProcessMetadata(ahm)
constructSeqInfo(species, genome)

inputFiles(object, ...)
outputFile(object)
ahmToJson(ahm)
deleteResources(id)
getImportPreparerClasses()
makeAnnotationHubResource(objName, makeAnnotationHubMetadataFunction,
                          ..., where)
tracksToUpdate()
}

\arguments{
    \item{AnnotationHubRoot}{\code{character(1)} Absolute path to
        directory structure containing resources to be added to
        AnnotationHub}
    \item{SourceUrl}{\code{character()} URL where resource(s) can be
        found}
    \item{SourceType}{\code{character()} which indicates what kind of
    resource was initially processed.  The preference is to name the
    type of resource if it's a single file type and to name where the
    resources came from if it is a compound resource.  So Typical
    answers would be like: 'BED','FASTA' or 'Inparanoid' etc.}
    \item{SourceVersion}{\code{character(1)} Version of original file}
    \item{SourceLastModifiedDate}{\code{POSIXct()} The date when the
    source was last modified.  Leaving this blank should allow the
    values to be retrieved for you (if your sourceURL is valid).}
    \item{SourceMd5}{\code{character()} md5 hash of original file}
    \item{SourceSize}{\code{numeric(1)} Number of bytes in original file}
    \item{DataProvider}{\code{character(1)} Where did this resource come
        from?}
    \item{Title}{\code{character(1)} Title for this resource}
    \item{Description}{\code{character(1)} Description of the resource}
    \item{Species}{\code{character(1)} Species name}
    \item{TaxonomyId}{\code{character(1)}  NCBI code}
    \item{Genome}{\code{character(1)} Name of genome build}
    \item{Tags}{\code{character()} Free-form tags}
    \item{Recipe}{\code{character(1)} Name of recipe function}
    \item{RDataClass}{\code{character(1)} Class of derived object
        (e.g. \sQuote{GRanges})}
    \item{RDataDateAdded}{\code{POSIXct()} Date added to
        AnnotationHub. Used to determine snapshots.}
    \item{RDataPath}{\code{character(1)} file path to serialized form}
    \item{Maintainer}{\code{character(1)} Maintainer name and email
        address, \sQuote{A Maintainer \url{a.maintainer@email.addr}}}
    \item{BiocVersion}{\code{character(1)} Under which resource was built}
    \item{Coordinate_1_based}{\code{logical(1)} Do coordinates start
        with 1 or 0?}
    \item{DispatchClass}{\code{character(1)} string used to indicate
    which code should be called by the client when the resource is
    downloaded.  This is often the same as the RDataClass.  But it is
    allowed to be a different value so that the client can do something
    different internally if required.}
    \item{Location_Prefix}{\code{character(1)} This was added for
    resources where the metadata only is stored and the resource itself
    comes from a third party web site.  The location prefix says the
    base path where the resource is coming from, and the default value
    will be from our own site.}
    \item{Notes}{\code{character()} Notes about the resource.}
  
    \item{ahm}{An instance of class \code{AnnotationHubMetadata}.}
    \item{x}{An instance of class \code{AnnotationHubMetadata}.}
    \item{jsonpath}{\code{character(1)} full path to a JSON representation
        of \code{AnnotationHubMetadata}-class.}
    \item{ahroot}{A \code{character(1)} representing the value of
      \code{AnnotationHubRoot} to be added to the returned instance.}
    \item{originalFile}{A\code{character(1)}}
    
    \item{object}{An AnnotationHubRecipe instance.}
    \item{species}{\code{character(1)} The organism, e.g., "Homo sapiens".}
    \item{genome}{\code{character(1)} The genome build, e.g., "hg19".}

    \item{id}{An id whose DB record is to be fully deleted
      (from gamay - not production)}

    \item{objName}{\code{character(1)} The name of the preparerClass
    object that you intend to have be used for dispatch.  You can call
    it whatever you want as long as you don't use an existing
    preparerClass name}
    \item{makeAnnotationHubMetadataFunction}{\code{function} This is not
    a string, but just the name of the function that makes AHMs out of
    your resource of choice.}

    \item{preparerClasses}{\code{character()} vector of preparerClasses
    (as strings) to try and update.  Normally just one kind.}
    \item{insert}{\code{logical()} insert into the database or no?}
    \item{metadataOnly}{\code{logical()} process only the metadata?  Or
    also run any associated recipe?}
    \item{justRunUnitTest}{\code{logical()} For supporting functions,
    this is a flag that can help to make testing much faster, normally
    FALSE, but when supported, set this to TRUE so that you only process
    the 1st few resources to make sure that recipe works.}
    \item{where}{the environment in which to store the definition.
    Default value is sufficient.}
    \item{\dots}{additional arguments passed to methods}
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

\author{Dan Tenenbaum and Marc Carlson}


\examples{
getClass("AnnotationHubMetadata")
}

\keyword{classes}
