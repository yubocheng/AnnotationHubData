\name{readMetadataFromCsv}

\alias{readMetadataFromCsv}
\alias{metadata.csv}

\title{
  Reads a csv file of resource metadata into a data.frame
}

\description{
  Reads the metadata.csv file located in the "inst/extdata" package directory
  into a data.frame. 
}

\usage{
  readMetadataFromCsv(pathToPackage)
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including package name; no trailing slash.
  }
}

\details{
  \itemize{
    \item{readMetadataFromCsv:}{
      Reads the metadata.csv file describing the metadata for resources
      to be added to one of the 'Hubs' (AnnotationHub, ExperimentHub).
      The file should be located in the inst/extdata of the package.
      \code{readMetadataFromCsv} performs checks for required columns
      and data types and can be used by package authors to validate their
      metadata.csv. The function is used internally by
      \code{makeAnnotationHubMetadata}.

      The rows of metadata.csv represent individual \code{Hub}
      resources (i.e., data objects) and the columns are the metadata
      fields. All fields should be a single character string of length 1.

      Required Fields in metadata.csv:
      \itemize{
        \item Title: \code{character}. Name of the resource. This can be
              the exact file name (if self-describing) or a more complete
              description.

        \item Description: \code{character}. Brief description of the resource,
              similar to the 'Description' field in a package DESCRIPTION file.

        \item BiocVersion: \code{character}. The first Bioconductor version
              the resource was made available for. Unless removed from
              the hub, the resource will be available for all versions 
              greater than or equal to this field.

        \item Genome: \code{character}. Genome.

        \item SourceType: \code{character}. Format of original data, e.g., FASTA,
              BAM, BigWig, etc.

        \item SourceUrl: \code{character}. Optional location of original
              data files.

        \item SourceVersion: \code{character}. Version of original data.

        \item Species: \code{character}. Species.

        \item TaxonomyId: \code{character(1)}. Taxonomy ID.

        \item Coordinate_1_based: \code{logical}. TRUE if data are 1-based.

        \item DataProvider: \code{character}. Name of company or institution
              that supplied the original (raw) data.

        \item Maintainer: \code{character}. Maintainer name and email in the
              following format: Maintainer Name <username@address>.

        \item RDataClass: \code{character}. R / Bioconductor class the data
              are stored in.

        \item DispatchClass: \code{character}. Class used to determine how
              to load the data into R. The value for this field should
              be \sQuote{Rda} with the assumption that the data have been 
              serialized with \code{save()} and the filename has the 
              \sQuote{rda} extension. Contact maintainer@bioconductor.org
              if you data do not meet these assumptions and we can determine
              the most appropriate dispatch class.

        \item ResourceName: \code{character}. Exact file name.

        \item Tags: \code{character}. 
              \sQuote{Tags} are search terms used to define a subset of
              resources in a \code{Hub} object, e.g, in a call to \code{query}. 

              For AnnotationHub resources, \sQuote{Tags} are a free-form field
              of search terms defined by the user.  The package name is
              automatically added as one of the \sQuote{Tags} before the
              metadata are finalized.

              For ExperimentHub resources, \sQuote{Tags} are not specified
              by the user but are automatically generated from the 
              \sQuote{biocViews} in the DESCRIPTION. \sQuote{Tags} values 
              supplied by the
              user will be ignored. This approach was taken in order to
              impose more structured vocabulary on the values giving
              the user a defined set of values on which to search. This 
              approach may change in the future.

      }
      The metadata.csv file can have additional columns beyond the 'Required
      Fields' listed above. These values are not added to the Hub
      database but they can be used in package functions to provide an
      additional level of metadata on the resources.
    }
  }
}

\value{
    A data.frame with one row per resource and columns for the Required 
    Fields described above. Additional auto-generated columns, e.g., 
    RDataDateAdded, RDataPath and PreparerClass may also be present and are 
    used by internal functions when generating the final metadata.
}

\seealso{
  \itemize{
    \item \code{\link{makeAnnotationHubMetadata}}
  }
}

\examples{

## Each resource needs a separate row of metadata. This example is for a
## single resource. If you have multiple resources the arguments below
## would be character vectors that produced multiple rows in the data.frame.

meta <- data.frame(
    Title = "RNA-Sequencing dataset from study XYZ",
    Description = paste0("RNA-seq data from study XYZ containing 10 normal ",
                         "and 10 tumor samples represented as a",
                         "SummarizedExperiment"),
    BiocVersion = "3.3",
    Genome = "GRCh38",
    SourceType = "BAM",
    SourceUrl = "http://www.path/to/original/data/file",
    SourceVersion = "Jan 01 2016",
    Species = "Homo sapiens",
    TaxonomyId = 9606,
    Coordinate_1_based = TRUE,
    DataProvider = "GEO",
    Maintainer = "Your Name <youremail@provider.com>",
    RDataClass = "SummarizedExperiment",
    DispatchClass = "Rda",
    ResourceName = "FileName.rda"
)

\dontrun{
## Write the data out as 'metadata.csv' and put in the inst/extdata 
## package directory.
write.csv(meta, file="metadata.csv", row.names=FALSE)

## Test metadata.csv with readMetadataCsv():
readMetadataFromCsv("path/to/mypackage")
}

}

\keyword{methods}
