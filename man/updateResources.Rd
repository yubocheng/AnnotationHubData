\name{updateResources}
\alias{updateResources}

\alias{runRecipes}
\alias{pushResources}
\alias{pushMetadata}


\title{updateResources}

\description{
  Add new resources to AnnotationHub 
}

\usage{
updateResources <- function(AnnotationHubRoot, BiocVersion = biocVersion(),
                            preparerClasses = getImportPreparerClasses(),
                            metadataOnly = TRUE, insert = FALSE,
                            justRunUnitTest = FALSE)
}

\arguments{
  \item{AnnotationHubRoot}{
    Local path where files will be downloaded.
  }
  \item{BiocVersion}{
    A character vector of Bioconductor versions the resources should be 
    available for.
  }
  \item{preparerClasses}{
    One of the \code{ImportPreparer} subclasses as defined in
    \code{getImportPreparer()}. This class is used for dispatch during
    data discovery.
  }
  \item{metadataOnly}{
    A \code{logical} to specify whether metadata only or both metadata and
    data are processed.

    When TRUE, \code{updateResources} re
    
    
    
    
    that indicates metadata only is generated. When TRUE, metadata
    is generated but no files are downloaded or processed.

    When FALSE, 
            if (!getOption("AnnotationHub_Use_Disk", FALSE)) if this is FALSE
            it goes to S3

## it does not work well for uploading files. Therefore this
## function expects the AWS CLI to be installed. 
## See: https://aws.amazon.com/cli/
## It should be configured with a user who can write to 
## the appropriate bucket. 

TO upload to S3 AWS command line tools must be installed on local machine
(where downloaded files are)



  }
  \item{insert}{
    A logical to control if metadata should be inserted in the AnnotationHub
    db. By default this option is FALSE which is a useful state in which
    to test a new recipe and confirm the metadata fields are correct.
 
    When \code{insert = TRUE}, the option AH_SERVER_POST_URL must
    be set in the global environment or .Rprofile.
  }
  \item{justRunUnitTest}{
    A \code{logical}. When TRUE, a small number of records (usually 5) are
    processed instead of all.
  }
}

\details{

This page documents procedures for adding new records to AnnoatationHub
with the assumption that the type of data, or ImportPreparerClass,
already exits.

for creating a new class please see ***


when is AnnotationHub_Use_Disk set?
 (!getOption("AnnotationHub_Use_Disk", FALSE))

  \code{updateResources} is responsible for creating the metadata and
  downloading and processing data files. Options \code{insert} control
  whether the metadata are inserted in the \code{AnnotationHub} db and
  \code{metadataOnly} controls whether just metadata or both metadata and
  data files are processed.


## Here is the main function and what it is responsible for:
## 1) spawning the AHMs
## 2) making them into JSON
## 3) send metadata off to the back end
## 4) call the recipe and push the results of that off to the right place


}

\value{
  A list of \code{AnnotationHuDataMetadata} objects.
}

\author{Martin Morgan, Marc Carlson}

\seealso{
}

\examples{

\dontrun{

## -----------------------------------------------------------------------
## Inspect metadata:
## -----------------------------------------------------------------------
## When testing a new recipe a useful first step is to generate a small
## amount of metadata for visual inspection. Using the combination of
## 'metadataOnly=TRUE', 'insert=FALSE' and 'justRunUnitTest=TRUE'
## generates metadata for the first 5 records and does not download or 
## process any data files.

meta <- updateResources("home/vobencha/ubuntu/sandbox", 
                        BiocVersion = c("3.2", "3.3"),
                        preparerClasses = "EnsemblFastaImportPreparer",
                        metadataOnly = TRUE, insert = FALSE,
                        justRunUnitTest = TRUE)

INFO [2015-11-12 07:58:05] Preparer Class: EnsemblFastaImportPreparer
Ailuropoda_melanoleuca.ailMel1.cdna.all.fa.gz
Ailuropoda_melanoleuca.ailMel1.dna_rm.toplevel.fa.gz
Ailuropoda_melanoleuca.ailMel1.dna_sm.toplevel.fa.gz
Ailuropoda_melanoleuca.ailMel1.dna.toplevel.fa.gz
Ailuropoda_melanoleuca.ailMel1.ncrna.fa.gz

## The return value is a list of metadata for the first 5 records:

> names(meta)
[1] "FASTA cDNA sequence for Ailuropoda melanoleuca" 
[2] "FASTA DNA sequence for Ailuropoda melanoleuca"  
[3] "FASTA DNA sequence for Ailuropoda melanoleuca"  
[4] "FASTA DNA sequence for Ailuropoda melanoleuca"  
[5] "FASTA ncRNA sequence for Ailuropoda melanoleuca"


## Each record is of class AnnotationHubMetadata:

> class(meta[[1]])
[1] "AnnotationHubMetadata"
attr(,"package")
[1] "AnnotationHubData"
}

## -----------------------------------------------------------------------
## Insert metadata in the db and process/push data files:
## -----------------------------------------------------------------------
## This next code chunk creates the metadata and downloads and processes
## the data. If all files are successfully pushed to their
## final resting place then metadata are inserted in the AnnotationHub db.

meta <- updateResources("local/directory/path", 
                        BiocVersion = c("3.2", "3.3"),
                        preparerClasses = "EnsemblFastaImportPreparer",
                        regex = ".*release-81",
                        metadataOnly = FALSE, insert = TRUE,
                        justRunUnitTest = FALSE)

## -----------------------------------------------------------------------
## Recovery helpers:
## -----------------------------------------------------------------------

## Function that downloads, processes and pushes data to S3 buckets.
## pushResources(meta)

## Insert metadata in the db.
## pushMetadata(meta, url = getOption("AH_SERVER_POST_URL"))
}

\keyword{methods}
