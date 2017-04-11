\name{makeStandardOrgDbs}

\alias{makeStandardOrgDbsToAHM}
\alias{makeStandardTxDbsToAHM}
\alias{makeNCBIToOrgDbsToAHM}

\title{Functions to add OrgDb and TxDb sqlite files to AnnotationHub}

\description{Add OrgDb and TxDb sqlite files to AnnotationHub}

\usage{
makeStandardOrgDbsToAHM(currentMetadata, justRunUnitTest = FALSE, 
                        BiocVersion = biocVersion(), 
                        downloadOrgDbs = TRUE)

makeStandardTxDbsToAHM(currentMetadata, justRunUnitTest = FALSE, 
                       BiocVersion = biocVersion(), TxDbs)

makeNCBIToOrgDbsToAHM(currentMetadata, justRunUnitTest = FALSE,
                      BiocVersion = biocVersion(), 
                      baseUrl = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/")
}

\arguments{
  \item{currentMetadata}{
    Currently not used. Intended to be a list of metadata to filter, i.e.,
    records that do not need to be processed again. Need to remove or fix.
  }
  \item{baseUrl}{
    file location.
  } 
  \item{justRunUnitTest}{
    A \code{logical}. When TRUE, a small number of records (usually 5) are
    processed instead of all.
  }
  \item{BiocVersion}{
    A \code{character} vector of Bioconductor versions the resources should be 
    available for.
  }
  \item{TxDbs}{
    Character vector of the \code{TxDb}s to build and push. This should
    include \code{TxDbs} that were new or updated for the current release.
  }
  \item{downloadOrgDbs}{
    Logical. Indicates if all \code{OrgDb} packages in the Bioconductor
    repo should be downloaded and installed. This should be \code{TRUE}
    the first time the recipe is run and can be \code{FALSE} for
    subsequent runs.
  }
}

\details{
  \code{makeStandardOrgDbsToAHM} and \code{makeStandardTxDbsToAHM} extracts
  the sqlite files from the existing \code{OrgDb} and \code{TxDb} packages
  in the Bioconductor repositories and generate associated metadata.

  \code{makeNCBIToOrgDbsToAHM} creates sqlite files and metadata for 1000
  organisms with the \code{makeOrgPackageFromNCBI} function. These
  organisms are less 'main stream' than those hosted in the Bioconductor
  repository (\code{makeStandardOrgDbsToAHM}) and the databases are less 
  comprehensive because data only come from one source, NCBI.
}

\value{
  List of \code{AnnotationHubMetadata} objects. 
}

\author{Bioconductor Core Team}

\seealso{
  \itemize{
    \item \link{updateResources}
    \item \link{AnnotationHubMetadata}
  }
}

\examples{
\dontrun{
## In Bioconductor 3.5, one new TxDb was added and 4 active
## tracks were updated. This piece of code shows how to add these 5
## packages to AnnotationHub.

## Step I: low-level inspection
##
## Generate the metadata for inspection.
TxDbs <- c("TxDb.Ggallus.UCSC.galGal5.refGene",
           "TxDb.Celegans.UCSC.ce11.refGene",
           "TxDb.Rnorvegicus.UCSC.rn5.refGene",
           "TxDb.Dmelanogaster.UCSC.dm6.ensGene",
           "TxDb.Rnorvegicus.UCSC.rn6.refGene")
meta <- makeStandardTxDbsToAHM(currentMetadata=list(AnnotationHubRoot=getwd()), 
                               justRunUnitTest = FALSE, 
                               TxDbs = TxDbs)

## Step II: build metadata with updateResources()
##
## Generate the metadata using updateResources(). The first time through
## use metadataOnly=TRUE and insert=FALSE. This will create the list of
## metadata and confirm the 'TxDbs' argument is properly interpreted.
meta <- updateResources(getwd(),
                        preparerClasses = "TxDbFromPkgsImportPreparer",
                        metadataOnly = TRUE, insert = FALSE,
                        justRunUnitTest = FALSE, TxDbs = TxDbs)

INFO [2017-04-11 09:12:09] Preparer Class: TxDbFromPkgsImportPreparer
complete!
> length(meta)
[1] 5

## Step III: build metadata and run recipe with updateResources() 
##
## metadataOnly=FALSE will force the 'recipe' to run; in this case
## the recipe involves extracting sqlite files from the packages
## and pushing them up to an S3 bucket. Set insert=TRUE to insert
## metadata into the AnnotationHub database. Insertions are generally done with
## the AnnotationHub Docker.

meta <- updateResources(getwd(),
                        BiocVersion = "3.5",
                        preparerClasses = "TxDbFromPkgsImportPreparer",
                        metadataOnly = FALSE, insert = FALSE,
                        justRunUnitTest = FALSE, TxDbs = TxDbs)
}
}

\keyword{methods}
