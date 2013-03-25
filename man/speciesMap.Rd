\name{speciesMap}
\alias{speciesMap}
\docType{data}

\author{
  Marc Carlson
  }

  \title{
Species Mapper object
}
\description{
A data frame containing a mapping between NCBI Taxonomy IDs and species
names, for instance, from "H
}
\usage{data(speciesMap)}
\format{
  A data frame with 1416382 observations on the following 2 variables.
  \describe{
    \item{\code{taxon}}{a numeric vector}
    \item{\code{species}}{a character vector}
  }
}
\examples{
data(speciesMap)
sapply(speciesMap, class)   #       taxon     species 
                            #    "integer" "character" 
subset(speciesMap, species=="Homo sapiens")$taxon # [1] 9606
}
\keyword{datasets}
