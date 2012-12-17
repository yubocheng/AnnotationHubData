\name{AnnotationHubRecipes-package}
\alias{AnnotationHubRecipes-package}
\alias{AnnotationHubRecipes}

\docType{package}
\title{
Transform public data resources into Bioconductor Data Structures
~~ package title ~~
}
\description{
These recipes convert a wide variety and a growing number of public bioinformatic data sets into easily-used standard Bioconductor data structures.
}
\details{
\tabular{ll}{
Package: \tab AnnotationHubRecipes\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-11-30\cr
License: \tab Artistic-2.0\cr
}
This package provides a set of methods which convert bioinformatic data
resources into standard Bioconductor data types.  For example, a UCSC
genome browser track, expressed as a BED file, is converted into a
GRanges object.  Not every valuable data resource can be transformed
quite so easily; some require more elaborate transformation, and hence a
more specialized recipe.  Every effort is made to limit the number of
recipes required.  One strategy that helps with the principle of "zero
curation":  unless absolutely required, the "cooked" version of the data
resource produced by a recipe is a simple and unembellished reflection
of the original data in its downloaded form.
}
\author{Dan Tenenbaum, Paul Shannon}

\keyword{package}
