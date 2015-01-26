\name{rtrackLayerImport}
\alias{rtrackLayerImport}
\title{rtrackLayerImport}

\description{

  Wraps simple calls to rtracklayer::import for ease of use with
  AnnotationHub recipes

}

\usage{
rtrackLayerImport(ahm)
}

\arguments{
  \item{ahm}{an \code{AnnotationHub} object. }
}

\details{

   Saves a GRanges version of the resource specified in the recipe, with
   seqinfo assigned.
}

\value{
   The name of the saved serialized GRanges file.
}

\author{Dan Tenenbaum}

%%\example{
%%   perhaps dan can provide one...
%% }

\keyword{classes}
