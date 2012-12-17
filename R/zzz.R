## To build the man pages for this package:
## Assuming you are in the Rpacks directory:
## From R, 
## library(roxygen2)
## roxygenize("AnnotationHubData", roclets="rd")
## It's important to specify roclets="rd", otherwise
## roxygen2 will overwrite NAMESPACE and change the DESCRIPTION file.

globalVariables("speciesMap")
