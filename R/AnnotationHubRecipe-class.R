setClass('AnnotationHubRecipe',
    representation(
      metadata='AnnotationHubMetadata'
      )
)

setValidity('AnnotationHubRecipe', function(object) {
   msg <- NULL
   return(TRUE)
})


AnnotationHubRecipe <- function(metadata=NULL)
{
    x <- new("AnnotationHubRecipe")
    if(is.null(metadata)) {
        x@metadata <- AnnotationHubMetadata ()
        return(x)
    }
    x@metadata <- metadata
    x
}

