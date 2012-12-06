setClass("AnnotationHubMetadata",
    representation(
       dcfFile='character',
       title='character',
       description='character',
       type='character',
       url='character',
       dataFile='character',
       recipe='character',
       providerVersion='character',
       biocVersion='character',
       md5='character',
       dateRetrieved='character',
       organismCode='character',
       base.1.coordinate='logical',
       genomeVersion='character',
       notes='character'
    )
)


setGeneric('metadataTitle', signature='object', function(object) standardGeneric('metadataTitle'))
setGeneric('dcfFile', signature='object', function(object) standardGeneric('dcfFile'))
setGeneric('dataFile', signature='object', function(object) standardGeneric ('dataFile'))


setValidity('AnnotationHubMetadata', function(object) {
   msg <- NULL
   dcfFile <- dcfFile(object)
     # the empty constructor is valid, if not very interesting nor useful
   if(is.na(dcfFile))
       return(TRUE)
   if(!file.exists(dcfFile))
       msg <- c(msg, sprintf("cannot read '%s'", dcfFile))
   title <- metadataTitle(object)
   if(nchar(title) == 0)
       msg <- c(msg, sprintf("zero-length 'metadata title is missing"))
   if (!is.null(msg))
       return(msg)
   
   return(TRUE)
})


AnnotationHubMetadata <- function(dcfFile=NA, ...)
{
    x <- new("AnnotationHubMetadata")
    if(is.na(dcfFile)) {
        x@dcfFile = NA_character_
        return(x)
    }
    x@dcfFile <- dcfFile
    dv <- read.dcf(dcfFile)[1,]
    x@title <- dv[['Title']]
    x@dataFile <- dv[['File']]
    x
}

setMethod('dcfFile', 'AnnotationHubMetadata',
    function(object){
        object@dcfFile
      })

setMethod('metadataTitle', 'AnnotationHubMetadata',
    function(object){
        object@title
      })

setMethod('dataFile', 'AnnotationHubMetadata',
    function(object){
        object@dataFile
      })

setMethod('show', 'AnnotationHubMetadata',

    function(object) {
        msg = sprintf('AnnotationHubMetadata')
        cat(msg, '\n', sep='')
        })

