tsvToRefnet <- function(ahm)
{
    inputFile <- inputFiles(ahm)[1]
    outputFile <- outputFile(ahm)

    tbl.orig <- read.table(inputFile, header=TRUE, as.is=TRUE, sep="\t")
    provider <- metadata(ahm)$Title
    tbl <- .refnetTSVtoPSICQUIC(tbl.orig, provider)

    save(tbl, file=outputFile)

    outputFile

} # tsvToRefNetDataFrame
#-------------------------------------------------------------------------------
# outgoing PSICQUIC format
#
# "A"               "B"               "altA"           
# "altB"            "aliasA"          "aliasB"         
# "detectionMethod" "firstAuthor"     "publicationID"  
# "taxonA"          "taxonB"          "type"           
# "sourceDatabases" "interactionID"   "confidenceScore"
# "provider"       
#
.refnetTSVtoPSICQUIC <- function(tbl, provider)
{
      # add some extra columns:
      #   empty: NA
      #   provider: specific refnet source
      #   sourceDatabase: RefNet

    count <- nrow(tbl)    
    tbl <- cbind(tbl, empty=rep("-", count),
                 stringsAsFactors=FALSE)
    tbl <- cbind(tbl, provider=rep(provider, count),
                 stringsAsFactors=FALSE)
    tbl <- cbind(tbl, sourceDatabase=rep("RefNet", count),
                 stringsAsFactors=FALSE)
    
    old.cols <- c("a.common",    "b.common",
                  "a.canonical", "b.canonical",
                  "a.common",    "b.common",
                  "detectionMethod",
                  "empty",          # firstAuthor
                  "pmid",
                  "a.organism",
                  "b.organism",
                  "relation",
                  "sourceDatabase",
                  "empty",          # interaction ID
                  "empty",          # confidenceScore
                  "provider",
                  "a.common",
                  "b.common",
                  "a.canonical",
                  "b.canonical",
                  "cellType",
                  "a.modification",
                  "a.cellularComponent",
                  "b.modification",
                  "b.cellularComponent",
                  "a.canonicalIdType",
                  "b.canonicalIdType",
                  "comment")

    tbl.new <- tbl[, old.cols]

    new.cols  <- c("A", "B",
                   "altA", "altB",
                   "aliasA", "aliasB",
                   "detectionMethod",
                   "firstAuthor",
                   "publicationID",
                   "taxonA", "taxonB",
                   "type",
                   "sourceDatabases",
                   "interactionID",
                   "confidenceScore",
                   "provider",
                   "A.common", "B.common",
                   "A.canonical", "B.canonical",
                   "cellType",
                   "a.modification",
                   "a.cellularComponent",
                   "b.modification",
                   "b.cellularComponent",
                   "a.canonicalIdType",
                   "b.canonicalIdType",
                   "comment")

    colnames(tbl.new) <- new.cols
    tbl.new

} # .refnetTSVtoPSICQUIC
#-------------------------------------------------------------------------------
