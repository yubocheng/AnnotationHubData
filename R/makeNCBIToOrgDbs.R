## This is an example of how this new helper method can make things
## simpler and also provides a test case for how we can parse ensembl
## GTF files into GRanges objects.


## helper to make metadata list from the data
.NCBIMetadataFromUrl <- function(baseUrl, justRunUnitTest) {    
    ## These are the IDs (prescreened) for us to process
    load(system.file('extdata','viableIDs.rda', package='AnnotationForge'))
    ids <- results

    if(justRunUnitTest)
	ids <- head(ids)

    ## need to find an alternative to this...
    ## old school table of tax Ids
    if (!exists("specData")) {
     data(specData, package = "GenomeInfoDb")
    }
    sd <- specData[!is.na(specData[[3]]),]

    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::.lookupSpeciesFromTaxId(id)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- ids[ids %in% sd$tax_id]
    ## AND remove this one bad one that we discovered (an overly
    ## general barley ID)
##    ids <- ids[!(ids %in% '4513')]

## TEMP HACK to avoid a 20 minute wait
## ids <- ids[1:4]
    ## This step takes a minute
    res <- lapply(ids,lookup)
    
    ## get the tax_ids like so (etc.)
    taxonomyId <- as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))  
    ## cleanup of complex names
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)
    ## then we need the full original genus and species etc.
    oriSpecies <- paste(genus, species, sep=" ") 
    fullSpecies <- gsub(" ", "_", oriSpecies)
    ## fullSpecies <- gsub("/", "_", fullSpecies)

    ## get the name for the DB
    title <- paste0("org.",
                    fullSpecies,
                    ".eg",
                    ".sqlite")    
    ## root <- setNames(rep(NA_character_, length(allDirs)), title)
    genome <- setNames(rep("NCBI genomes", length(fullSpecies)), title)
    dateMessage <- paste0('NCBI gene annotations as of ', as.character(date()))
    sourceVersion <- rep(dateMessage, length(fullSpecies))
    description <- paste("NCBI gene ID based annotations about", fullSpecies)
#     sourceUrl <- rep(baseUrl,length(fullSpecies))
    sourceUrls <- c(baseUrl,"ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/idmapping_selected.tab.gz")
    sourceUrl <- rep(list(sourceUrls), length(fullSpecies))
    rDataPath <- paste0("ncbi/uniprot/",title)
    ## return as a list
    list(##annotationHubRoot = root,
        title=title, species = oriSpecies,
        taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
        sourceVersion = sourceVersion,
        description=description, rDataPath=rDataPath)
}


## STEP 1: make function to process metadata into AHMs
## This function will return the AHMs and takes no args.
## It also must specify a recipe function.
makeNCBIToAHMs <- function(currentMetadata, justRunUnitTest=FALSE){
    baseUrl <- 'ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/'
    ## Then make the metadata for these
    meta <- .NCBIMetadataFromUrl(baseUrl, justRunUnitTest)
    ## then make AnnotationHubMetadata objects.
    Map(AnnotationHubMetadata,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        RDataPath=meta$rDataPath,
        MoreArgs=list(
            Coordinate_1_based = TRUE, ## TRUE unless it "needs" to be FALSE
            DataProvider = baseUrl,
            Maintainer = "Marc Carlson <mcarlson@fhcrc.org>",
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/UniProt",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::NCBIToOrgDbsRecipe",
            Tags = c("NCBI", "Gene", "Annotation")))
}


## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
## REMEMBER: inputFiles will be file.path(AnnotationHubRoot,SourceFile)
## (from the AHM)
## and outputFile will be file.path(AnnotationHubRoot,RDataPath)
NCBIToOrgDbsRecipe <- function(ahm){
    require(AnnotationForge)
    ## make use of file.path to put on a trailing slash of the appropriate kind
    ## dbname <- makeInpDb(dir=file.path(inputFiles(ahm, useRoot=FALSE),""),
    ##                     dataDir=tempdir())
    fullSpecies <- ahm@Species
    genus <- unlist(strsplit(fullSpecies,split=" "))[1]
    species <- unlist(strsplit(fullSpecies,split=" "))[2]
    dbname <- makeOrgPackageFromNCBI(version="1.0.0",
                                     maintainer=ahm@Maintainer,
                                     author=ahm@Maintainer,
                                     outputDir=getwd(),
                                     tax_id=as.character(ahm@TaxonomyId),
                                     genus=genus,
                                     species=species,
                                     NCBIFilesDir=getwd(),
                                     databaseOnly=TRUE)
    db <- loadDb(file=dbname)
    saveDb(db, file=outputFile(ahm))
    outputFile(ahm)
}




## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("NCBIImportPreparer",
                          makeNCBIToAHMs)


## First the good news is that I can almost just run this (on 3.2) with no changes.
## Things still TODO: 
##1) change the list (slightly) for which things are supported to be made into OrgDbs - but be conservative since most should still qualify... 2) Write function to make that guess in a smart way for each of the original TaxIDs and sees if there are GO terms or not (to quickly get a 'supported' list.
## Questions: 1) how picky (how many GO terms before we green light it?) 2) Do we want to have both these and the older Blast2GOs hanging around or just these?

## The (now official) answers are: 1) any GO terms if we supported it
## before (or if it's new) and 2) don't mark the older OrgDbs as
## supported in 3.2 (only mark the newer ones).  We will not worry
## about the other NCBI data quality. (as we haven't before and
## someone may honestly want to know that there are only 6 genes for
## their organism).  Also: don't worry about which tax IDs we
## supported before.  I have already pre-computed and determined that
## we mostly overlap them and we can't do much about the previous list
## so we are starting over (basically) for 3.2)




## Overall strategy process the data in NCBI.sqlite and extract
## meaningful taxIDs.  Then save those for use in the recipe above.

## Step 0: Run these functions from a directory where you have already
## built an NCBI.sqlite file.  You can call the example for
## makeOrgPackageFromNCBI()

## First get list of 'core tax IDs' (as before we want to use GO from
## NCBI if that is available and we always want to support those
## organisms). So call .getCoreTaxIds() as before.

## ## STEP 1: 
## ## This helper will just get the taxIDs that we already have GO data for.
## .getCoreTaxIds <- function(NCBIFilesDir=getwd()){
##     ## connect to the DB
##     require(RSQLite)
##     NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
##     dbGetQuery(NCBIcon, "SELECT DISTINCT tax_id FROM gene2go;")[[1]]
## }
## ## coreIDs = .getCoreTaxIds()


## ## Next get the list of other taxIds where there is at least one GO term (we did this before too, but it basically involves a fast query to get the 1st example of a taxID from that new altGO table that we are now populating.

## ## Step 2:
## .getAltTaxIds <- function(NCBIFilesDir=getwd()){
##     require(RSQLite)
##     NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
##     ## Then get the go related Tax Ids
##     sql <- paste0("SELECT distinct NCBItaxon FROM altGO")
##     goTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]
##     ## And also the gene related tax Ids
##     sql <- paste0("SELECT distinct tax_id FROM gene_info")
##     geneTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]
##     intersect(goTaxIds, geneTaxIds)
## }
## ## altTaxIDs = .getAltTaxIds()
## ## The length of the allTaxIDs vector is: 555345!  So I am going to start with just the set that matches the existing tax IDs with a third function:

## .getAllTaxIdsThatOverlapHub <- function(){
##     require(AnnotationHub)
##     ah <- AnnotationHub()
##     ahs <- subset(ah, ah$rdataclass=='OrgDb')
##     ahs <- subset(ahs, ahs$sourcetype=='NCBI/blast2GO')
##     m = mcols(ahs)
##     m$taxonomyid
## }
## ## hubTaxIds <- .getAllTaxIdsThatOverlapHub()

## Then overlap those (I did this before in my testing - find that file).
## Then take these taxIds and run the recipe off of just those... (IOW call that function above in the recipe).  Before this was all way too slow and so we had to pre-compute the list of TaxIDs to process. But now I should be able to do it as I go.  I will also pre-compute them this time simply because it's overall much faster to do it that way when dealing with 1000+ OrgDbs

## allIds <- union(coreIDs, altTaxIDs)
## results <- intersect(hubTaxIds, allIds)
## save(results, file='viableIDs.rda')

################################################################################
## THEN: move this above result to AnnotationHub/inst/extdata/ for safe keeping.
################################################################################



## PROBLEM: about 513 organisms are not unavailable! (mostly microbes).  It looks like these are lost because of changes to the tax IDs

## Here is how I showed they are mostly microbes:

## lost <- hubTaxIds[!hubTaxIds %in% allIds]
## lostNames <- lapply(lost, GenomeInfoDb:::.lookupSpeciesFromTaxId)
## ln <- as.data.frame(matrix(unlist(lostNames), nrow = 513, ncol = 3, byrow = TRUE))
## ln



## Lets do some quick QA to see what the quality of gene id annotation is for the organisms we have taxID data for.

## start with the taxIDs that we could consider (allIds)
## Then query out the taxIDs from the gene_info table (not distinct) to get a sense for the coverage there.


## require(RSQLite)
## NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
## ## Then get the go related Tax Ids
## idStr <- paste(allIds,collapse="','")
## sql <- paste0("SELECT tax_id FROM gene_info WHERE tax_id IN('",idStr,"')")
## giFullTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]

## ## now I just need to quantify the number of each type
## taxidTable <- table(giFullTaxIds)
## head(sort(taxidTable, decreasing=TRUE))


## There are a couple of 'data cliffs':
## barplot(log(sort(taxidTable, decreasing=TRUE)))
## first cliff: (around 7.5)  (~ 1500 genes)
## head(log(sort(taxidTable, decreasing=TRUE)), n=1000)
## barplot(head(log(sort(taxidTable, decreasing=TRUE)), n=1000))
## second cliff: (around 3.75) (~ 40 genes)
## head(log(sort(taxidTable, decreasing=TRUE)), n=4000)
## barplot(head(log(sort(taxidTable, decreasing=TRUE)), n=4000))



## head(sort(taxidTable, decreasing=TRUE), n=1500)



## Drop off happens right around 1031
## As a rule of thumb then, lets drop things that have less than ~1000
## genes annotated.  That would give us 1029 tax ids
## Or I could go with things > 500, but that only gives us 1074 tax ids (not a big gain).


## The other key question though, is: do I lose any of the biomart
## orgs when I cut off at 1000...
## thou = taxidTable[taxidTable>=1000]
## Then get the TaxIds that we hope to fully support...
## ensTaxIds = AnnotationForge:::available.FastaEnsemblSpecies()

## overlap looks promising, but imperfect...
## table(ensTaxIds %in% names(thou))

## What about the other 10?
## lostEns <- ensTaxIds[!names(ensTaxIds) %in% names(thou)]
## Looks like there are not A LOT of annotations for many of those...
## taxidTable[names(taxidTable) %in% names(lostEns)]

## And one of these is bakers yeast.  Which is troubling since it
## doesn't have much in the way of annotations under the 4932 tax id,
## and DOES have a LOT under that 559292.  This all points to the need
## to have a resource for tax IDs that is similar to what we do for GO
## Ids.  And also the fact that there needs to be a way to
## 'generalize' a tax ID when requesting data (or matching up an OrgDb
## with a TxDb etc.).



## TODO: 1) formalize the operations to get the top 1000 most
## annotated genestax ids (from the allIds vector).  2) then save
## those to 'viableIDs.rda' and re-run the recipe.


################################################################################################
################################################################################################
## HERE is how I get the 1000 most 'worthwhile' organisms to put into the hub:

## STEP 1: 
## This helper will just get the taxIDs that we already have GO data for.
.getCoreTaxIds <- function(NCBIFilesDir=getwd()){
    ## connect to the DB
    require(RSQLite)
    NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
    dbGetQuery(NCBIcon, "SELECT DISTINCT tax_id FROM gene2go;")[[1]]
}
## coreIDs = .getCoreTaxIds()


## Next get the list of other taxIds where there is at least one GO term (we did this before too, but it basically involves a fast query to get the 1st example of a taxID from that new altGO table that we are now populating.

## Step 2:
.getAltTaxIds <- function(NCBIFilesDir=getwd()){
    require(RSQLite)
    NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
    ## Then get the go related Tax Ids
    sql <- paste0("SELECT distinct NCBItaxon FROM altGO")
    goTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]
    ## And also the gene related tax Ids
    sql <- paste0("SELECT distinct tax_id FROM gene_info")
    geneTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]
    intersect(goTaxIds, geneTaxIds)
}
## altTaxIDs = .getAltTaxIds()

## Step 3: combine all those taxIds... and remove ones that we already have as packages:
.getPackageOrgDbTaxIds <- function(){
    orgDbs <- AnnotationHubData:::.GetOrgDbs()
    as.integer(unlist(lapply(orgDbs,
                function(x){m <- metadata(x); m[m$name=='TAXID', 2] })))
}
## existingOrgPkgTaxIds <- .getPackageOrgDbTaxIds()
## allIds <- union(coreIDs, altTaxIDs)
## allIds <- allIds[!(allIds %in% existingOrgPkgTaxIds)]

## Step 4: sort the taxIds by the coverage for genes...
.getSortedTaxIds <- function(allIds, NCBIFilesDir=getwd()){
    require(RSQLite)
    NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
    ## ## Then get the go related Tax Ids
    idStr <- paste(allIds,collapse="','")
    sql <- paste0("SELECT tax_id FROM gene_info WHERE tax_id IN('",idStr,"')")
    giFullTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]

    ## now I just need to quantify the number of each type
    taxidTable <- table(giFullTaxIds)
    names(sort(taxidTable, decreasing=TRUE))[1:1000]
}
## results <- .getSortedTaxIds(allIds)
## save(results, file='viableIDs.rda')
## Then copy that file to AnnotationForge/inst/extdata/ (and then reinstall)


## NCBIcon <- dbConnect(SQLite(), dbname = "NCBI.sqlite")
## ## Then get the go related Tax Ids
## idStr <- paste(allIds,collapse="','")
## sql <- paste0("SELECT tax_id FROM gene_info WHERE tax_id IN('",idStr,"')")
## giFullTaxIds <- dbGetQuery(NCBIcon, sql)[[1]]

## ## now I just need to quantify the number of each type
## taxidTable <- table(giFullTaxIds)
## head(sort(taxidTable, decreasing=TRUE))

