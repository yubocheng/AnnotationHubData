getImportPreparerClasses <- function()
{
    subclasses <- names(getClassDef("ImportPreparer")@subclasses)
    dont.use <- c("UCSCFullTrackImportPreparer") ## revisit this
    subclasses[!subclasses %in% dont.use]

}

convertVersionToCharacter <- function(lst)
{
    idx <- grep("(RData|Bioc)Version", names(lst))
    lst[idx] <- lapply(lst[idx], as.character)
    lst
}


## returns annotationHubMetadata objs.  So I need to massage res into a list of annotationHugMetadata objects. So I do need all this data. Call AnnotationHubMetadata() constructor in an lapply.
.makeHubMetas <- function(AnnotationHubRoot,
                          BiocVersion,
                          Coordinate_1_based,
                          DataProvider,
                          Description,
                          Genome,
                          Maintainer,
                          RDataClass,
                          RDataDateAdded,
                          RDataPath,
                          RDataVersion,
                          Recipe,
                          RecipeArgs,
                          SourceFile,
                          SourceSize,
                          SourceUrl,
                          SourceVersion,
                          Species,
                          Tags,
                          TaxonomyId,
                          Title){
    ## some casting
    if(!missing(BiocVersion)){
        BiocVersion <- package_version(as.character(unlist(BiocVersion)))}
    if(!missing(Coordinate_1_based)){
        Coordinate_1_based <- as.logical(Coordinate_1_based)}
    if(!missing(RecipeArgs)){
        RecipeArgs <- as.list(RecipeArgs)}
    if(!missing(SourceSize)){
        SourceSize <- as.numeric(unlist(SourceSize))}
    ## massage dates
    RDataDateAdded <- as.POSIXct(as.numeric(RDataDateAdded),
                                 origin = "1969-12-31 17:00:00", tz = "PDT")
    
    AnnotationHubMetadata(AnnotationHubRoot=AnnotationHubRoot,
                          BiocVersion=BiocVersion,
                          Coordinate_1_based=Coordinate_1_based,
                          DataProvider=DataProvider,
                          Description=Description,
                          Genome=Genome,
                          Maintainer=Maintainer,
                          RDataClass=RDataClass,
                          RDataDateAdded=RDataDateAdded,
                          RDataPath=RDataPath,
                          RDataVersion=RDataVersion,
                          Recipe=Recipe,
                          RecipeArgs=RecipeArgs,
                          SourceFile=SourceFile,
                          SourceSize=SourceSize,
                          SourceUrl=SourceUrl,
                          SourceVersion=SourceVersion,
                          Species=Species,
                          Tags=Tags,
                          TaxonomyId=TaxonomyId,
                          Title=Title)
}

## When we have 0s we need to put in NAs.
.addNAForZeros <- function(data, part){
    idx <- as.logical(part)
    res <- rep(NA, length(part))
    if(length(data) ==  table(idx)[["TRUE"]]){
        res[idx] <- data
    }else{
        warning("Bad partitioning info.")
    }
    res
}

## now we need helper to get data out into vectors that make sense.
.makeVecs <- function(l, isMulti){
    if(isMulti){ ## make characterList
        data <- as.list(splitAsList(l[[1]],
                                    f= rep(seq_along(l[[2]]), l[[2]])))
        .addNAForZeros(data, part=l[[2]])
    }else{ ## make character
        if(any(l[[2]]==0)){
            .addNAForZeros(data=l[[1]], part=l[[2]])
        }else{
            as.character(l[[1]])
        }
    }
}


.convertMetadataListToAnnotationHubMetadata <- function(root, lst){
    ## 1st decide if we need character or characterLists
    whichMulti <- unlist(lapply(lst, function(x){max(x[[2]]) > 1}))
    ## sanity checks on data
    lens1 <- unlist(lapply(lst, function(x){length(x[[1]])}))
    if(any(lens1 == 0)){stop("Some of the metadata fields are empty.")}
    lens2 <- unlist(lapply(lst, function(x){length(x[[2]])}))
    if(length(unique(lens2)) > 1){
        stop("All partitions must be the same length.")}
    ## if(any(lens1 < lens2)){
    ##     stop("Some data is missing from the vector to be partitioned.")}

    ## Problem #1 Above is bum test? (freaks out if there are 0's)-
    ## these should not be a real problem for the next step...

    
    ## Problem #2, the recipeArgs have bad partition information.
    
    cols <- mapply(.makeVecs, lst, whichMulti, SIMPLIFY=FALSE)
    numElems <- length(cols[[1]])

    
    ## Fixed: A potentially more serious bug: some of the cols[[9]]
    ## are the wrong length...  They really need to all be the same
    ## length at this point in time (for the mapply). Because if they
    ## are not the same length then they might be "recycled" in a bad
    ## way...
    
    ## Problem #3 RDataDateAdded is getting messed up in the process....
    ## So whatever is upstream of here, has jacked up the RDataDateAdded
    ## To fix ex:
    ## as.POSIXct(1372377600, origin = "1969-12-31 17:00:00", tz = "PDT")
    
    
    ## Note: no values are here for 'Notes' OR for 'SourceMd5'
    mapply(.makeHubMetas,
           AnnotationHubRoot=rep(root, times=numElems),
           BiocVersion=cols[["BiocVersion"]],
           Coordinate_1_based=cols[["Coordinate_1_based"]],
           DataProvider=cols[["DataProvider"]],
           Description=cols[["Description"]],
           Genome=cols[["Genome"]],
           Maintainer=cols[["Maintainer"]],
           RDataClass=cols[["RDataClass"]],
           RDataDateAdded=cols[["RDataDateAdded"]],
           RDataPath=cols[["RDataPath"]],
           RDataVersion=cols[["RDataVersion"]],
           Recipe=cols[["Recipe"]],
           RecipeArgs=cols[["RecipeArgs"]],
           SourceFile=cols[["SourceFile"]],
           SourceSize=cols[["SourceSize"]],
           SourceUrl=cols[["SourceUrl"]],
           SourceVersion=cols[["SourceVersion"]],
           Species=cols[["Species"]],
           Tags=cols[["Tags"]],
           TaxonomyId=cols[["TaxonomyId"]],
           Title=cols[["Title"]],
           SIMPLIFY=FALSE)
    
}


getExistingResources <- function(BiocVersion)
{
    latestSnapshotDate <- AnnotationHubServer::getLatestSnapshotDate(
                                                BiocVersion, returntype="data")
    if (length(latestSnapshotDate))
    {
        res <- query(BiocVersion=BiocVersion,
            RDataDateAdded=latestSnapshotDate, cols="all", returntype="data")
        ## t <- tempfile()
        ## cat(res[2], file=t)
        ## AnnotationHubMetadataFromJson(res)
        root <- "/var/FastRWeb/web" ## find out how we can learn this?
        .convertMetadataListToAnnotationHubMetadata(root, res) 
    } else {
        list()
    }

}

updateAllResources <- function(ahroot, BiocVersion,
    preparerClasses=getImportPreparerClasses(),
    existingResources = getExistingResources(BiocVersion),
    insert=TRUE, downloadIfExists=FALSE, insertOnlyIfRDataExists=TRUE,
    metadataOnly=FALSE, metadataFilter=NULL, parallelCores=NULL)
{

    processAhm <- function(ahm)
    {
        metadata(ahm)$AnnotationHubRoot <- ahroot
        needs.download <- TRUE ## FIXME
        provider <- metadata(ahm)$DataProvider
        if (grepl("http://inparanoid", provider, fixed=TRUE))
        {
            needs.download <- FALSE
        }
        if (needs.download)
        {
            downloadResource(ahm, downloadIfExists)
        }
        needs.recipe <- TRUE ## FIXME
        if (needs.recipe)
        ## FIXME this means we might need to download
        ## but not run recipe. Don't understand how that could happen.
        {
            tryCatch(ahm <- run(ahm), error=function(e){
                    flog(ERROR, "error processing %s: %s",
                        metadata(ahm)$SourceUrl,
                        conditionMessage(e))
                })
            # upload to S3
            #dante
            if (!getOption("AnnotationHub_Use_Disk", FALSE))
            {
                fileToUpload <- file.path(metadata(ahm)$AnnotationHubRoot,
                  metadata(ahm)$RDataPath)
                remotePath <- sub("^/", "", metadata(ahm)$RDataPath)
                res <- upload_to_S3(fileToUpload, remotePath)
                # TODO - if download is successful, delete local file?
            }
            if (insert)
                insertAHM(ahm, insertOnlyIfRDataExists)
        }
    }


    all.ahms <- list()
    for (preparerClass in preparerClasses)
    {
        flog(INFO, "Preparer Class: %s", preparerClass)
        if (exists(preparerClass))
        {
            args <- list()
            if ("annotationHubRoot" %in% names(formals(preparerClass)))
                args$annotationHubRoot <- ahroot
            preparerInstance <- do.call(preparerClass, args)

        } else
            preparerInstance <- do.call(new, list(preparerClass))
        ahms <- newResources(preparerInstance, existingResources)
        all.ahms <- append(all.ahms, ahms)
    }

    if (!metadataOnly)
    {
        if (!is.null(parallelCores))
            mclapply(all.ahms, processAhm, mc.cores=parallelCores)
        else
            lapply(all.ahms, processAhm)
    }


    if(is.function(metadataFilter))
        all.ahms <- Filter(metadataFilter, all.ahms)

    all.ahms
}

doesRecordExist <- function(metadata,
    fields=c("SourceUrl", "BiocVersion"))
{
    if (!all(c("BiocVersion") %in% fields))
        stop("BiocVersion is required in 'fields.'")
    lst <- metadata(metadata)
    template <- lst[fields]
    template <- convertVersionToCharacter(template)
    
    count <- mongo.count(.mongo$getMongo(), .mongo$getNamespace(),
        template)
    ## should it return an error or a warning?
    if (count > 1) warning(sprintf("%s records returned!", count))
    return(count == 1)
}

addNewRecords <- function()
{
    classes <- getImportPreparerClasses()
    for (class in classes)
    {
        ahmlist <- newResources(class, list())
        for (ahm in ahmlist)
        {
            addNewRecord(ahm)
        }
    }
}


## Important NOTE: metadata(ahm)$AnnotationHubRoot must be set correctly
## in order to truly check whether the referenced file already exists.
insertAHM <- function(ahm, insertOnlyIfRDataExists=TRUE,
  avoidDuplicates=TRUE)
{
    ahroot <- metadata(ahm)$AnnotationHubRoot
    metadata(ahm)$AnnotationHubRoot <- NULL
    biocVersion <- as.character(metadata(ahm)$BiocVersion)
    rdataDateAdded <- as.character(AnnotationHubServer::getLatestSnapshotDate(biocVersion, returntype="data"))
    existingRecord <- query(BiocVersion=biocVersion,
      RDataDateAdded=rdataDateAdded,
      SourceUrl=metadata(ahm)$SourceUrl, returntype="data")
    if (avoidDuplicates && (!is.null(existingRecord$SourceUrl$values)))
        return()

    RDataPath <- file.path(ahroot, metadata(ahm)$RDataPath)
    if (getOption("AnnotationHub_Use_Disk", FALSE)) {
      exists <- all(file.exists(RDataPath))

    } else {
        bucketname <- getOption("ANNOTATION_HUB_BUCKET_NAME", "annotationhub")
        exists <- s3Exists(bucketname,
          metadata(ahm)$RDataPath, auth=NA)
        if (is.na(exists)) exists <- FALSE
    }
    if ( (exists && insertOnlyIfRDataExists)  || (!insertOnlyIfRDataExists))
    {
        lst <- convertVersionToCharacter(metadata(ahm))
        ## filter NAs
        nas <- sapply(lst, function(y) is.null(y) ||
                      any(is.na(y)) || length(y) == 0)
        lst <- lst[!nas]
        mongo.insert(.mongo$getMongo(), .mongo$getNamespace(), lst)
    }
}


downloadResource <- function(ahm, downloadIfExists)
{
    with(metadata(ahm), {
        flog(INFO, "in downloadResource(), url is : %s", SourceUrl)
        ## the [1] assumes that all files in this resource
        ## have the same protocol:
        protocol <- tolower(URL_parts(SourceUrl)[,'protocol'])[1]
        filename <- basename(URL_parts(SourceUrl)[,'path'])
        dest0 <- file.path(AnnotationHubRoot, RDataPath)
        destdir <- dirname(dest0)
        destfile <- file.path(destdir, filename)

        if (file.exists(destfile) && (!downloadIfExists))
        {
            flog(INFO, "%s exists, skipping...", destfile)
            return()
        }

        for (dir in unique(destdir))
            if (!file.exists(dir))
                dir.create(dir, recursive=TRUE)
        if (protocol == "rtracklayer")
            return ## recipe will do necessary downloading
        if (protocol == "ftp")
        {
            oldwd <- getwd()
            on.exit(setwd(oldwd))
            setwd(destdir)
            pieces <- detectCores()
            ## FIXME - this leads to oversubscription if
            ## parallelCores is not NULL in updateAllResources
            args <- sprintf("-e 'pget -n %s %s;quit'",
                pieces, SourceUrl)
            tryCatch(ret <- system2("lftp", args),
                error=function(e){
                    flog(ERROR, "Error downloading %s: %s",
                        SourceUrl, conditionMessage(e))
                    })
            if (!getOption("AnnotationHub_Use_Disk", FALSE))
            {
                res <- upload_to_S3(destfile, RDataPath)
                # TODO - if download is successful, delete local file?
            }
        } else if (protocol %in% c("http", "https")){
            ## FIXME be more sophisticated in deciding how to download
            ## (e.g. use parallel download for bigger files)
            tryCatch(download.file(SourceUrl, destfile, quiet=TRUE),
                error=function(e){
                    flog(ERROR, "Error downloading %s: %s",
                        SourceUrl, conditionMessage(e))
                    })
            # if (!getOption("AnnotationHub_Use_Disk", FALSE))
            # {
            #     fileToUpload <- file.path(AnnotationHubRoot, RDataPath)
            #     res <- upload_to_S3(fileToUpload, sub("^/", "", RDataPath))
            #     # TODO - if download is successful, delete local file?
            # }
        }
    })
}

addNewRecord <- function(ahroot, ahm)
{
    url <- metadata(ahm)$SourceUrl
    
    if(doesRecordExist(ahm))
        flog(INFO, "Already in database, skipping. %s", url)
    else {
        #insert record
        flog(INFO, "inserting %s", url)
        # does file exist? if not, download it
        # run recipe
        rdatapath <- metadata(ahm)$RDataPath
        path <- file.path(ahroot, rdatapath)
        if(!any(file.exists(path)))
        {
            flog(INFO, "One or more files does not exist: %s", rdatapath) ## ERROR?
            downloadResource(ahroot, rdatapath)
        }
    }
}
