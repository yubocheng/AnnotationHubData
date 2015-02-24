## Test of new constructor for an updated recipe
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
BiocVersion <- c("3.1")  

##debug(AnnotationHubData:::.generalNewResources)
##debug(AnnotationHubData:::ahmToJson)
## debug(AnnotationHub:::.checkRdataclassIsReal)

ahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "HaemCodeImportPreparer",
  insert = FALSE, metadataOnly=TRUE)

## ahms = updateResources(ahroot, BiocVersion,
##   preparerClasses = "EncodeImportPreparer",
##   insert = FALSE, metadataOnly=TRUE)

## Some bad juju in this one too:
## ahms = updateResources(ahroot, BiocVersion,
##   preparerClasses = "UCSCChainPreparer",
##   insert = FALSE, metadataOnly=TRUE)

## lets try a simple one
## ahms = updateResources(ahroot, BiocVersion,
##   preparerClasses = "UCSCChainPreparer",
##   insert = FALSE, metadataOnly=TRUE)



## TODO: fix problems here:
## Test for updated UCSC tracks recipe
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
BiocVersion <- c("3.1")  
## list the importPreparerClasses I might want: 
getImportPreparerClasses()

## The 1st test. 
UCSCAhms = updateResources(ahroot, BiocVersion,
  preparerClasses = "UCSCTrackImportPreparer",
  insert = FALSE, metadataOnly=TRUE)




###############################################################################
## Test for Martins new recipe
library(AnnotationHubData)
## library(grasp2db) (stashed in AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
BiocVersion <- c("3.1")  
## list the importPreparerClasses I might want: 
getImportPreparerClasses()

## The 1st test. 
ahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "Grasp2ImportPreparer",
  insert = FALSE, metadataOnly=TRUE, filtering=FALSE)


## Then again to push the data to local metadata (he already ran the actual recipe)
ahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "Grasp2ImportPreparer",
  insert = TRUE, metadataOnly=TRUE, filtering=FALSE)


## hooray. The above stuff at least made it in.
## Still TOFO: move the updated recipe back over to the other package


## TODO:
## work out what happened to this title: "Ailuropoda_melanoleuca.ailMel1.78.dna.toplevel.fa" - answer - it was changed to no longer contain the 'version' From now on filters need to check that both the name and the sourceversion are present and identical (not just the name)
## BUT maybe that just isn't the problem *here* since FASTA records (and indeed most FTP sources will include the version number in the sourceURL (as it includes the full path) - still gonna change it though - but this may not be the specific problem I am having here...
## And now I have convinced myself that there really isn't a problem here other than the lack of separate version checking as ~400 is the number of records we have for previous versions (and the number that is expected)




################################################################################
## New test on gamay with the latest (need to update ensembl fasta files
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
BiocVersion <- c("3.1")  
## list the importPreparerClasses I might want: 
potentialClasses <- getImportPreparerClasses()
potentialClasses
## for 1st attempt don't insert OR run recipes...


## The 1st test. (looks OK.  With filter it adds for: 76-78)
fastaAhms = updateResources(ahroot, BiocVersion,
  preparerClasses = "EnsemblFastaImportPreparer",
  insert = FALSE, metadataOnly=TRUE,filtering=FALSE)           
fastaAhms[[1]]
## And this looks 'good'
cat(AnnotationHubData:::ahmToJson(fastaAhms[[1]]))

## debug(AnnotationHubData:::cleanupLst)
## debug(ahmToJson)
## For testing:
# debug(AnnotationHubData:::filterAHMs)
# load('allAhms.rda')
# allAhms <- AnnotationHubData:::filterAHMs(allAhms)

## Test to just see if we can generate the files.
## But still don't insert as Sonali is modifying the DB.
## fastaAhms = updateResources(ahroot, BiocVersion,
##                             preparerClasses = "EnsemblFastaImportPreparer",
##                             insert = FALSE, metadataOnly=FALSE)           


## Then run it this way to get the metadata
fastaAhms = updateResources(ahroot, BiocVersion,
                            preparerClasses = "EnsemblFastaImportPreparer",
                            insert = TRUE, metadataOnly=TRUE)           



## <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">
##<html><body><p>invalid resource: {:rdataversion=&gt;["cannot be empty"]}</p></body></html>
 

## This is the SQL that extracts the stuff we need to clone for spoofing .fai records (plus one other field)
## SELECT  input_sources.sourceversion, rdatapath, rdataclass, rdatasize, rdatamd5, rdatalastmodifieddate, rdatapaths.resource_id FROM rdatapaths, input_sources WHERE input_sources.resource_id=rdatapaths.resource_id AND sourceversion='release-78' AND rdatapath LIKE '%Ailuropoda_melanoleuca.ailMel1.%';

## When refined this should look like this in .sqlite:
## SELECT  input_sources.sourceversion, rdatapath||'.fai', rdataclass, rdatasize, rdatamd5, rdatalastmodifieddate, rdatapaths.resource_id FROM rdatapaths, input_sources WHERE input_sources.resource_id=rdatapaths.resource_id AND sourceversion='release-78';

## and like this for mysql:
## SELECT  input_sources.sourceversion, concat(rdatapath,'.fai'), rdataclass, rdatasize, rdatamd5, rdatalastmodifieddate, rdatapaths.resource_id FROM rdatapaths, input_sources WHERE input_sources.resource_id=rdatapaths.resource_id AND sourceversion='release-78';

## And this query should insert that stuff again

## This is the final command for MySQL
##INSERT INTO rdatapaths (rdatapath, rdataclass, rdatasize, rdatamd5, rdatalastmodifieddate, resource_id) SELECT concat(rdatapath,'.fai'), rdataclass, rdatasize, rdatamd5, rdatalastmodifieddate, rdatapaths.resource_id FROM rdatapaths, input_sources WHERE input_sources.resource_id=rdatapaths.resource_id AND sourceversion='release-78';









## This script is for updating to the latest ensembl data.  Lets start
## with just one or two and then try to update the set once we know
## the recipe works.

## On gamay:
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
## BiocVersion <- c("2.14")  ## is this right?
BiocVersion <- c("2.12", "2.13", "2.14", "3.0", "3.1")  

## list the importPreparerClasses I might want: 
potentialClasses <- getImportPreparerClasses()
potentialClasses


## generate a list of AnnotationHubMetadata objects for existing resources. 
## debug(AnnotationHubData:::.convertMetadataListToAnnotationHubMetadata)
resources <- AnnotationHubServer:::getExistingResources(BiocVersion)
## how many?
length(resources) 


## Code that makes an AHM into json:
ahm=resources[[1]]
cat(ahmToJson(ahm))
## Or just to get the whole thing (as transformed before)
#load('~/proj/Rpacks/AnnotationHubData/inst/scripts/ahmsAsJson.rda')
json = lapply(resources,ahmToJson) 


## Dans new code for pushing JSON to the back end.
library(httr)
h <- handle("http://gamay:9393/new_resource")
res2 <- lapply(json, function(x) {
    result <- POST(handle=h, body=list(payload=x))
    print(result)
    result
})
## AND ThiS (the above), all worked fine.


## Now I need to test on my new 'org' resources (which should not be in the 
## metdata yet)
## Step #1: make a recipe etc. for my org class - DONE (should work)
## Step #2: make a replacement for updateAllResources().
## Step #3: make a repacement for getExistingResources() (for new back
## end and in the new client).  I may have to get this working 1st?


## But rather than running updateAllResources() like before...  
## I need a new function that will 1) spawn the AHM, 2) make it into json 
## and 3) use the function above to send it off to the back end 
## (while calling the recipe).

## For this I will start with a fresh function in AHD.
## Later on I need to replace the getExistingResources() function 
## with something in the client 









################################################################################
## Testing for chainFiles. On gamay:
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
BiocVersion <- c("3.0", "3.1")  
## list the importPreparerClasses I might want: 
potentialClasses <- getImportPreparerClasses()
potentialClasses
## for 1st attempt don't insert OR run recipes...

chainahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "UCSCChainPreparer",
  insert = FALSE, metadataOnly=TRUE,
  filtering=FALSE)           

## debug(AnnotationHubData:::cleanupLst)
## debug(ahmToJson)


chainahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "UCSCChainPreparer",
  insert = TRUE, metadataOnly=TRUE)           



################################################################################
## "UCSC2BitPreparer"
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
BiocVersion <- c("3.1")  
ahms = updateResources(ahroot, BiocVersion,
                        preparerClasses = "UCSC2BitPreparer",
                        insert = FALSE, metadataOnly=TRUE,
                        filtering=FALSE)           






################################################################################
mdinp = updateResources(ahroot, BiocVersion,
  insert = FALSE,                                  ## for 1st attempt.
  preparerClasses = "Inparanoid8ImportPreparer",
  metadataOnly=TRUE)                               ## for 1st attempt.



################################################################################
## Testing for ensembl 77. On gamay:
library(AnnotationHubData)
ahroot <- "~/TEMP/TEST_newResources"
BiocVersion <- c("3.0")  
## list the importPreparerClasses I might want: 
potentialClasses <- getImportPreparerClasses()
potentialClasses
## for 1st attempt don't insert OR run recipes...

# debug(AnnotationHubData:::makeEnsemblFastaToAHMs)
# debug(AnnotationHubData:::.ensemblMetadataFromUrl)
# debug(AnnotationHubData:::.taxonomyId)

faahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "EnsemblFastaImportPreparer",
  insert = FALSE, metadataOnly=TRUE)           

## sub("^([[:alpha:]_]+)\\.(.*)", "\\1", title)




################################################################################
## Testing pushing of AHMs / recipe running to the new backend:

## On gamay:
library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
## BiocVersion <- c("2.14")  ## is this right?
BiocVersion <- c("2.12", "2.13", "2.14", "3.0")  
## list the importPreparerClasses I might want: 
potentialClasses <- getImportPreparerClasses()
potentialClasses
#debug(updateResources)
#debug(AnnotationHubData:::NCBIToOrgDbsRecipe)
# debug(AnnotationHubData:::.runRecipes)

orgahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "NCBIImportPreparer",
  insert = FALSE, ## for 1st attempt don't insert OR run recipes...
  metadataOnly=TRUE)           

## This works now
orgahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "NCBIImportPreparer",
  insert = TRUE, ## for 2nd attempt, try to insert metadata only
  metadataOnly=TRUE)           

orgahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "NCBIImportPreparer",
  insert = TRUE,  ## For 3rd attempt make sure it doesn't double insert.
  metadataOnly=FALSE)
## Some good news is that it appears to NOT do double inserts :)

## And this works too now
orgahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "NCBIImportPreparer",
  insert = FALSE,  ## For 3rd attempt make sure it doesn't double insert.
  metadataOnly=FALSE)

## And so the 'final' run will be like this:
orgahms = updateResources(ahroot, BiocVersion,
  preparerClasses = "NCBIImportPreparer",
  insert = TRUE,  ## For 3rd attempt make sure it doesn't double insert.
  metadataOnly=FALSE)





##max record at start of testing was 12815
## Then 12816 and 12817 were each added...  :)








###########################################################################
## debug(AnnotationHubData:::updateAllResources)
## debug(AnnotationHubData:::.generalNewResources)

## Then try to update resources for inparanoid.
mdinp = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
##  existingResources = resources,
  insert = FALSE,                                  ## for 1st attempt.
  preparerClasses = "Inparanoid8ImportPreparer",
  metadataOnly=TRUE)                               ## for 1st attempt.

## Hack to just only process mdinp[[4]]
## resources <- c(resources, mdinp[c(1:3, 5:7)])

## This will now actually run the recipe on 7 inp8 AHMs.
mdinp = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = TRUE,                                  ## safety is off
  preparerClasses = "Inparanoid8ImportPreparer",
  metadataOnly=FALSE)                               ## upload data too
## It looks like this works as expected, but there is some kind of
## problem with makeInpDb at this time.


## look at the RDataPath generated for these as that will need to be
## the filename.sqlite (final).  And that is where the recipe will
## write the final DB - looks OK except for strange extension: _0.0.1.RData

## New tricks:
## TO debug the the run method for AnnotationHubRecipe:
##  x = selectMethod('run', 'AnnotationHubRecipe')
## debug(x)
##
## And to see the nightly cron command: crontab -l




## Problem: are default args no longer evaluated ahead of time?
## regardless the trouble I have right now is definitely caused by existingResources = getExistingResources(BiocVersion) not getting evaluated when updateAllResources() is called...

## Of course when I really run this, it will take a lot longer (takes time to run the inparanoid script)




## quick test
.hasInp <- function(ahm){
    v = ahm@Recipe
    if(any(grepl("inparanoid8ToTxDbsRecipe",v))){ return(TRUE)
                                              }else{ return(FALSE)}}
.hasFasta <- function(ahm){
    v = ahm@Recipe
    if(any(grepl("ensemblFastaToFaFile",v))){ return(TRUE)
                                              }else{ return(FALSE)}}
foo = unlist(lapply(resources, .hasInp))
bar = unlist(lapply(resources, .hasFasta))

## test for version 3.0
.has3 <- function(ahm){
    v = ahm@BiocVersion
    if(any(grepl("3.0",v))){ return(TRUE)
                         }else{ return(FALSE)}}
threes = unlist(lapply(resources, .has3))




## Then update the resources. 
## This appears to work:
mdgtf = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = FALSE,                                  ## for 1st attempt.
  preparerClasses = "EnsemblGtfImportPreparer",
  metadataOnly=TRUE)                               ## for 1st attempt.


save(mdgtf, file="mdgtf.rda")

## But this one doesn't work...
mdfasta = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = FALSE,                                  ## for 1st attempt.
  preparerClasses = "EnsemblFastaImportPreparer",
  metadataOnly=TRUE)                               ## for 1st attempt.
#mdfasta[1]
save(mdfasta, file="mdfasta.rda") 

length(mdgtf) ## TOO LONG
length(mdfasta) ## ALSO TOO LONG

## So there is a problem (upstream in the call to query) where we are
## filtering out all the dates that are not the right thing (this will
## have to be fixed later).  HOWEVER: I can quickly hack this by
## identifying the AHMs from my grab lists that I should have in the
## existingResources list of AHMs and appending them onto there...
## SO That's what I am doing here:

versions = unlist(lapply(mdfasta, function(x)x@SourceVersion))
unique(versions)  ## raises eyebrow
low <- unlist(lapply(mdfasta, function(x)x@SourceVersion!="release-74"))
table(low) ## mmmHmmmmm
lowFasta <- mdfasta[low]
keepFasta <- mdfasta[!low]
save(keepFasta, file="keepFasta.rda")

low <- unlist(lapply(mdgtf, function(x)x@SourceVersion!="release-74"))
table(low) ## yep
lowGTF <- mdgtf[low]
keepGTF <- mdgtf[!low]
save(keepGTF, file="keepGTF.rda")

## Now (crucially), append the 'low' AHMs to resources
resources2 <- unique(c(resources, lowFasta, lowGTF))
length(resources2)
resources <- resources2

## problem is in .ensemblFastaSourceUrls()
## set up an example url (retrieved from want)
## url = "ftp://ftp.ensembl.org/pub/release-71/fasta/"
## step 3 of .processUrl fails to pattern match

mdgtf2 = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = TRUE,                                    ## for 2nd attempt.
  preparerClasses = "EnsemblGtfImportPreparer",
  metadataOnly = FALSE)                               ## for 2nd attempt.
#save(mdgtf2, file="mdgtf2.rda")

## So far so good 


## But there is a problem here (colons are being included where they should not be)
mdfasta2 = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = TRUE,                                  ## for 1st attempt.
  preparerClasses = "EnsemblFastaImportPreparer",
  metadataOnly = FALSE)                               ## for 1st attempt.
#save(mdfasta2, file="mdfasta2.rda")

## So there is an issue with the 




## Test that I can DL this:
## ftp://ftp.ensembl.org/pub/release-72/fasta/callithrix_jacchus/pep/Callithrix_jacchus.C_jacchus3.2.1.72.pep.all.fa.gz

options(AnnotationHub.debug=TRUE) # print debug messages
options(AnnotationHub.Host="http://gamay") # use gamay as our server
library(AnnotationHub)
ah <- AnnotationHub()
foo = ah$


## And get metadata 

## So to run this every day, I need this script, PLUS I need a way to know that it worked. (there needs to be checks in place along the way. 


## OK so after rsyncing the data over like this:
## rsync -ave ssh --exclude "*.gz" /var/FastRWeb/web/ ubuntu@annotationhub.bioconductor.org:/var/FastRWeb/web 


## Then we have to put the metadata in on the server side.
scp /home/mcarlson/proj/Rpacks/mdgtf2.rda  ubuntu@annotationhub.bioconductor.org:~/tmp
scp /home/mcarlson/proj/Rpacks/mdfasta2.rda  ubuntu@annotationhub.bioconductor.org:~/tmp

## Backup DB 1st
sudo -s
cd /mnt/mongodata/db_backups/
mkdir 2013_11_06
cd  2013_11_06
mongodump -d AnnotationHub -c metadata


## GO to ~/tmp...
cd ~/tmp

## R session
library(AnnotationHubData)
load('mdfasta2.rda')
load('mdgtf2.rda')

## then add records
insertAHMelem <- function(elem){
    metadata(elem)$AnnotationHubRoot <- "/var/FastRWeb/web"
    AnnotationHubData:::insertAHM(elem)
}
## loop to gtf records
lapply(mdgtf2, insertAHMelem)
## loop to fasta records
lapply(mdfasta2, insertAHMelem)


## This insert function is a wee bit slow.
## test the inserts
## at the mongo command line:
mongo AnnotationHub
db.metadata.count()

## IF you need to use authorization use this command:
db.auth("AnnotationHubUser", "f1ftyl0quat3")

## And change permissions for all new files that are rsynced
sudo chmod -R a+rwx /var/FastRWeb/web



##############################################################################
## More work to just add new Haemcode resources.
##

library(AnnotationHubData)
ahroot <- "/var/FastRWeb/web"
## BiocVersion <- c("2.14")  ## is this right?
BiocVersion <- c("2.14")  

## list the importPreparerClasses I might want: 
potentialClasses <- AnnotationHubData:::getImportPreparerClasses()
potentialClasses

## generate a list of AnnotationHubMetadata objects for existing resources. 
resources <- AnnotationHubData:::getExistingResources(BiocVersion)
length(resources)


## now test run to get metadata for just the HaemCode stuff
mdhaem = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = FALSE,                                  ## for 1st attempt.
  preparerClasses = "HaemCodeImportPreparer",
  metadataOnly=TRUE)                               ## for 1st attempt.
## save(mdhaem, file="mdhaem.rda")


mdhaem = AnnotationHubData:::updateAllResources(ahroot, BiocVersion,
  existingResources = resources,
  insert = TRUE,                                  ## for 2nd attempt.
  preparerClasses = "HaemCodeImportPreparer",
  metadataOnly=FALSE)                               ## for 2nd attempt.



## TEMP instructions for pushing to local disc, in future we will have
## a staging bucket in S3 for testing instead.

## It really doesn't look like our code is truly "skipping"
## pre-processed results...  Looks like it is processing them, and
## then skipping at the copy step...


## We must always chance the mode (from /var/FastRWeb)
sudo chmod -R a+rwx /var/FastRWeb/web

## Also change owner:
sudo chown -R www-data:www-data /var/FastRWeb/web

## AND make sure server is doing the right thing (if testing on gamay)
## Before starting the server on gamay you need to set the environment variable
ANNOTATIONHUB_USE_DISK=true

## THEN do this as root: (sudo -s to become root)
cd /var/FastRWeb/code
killall -INT Rserve
ANNOTATIONHUB_USE_DISK=true ./start
## then stop being root (safety!)
exit
## This process tells the server to look on its disk for the files. If
## this is not set, the server will look in S3 for the files.


## Also: control the test condition when you launch AnnotationHub (from gamay)
options(AnnotationHub.Host = "http://gamay")
options(AnnotationHub.debug=TRUE)
options(AnnotationHub_Use_Disk=TRUE)
library(AnnotationHub)
ah <- AnnotationHub()
## Tested example (confirmed that it exists in dir...)
foo <- ah$blood.Peaks.mm10.GSM730632_Runx1.bed_0.0.1.RData

## Older example also works:
res <- ah$goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData


###############################################################################
## OK: now I need to push just the metadata (dan is pushing the files to S3)

## 1st copy files (from gamay)
scp /home/mcarlson/proj/Rpacks/AnnotationHubData/inst/scripts/mdhaem.rda  ubuntu@annotationhub.bioconductor.org:~/tmp
## Then go there (from shrew)
ssh ubuntu@annotationhub.bioconductor.org

## Then backup DB 1st
sudo -s
cd /mnt/mongodata/db_backups/
mkdir 2013_11_06
cd  2013_11_06
mongodump -d AnnotationHub -c metadata


## GO to ~/tmp...
cd ~/tmp

## R session
library(AnnotationHubData)
load("mdhaem.rda")
#elem = mdhaem[[10]]
#mdhaem = mdhaem[-1]

## then add records
insertAHMelem <- function(elem){
    metadata(elem)$AnnotationHubRoot <- "/var/FastRWeb/web"
    message(metadata(elem)$SourceUrl)
    AnnotationHubData:::insertAHM(elem)
}
## loop to gtf records
lapply(mdhaem, insertAHMelem)


## This insert function is a wee bit slow.
## test the inserts
## at the mongo command line:
mongo AnnotationHub
db.metadata.count()

## IF you need to use authorization use this command:
db.auth("AnnotationHubUser", "f1ftyl0quat3")




########################################################
## how to remove and coung OLDER metadata entries:
########################################################
## At the command line
mongo AnnotationHub ## launches the mongo client
## now (test) we use javascript commands for mongo
db.metadata.find({RDataPath: /^blood/})
## now count
db.metadata.count({RDataPath: /^blood/})
## now delete records
db.metadata.remove({RDataPath: /^blood/})
## now count again
db.metadata.count({RDataPath: /^blood/})
## now leave
exit




###############################################
## A quick modification to change the metadata objects.

changeAHMelem <- function(elem){
    metadata(elem)$RDataPath <- paste0('haemcode/',metadata(elem)$RDataPath)
    metadata(elem)$SourceFile <- paste0('haemcode/',metadata(elem)$SourceFile)
    elem
}
## loop to gtf records
mdhaem <-  lapply(oldmdhaem, changeAHMelem)
## save new result
save(mdhaem, file="mdhaem.rda")




















###############################################################################
## Dans comments on S3 syncing:

## OK, so there is a utility in gamay in ~/s3sync called s3sync.rb, you can get basic help like this:

## $ s3sync/s3sync.rb 
## Need a source and a destination
## s3sync.rb [options] <source> <destination>		version 1.2.6
##   --help    -h          --verbose     -v     --dryrun    -n	
##   --ssl     -s          --recursive   -r     --delete
##   --public-read -p      --expires="<exp>"    --cache-control="<cc>"
##   --exclude="<regexp>"  --progress           --debug   -d
##   --make-dirs           --no-md5
## One of <source> or <destination> must be of S3 format, the other a local path.
## Reminders:
## * An S3 formatted item with bucket 'mybucket' and prefix 'mypre' looks like:
##     mybucket:mypre/some/key/name
## * Local paths should always use forward slashes '/' even on Windows
## * Whether you use a trailing slash on the source path makes a difference.

## So to sync from /var/FastRWeb/web to the annotationhub bucket you would do this:

## ~/s3sync/s3sync.rb -n -p -r --verbose --progress web/ annotationhub:


## The -n means "dry run" so it won't actually do anything, but if it seems to be doing the right thing, you can remove the -n. 

## I've already started running this for the haemcode stuff but this will be handy in the future.

## Dan



## BTW, this assumes you are in /var/FastRWeb, but you could change it to:

## ~/s3sync/s3sync.rb -n -p -r --verbose --progress /var/FastRWeb/web/ annotationhub:

## and run it from anywhere.
## Dan

######################
## To copy things up to the S3 buckets: 
## You can see help like this
## aws s3 help

## You can get copying help like this
## aws s3 cp help

## And you can copy something to be read only using the --acl argument like this:
## aws s3 cp --acl public-read hello.txt s3://annotationhub/foo/bar/hello.txt

## To sign in at the AWS console go here: (use your username and password)
## https://bioconductor.signin.aws.amazon.com/console
## then click 'S3'
## (account is bioconductor)

## https://console.aws.amazon.com/s3/home?region=us-west-2

## And to recursively copy back down from the S3 bucket do like this:
## aws s3 cp --dryrun --recursive s3://annotationhub/ensembl/release-75/fasta/ .

## And then to copy back up you can do this:
## aws s3 cp --dryrun --recursive --acl public-read ./fasta/ s3://annotationhub/ensembl/release-75/fasta/

## And actually to copy back I should have uses 'aws s3 sync' instead of
## 'aws s3 cp'...


## uncovered a problem when making the .fai files (with macaca mulata dna sm sample)


######################################################################
## To spawn up the sqlite file from the mySQL one:
## there is a chron job that does this for production
## but for gamay you have to run it manually.  You can check out the
## script that does it from github and then run it.



#############################################################
## Instructions for connecting to the back end DB directly:
## on gamay just connect to the DB like this:
## mysql -p -u ahuser annotationhub
## tickytacky
##
## And for production just log in FIRST
## ssh ubuntu@annotationhub.bioconductor.org # it has your key
## And proceed as above.

