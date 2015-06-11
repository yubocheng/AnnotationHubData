## good = 'AH47109'
## bad = 'AH19920'



select ah_id, id, title from resources where ah_id='AH47109' limit 4;
select * from rdatapaths where resource_id='47109';

select ah_id, id, title from resources where ah_id='AH19920' limit 4;
select * from rdatapaths where resource_id='19920';

## in this case I *only* need to add missing records to rdatapaths for
## those records where the release was between version 69-75 and the
## type was FaFile.  Basically all I need to fix this is the
## rdatapaths table already...


## This will get the records that I want (for release 70)
select * from rdatapaths where rdataclass ='FaFile' and rdatapath LIKE 'ensembl/release-70%';

## just get how many (why so few on recent releases?) compare:
select count(*) from rdatapaths where rdataclass ='FaFile' and rdatapath LIKE 'ensembl/release-70%';

## vs
select count(*) from rdatapaths where rdataclass ='FaFile' and rdatapath LIKE 'ensembl/release-80%';

## VS THIS:
SELECT count(*) FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fai';
SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fai' limit 4;


## This is a problem because it suggests that the earlier records (69-75) were doubled somehow..

## So to really fix this I want to just delete records that are 'too new' and that match the criteria for an affected release.

## AND you can see where the break happens here (for example):
SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz';

## So this query should only select the offending records
SELECT rdatapath,rdataclass,resource_id  FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' AND resource_id > 521;
## Should start with 12599 and be half of '70' records) ## 577 records


## Just how many times have these things been inserted?
SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdatapath LIKE 'ensembl/release-70/fasta/ochotona_princeps/dna/Ochotona_princeps.pika.70.dna.toplevel.fa.rz%';


## Looks like one extra insert was made after the initial one... (an extra *resource* was added though)
## resource IDs for this example are: 367 AND 20133
## :/
## SO 
SELECT * FROM resources WHERE id = 20133;
## AND a few other tables that are connected to resource will also have junk in them.
SELECT * FROM biocversions WHERE resource_id = 20133;
## AND
SELECT * FROM input_sources WHERE resource_id = 20133;
## AND
SELECT * FROM tags WHERE resource_id = 20133;

###############################################################################
## BUT right now ON DELETE CASCADE is not set up for the MYSQL tables.
## So I have to fix this for all the tables that are child-tables of
## the resources table (which is the table with the primary key)

## To alter all the child tables to support (ON DELETE CASCADE):
## Step 1: learn the name of the foreign key:
SHOW CREATE TABLE tags;

## Step 2: remove foreign key
ALTER TABLE tags DROP FOREIGN KEY tags_ibfk_1;

## Step 3: replace foreign constraint
ALTER TABLE tags ADD FOREIGN KEY(resource_id) REFERENCES resources(id) ON DELETE CASCADE; 

## Step 4: verify 
SHOW CREATE TABLE tags;
## Then this was repeated for: rdatapaths, biocversions, input_sources.
## (I then saved a gamay dump.)




## So for this to ALL go away I should only have to do this:
DELETE FROM resources WHERE id = 20133;
## AND IT works.  :)



## SO here are my (proposed) two steps:

## Step ONE: find the 'break' after sorting by resource_id (for a release)
#SELECT rdatapath, rdataclass, resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%' ORDER BY resource_id;

## Shows the break is at >521 ?!???

## Step TWO: delete offending records using ON DELETE CASCADE like so
#DELETE FROM resources WHERE id IN (SELECT resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%' AND resource_id >521);


## BUT BEWARE/NOT SO FAST!: finding the break can be 'tricky'.  See panda for example

SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdatapath LIKE 'ensembl/release-70/fasta/ailuropoda_melanoleuca/cdna/Ailuropoda_melanoleuca.ailMel1.70.cdna.all.fa.rz%';


## So I actually need a better query than just ">X"...  I actually want to group by the rdatapath (minus .fai) and then I want to return the max number for each...  (since that is the number I will want to delete)


## 1st lets trim any .fai's from our rdatapath
#SELECT trim(TRAILING '.fai' FROM rdatapath) ,rdataclass,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%' group by trim(TRAILING '.fai' FROM rdatapath) limit 20;




## Actually forget the .fai's as they are not duplicated. All we want is to get rid of the extra (highest resource_id) .fa.rz's
SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' order by rdatapath ;




## This is my solution (at small scale)
SELECT *, max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' order by rdatapath limit 10) AS foo GROUP BY rdatapath;



## So to actually do this we would then do like this:
DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' order by rdatapath limit 10) AS foo GROUP BY rdatapath);


## And to REALLY actually do it we take off the training wheels like this (for release-70):
DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);



###############################################################################################
## Doing the actual record deletes:
## two good things to check (before and after each delete)

## Look at all .fa files for 70:
SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' order by resource_id ;

## Ochotona princeps status (first?)
SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdatapath LIKE 'ensembl/release-70/fasta/ochotona_princeps/dna/Ochotona_princeps.pika.70.dna.toplevel.fa.rz%';

## panda status (other first?)
SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdatapath LIKE 'ensembl/release-70/fasta/ailuropoda_melanoleuca/cdna/Ailuropoda_melanoleuca.ailMel1.70.cdna.all.fa.rz%';

## and Xiphophorus maculatus status (last?)
SELECT rdatapath,rdataclass,resource_id FROM rdatapaths WHERE rdatapath LIKE 'ensembl/release-70/fasta/xiphophorus_maculatus/pep/Xiphophorus_maculatus.Xipmac4.4.2.70.pep.all.fa.rz%';


## THEN we need to roll back and just do the same as above for EACH of the other releases.
## BUT BE CAREFUL! These queries can be run only one time each! (since they delete the most recent records)
## After running these I commented them (for safety)

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-69%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-70%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-71%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-72%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-73%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-74%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);

## DELETE FROM resources WHERE id IN (SELECT max(resource_id) AS bad_ids FROM (SELECT rdatapath,resource_id FROM rdatapaths WHERE rdataclass ='FaFile' AND rdatapath LIKE 'ensembl/release-75%.fa.rz' order by rdatapath) AS foo GROUP BY rdatapath);









#############################################################################
## Also needed: have to bump version number for recent Fasta 79 and 80. (to also allow 3.1)




## need to update these records:

SELECT * FROM rdatapaths WHERE rdataclass='FaFile' AND rdatapath LIKE 'ensembl/release-79/fasta/%' limit 4;


## I want to do something like this:
INSERT INTO biocversions (biocversion, resource_id)
SELECT DISTINCT '3.1' AS biocversion, resource_id FROM rdatapaths WHERE rdataclass='FaFile' AND rdatapath LIKE 'ensembl/release-79/fasta/%';

## And for release 80
INSERT INTO biocversions (biocversion, resource_id)
SELECT DISTINCT '3.1' AS biocversion, resource_id FROM rdatapaths WHERE rdataclass='FaFile' AND rdatapath LIKE 'ensembl/release-80/fasta/%';


## Test some
SELECT * FROM rdatapaths WHERE rdataclass='FaFile' AND rdatapath LIKE 'ensembl/release-79/fasta/%' limit 4;

select * from biocversions where resource_id='47523' limit 4;
select * from biocversions where resource_id='47524' limit 4;

select * from biocversions where resource_id='47109' limit 4;
select * from biocversions where resource_id='47110' limit 4;




## Need to also update new OrgDbs (the new package ones) to support '3.1' (for course)

## I want to change for these records here
SELECT * FROM rdatapaths WHERE rdataclass='OrgDb' AND resource_id > 46983;

## SO:
INSERT INTO biocversions (biocversion, resource_id)
SELECT DISTINCT '3.1' AS biocversion, resource_id FROM rdatapaths WHERE rdataclass='OrgDb' AND resource_id > 46982;


## Then test some

## older 
select * from biocversions where resource_id=13962;

## newer (should have two now)
select * from biocversions where resource_id=46983;
select * from biocversions where resource_id=47001;
