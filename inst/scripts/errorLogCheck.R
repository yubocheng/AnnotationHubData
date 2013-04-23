
## Code to parse error log

lines = readLines("error.log")

## 1st lets get a list of the tracks that failed
bt1 = sub("^.+hgdownload.cse.ucsc.edu\\/goldenpath\\/.+\\/database\\/","",lines, perl=TRUE)
bt = sub(":.+$","",bt1, perl=TRUE)

## and the genomes
bg1 = sub("^.+hgdownload.cse.ucsc.edu\\/goldenpath\\/","",lines, perl=TRUE)
bg = sub("/.+$","",bg1, perl=TRUE)

## now lets start categorizing the errors.
er <- sub("^.+hgdownload.cse.ucsc.edu\\/goldenpath\\/.+\\/database\\/.+?: ","",lines, perl=TRUE)

## So now we could make a data.frame
df = data.frame(genome=bg, track=bt, error=er, stringsAsFactors=FALSE)
## but probably we want to get rid of redundant genome/track info.
dupIdx = !duplicated(df[,c(1,2)])
erTab = df[dupIdx,] ## throws out 2nd+ occurrences of genome/track

save(erTab,file="errorTable.Rda")

