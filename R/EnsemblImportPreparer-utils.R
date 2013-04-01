.ensemblBaseUrl <- "ftp://ftp.ensembl.org/pub/"

.ensemblReleaseRegex <- ".*release-(69|7[[:digit:]])"

## list directories below url/dir satisfying regex
.ensemblDirUrl <-
    function(url, dir, regex = .ensemblReleaseRegex)
{
    lst <- getURL(url=url, dirlistonly=TRUE, followlocation=TRUE)
    lst <- strsplit(lst, "\n")[[1]]
    releases <- paste0(url, lst)
    paste(grep(regex, releases, value=TRUE), dir, sep="/")
}

## rename SourcePath by replacing baseUrl with 'ensembl/'
.ensemblSourcePathFromUrl <-
    function(baseUrl, sourceUrl)
{
    sub(baseUrl, "ensembl/", sourceUrl)
}

## mangle url to metadata where possible
.ensemblMetadataFromUrl <-
    function(sourceUrl)
{
    sgRegex <- "^([[:alpha:]_]+)\\.(.*)\\.[[:digit:]]+\\.[[:alpha:]]+"
    releaseRegex <- ".*(release-[[:digit:]]+).*"
    title <- sub(".gz$", "", basename(sourceUrl))
    root <- setNames(rep(NA_character_, length(sourceUrl)), title)
    species <- gsub("_", " ", sub(sgRegex, "\\1", title), fixed=TRUE)
    taxonomyId <- local({
        uspecies <- unique(species)
        .taxonomyId(uspecies)[match(species, uspecies)]
    })
    list(annotationHubRoot = root, title=title, species = species,
         taxonomyId = taxonomyId, genome = sub(sgRegex, "\\2", title),
         sourceVersion = sub(releaseRegex, "\\1", sourceUrl))
}
