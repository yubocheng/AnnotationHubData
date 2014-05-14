importTable <- function(ahm)
{
    md <- metadata(ahm)
    input.filename <- inputFiles(ahm)[1]
    output.filename <- outputFile(ahm)

    args <- md@RecipeArgs
    stopifnot(sort(names(args)) == c("colClasses", "header", "sep"))
    
    tbl <- read.table(input.filename,
                      header=args$header,
                      sep=args$sep,
                      colClasses=args$colClasses)
                      
    save(tbl, file=output.filename)
    output.filename
}
