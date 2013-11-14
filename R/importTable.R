importTable <- function(recipe)
{
    md <- metadata(recipe)
    input.filename <- inputFiles(recipe)[1]
    output.filename <- outputFile(recipe)

    args <- md@RecipeArgs
    stopifnot(sort(names(args)) == c("colClasses", "header", "sep"))
    
    tbl <- read.table(input.filename,
                      header=args$header,
                      sep=args$sep,
                      colClasses=args$colClasses)
                      
    save(tbl, file=output.filename)
    output.filename
}
