test_ImportPreparer_constructors <- function()
{
    classes <- names(getClassDef("ImportPreparer")@subclasses)
    ## need to be no-arg capable
    valid <- sapply(classes, function(cl) validObject(new(cl)))
    checkTrue(all(valid))
}
