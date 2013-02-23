test_ImportPreparer_constructors <- function()
{
    classes <- names(getClassDef("ImportPreparer")@subclasses)
    ## need to be no-arg capable
    for (cl in classes)
        checkTrue(validObject(new(cl)))
}
