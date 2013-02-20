test_DataProvider_constructors <- function()
{
    classes <- names(getClassDef("DataProvider")@subclasses)
    ## need to be no-arg capable
    for (cl in classes)
        checkTrue(validObject(new(cl)))
}
