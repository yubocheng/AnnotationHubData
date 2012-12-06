library(AnnotationHubData)
library(RUnit)

runTests <- function()
{
    test_empty_constructor()
}

test_empty_constructor <- function()
{
    print ("--- test_empty_constructor")
    recipe <- AnnotationHubRecipe()
    checkTrue(validObject(recipe))
}

