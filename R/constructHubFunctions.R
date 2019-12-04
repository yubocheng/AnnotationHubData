#' Function for creating a hub package
#' 
#' @param package The name of the hub package to be created.
#' @param type Either \code{AH} or \code{EH} depending on if the package should 
#'     be an AnnotationHub package or ExperimentHub package.
#' 
#' @importFrom available available_on_cran available_on_bioc valid_package_name
#' @import usethis
#'  
#' @return Path to newly created package
#' 
#' @export
#' 
#' @examples
#' hub_create_package("test_hub", "AH")

hub_create_package <- function(package, type = c("AH", "EH"))
{
    stopifnot(
        available_on_cran(package) == TRUE, 
        available_on_bioc(package) == TRUE,
        valid_package_name(package) == TRUE
    )

    create_package(package)

    ## Modifications made to DESCRIPTION file
    usethis:::use_description_field("License", "Artistic-2.0", overwrite = TRUE)
    usethis:::use_description_field("LazyData", "false", overwrite = TRUE)

    if (type == "AH") {
        use_package("AnnotationHubData", "Imports")
        use_package("AnnotationHub", "Imports")
        usethis:::use_description_field("biocViews", "AnnotationHub")
    }
    else {
        use_package("ExperimentHubData", "Imports")
        use_package("ExperimentHub", "Imports")
        usethis:::use_description_field("biocViews", "ExperimentHub")
    }
} 
