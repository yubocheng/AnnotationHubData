use_scripts <- function(filename)
{
    stopifnot(length(filename) == 1 && is.character(filename))

    use_directory("inst/scripts")
    edit_file(proj_path("inst/scripts", filename), open = FALSE)
}

hub_create_package <- function(package, type = c("AH", "EH"))
{
    stopifnot(
        length(package) == 1 && is.character(package),
        available_on_cran(package) == TRUE, 
        available_on_bioc(package) == TRUE,
        valid_package_name(package) == TRUE
    )

    create_package(package)

    ## Customization of the DESCRIPTION file
    usethis:::use_description_field("License", "Artistic-2.0", overwrite = TRUE)
    usethis:::use_description_field("LazyData", "false", overwrite = TRUE)
    usethis:::use_description_field("Version", "0.99.0", overwrite = TRUE)

    use_package_doc()
    use_roxygen_md()

    if (type == "AH") {
        use_package("AnnotationHubData")
        use_package("AnnotationHub")
        usethis:::use_description_field("biocViews", "AnnotationHub")
    }
    else {
        use_package("ExperimentHubData")
        use_package("ExperimentHub")
        usethis:::use_description_field("biocViews", "ExperimentHub")
    }

    ## Addition of README file
    use_readme_md(open = FALSE)

    ## Addition of NEWS file
    use_news_md(open = FALSE)

    ## Addition of the inst/extdata directory
    use_directory("inst/extdata")

    ## Addition of the inst/script R files
    use_scripts("make-data.R")
    use_scripts("make-metadata.R")

    ## Addition of man/ directory
    use_directory("man")
} 
