.printf <- function(...) print(noquote(sprintf(...)))


## from ?grep, by Luke Tierney
URL_parts <- function(x)
{
    m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
    parts <- do.call(rbind,
       lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
}


url.exists <- function(url)
{
   HEAD(url)$headers$status == "200"
}
