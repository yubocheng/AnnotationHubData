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

## log messages to console AND to a file
flog <- function(level, msg)
{
    loggerFunction <- switch(level,
        "flog.fatal",
        "flog.error",
        "noop",
        "flog.warn",
        "noop",
        "flog.info",
        "noop",
        "flog.debug",
        "flog.trace"
        )
    do.call(loggerFunction, list(msg=msg))
    do.call(loggerFunction, list(msg=msg, name="file"))
}