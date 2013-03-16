globalVariables(c("speciesMap", "futile.logger"))

.onLoad <-
    function(libname, pkgname)
{
   logDir <- file.path(Sys.getenv("HOME"),
        sprintf(".%s", pkgname))
    if (!file.exists(logDir))
    {
        .printf("Creating log directory %s", logDir)
        dir.create(logDir)
    }
    l <- library
    l(futile.logger)
    flog.threshold(TRACE)
    flog.appender(appender.file(file.path(logDir,
        sprintf("%s.log", pkgname))), name="file")
}
