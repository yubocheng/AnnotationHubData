#-------------------------------------------------------------------------------
# in the unit tests we want to actually create data resources, just as they
# are in the full running version of the hub.  this requires a writable
# directory: we don't want to try to write into the extdata directory of
# an installed package! so we need a directory which can be reliably created,
# written to, and checked, on any computer these tests will run on.  
# a recursive copy of the (possibly deeply-nested) source directory (below
# pkg/extdata) into a temporary and necessarily writable directory provides
# the solution
.createWorkingDirectory <- function(sourceDirectory, verbose=FALSE)
{
    newDirectory <- tempdir()
    suppressWarnings({ # .svn directories do not copy
        result=file.copy(sourceDirectory, newDirectory, recursive=TRUE)
        })

    if(verbose)
        message(sprintf("result of copy from %s to %s: %s", sourceDirectory, newDirectory, result))

    file.path(newDirectory, basename(sourceDirectory))
}
