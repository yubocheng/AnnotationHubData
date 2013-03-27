## -----------------------------------------------------------------------------
## Memo-ize some web access, to avoid unnecessary internet traffic
## 

.ucscSession <- local({
    session <- NULL
    function() {
        if (is.null(session)) {
            session <<- browserSession()
        }
        session
    }
})

.GRangesForUSCSGenome <- local({
    env <- new.env(parent=emptyenv())
    function(genome) {
        if (!exists(genome, env))
            env[[genome]] <- GRangesForUCSCGenome(genome)
        env[[genome]]
    }
})

.ucscTableQuery <- local({
    env <- new.env(parent=emptyenv())
    function(genome, trackName) {
        if (!exists(genome, env)) {
            range <- .GRangesForUSCSGenome(genome)
            env[[genome]] <- ucscTableQuery(.ucscSession(), trackName, range)
        } else {
            trackName(env[[genome]]) <- trackName
        }
        env[[genome]]
    }
})

