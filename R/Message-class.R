.Message <- setRefClass("Message",
    fields=list(
        name="character"
    ),
    methods=list(
        append = function(fmt, ...) {
            .self$name <- c(name, sprintf(fmt, ...))
            invisible(.self)
        },
        validity = function() {
            "report if any messages (e.g., after validity check)"
            if (length(name)) name else NULL
        },
        isComplete = function() {
            "stop if any messages"
            if (length(name)) {
                stop(paste(name, collapse="\n"))
            } else TRUE
        }
    )
)
