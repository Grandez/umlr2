IGeneral <- R6::R6Class(
    classname = "IGeneral",
    portable = TRUE,
    public = list(
        getCounter = function() stop("I'm an abstract interface method"),
        setCounter = function(value) stop("I'm an abstract interface method"),
        getMain = function() stop("I'm an abstract interface method"),
        setMain = function(value) stop("I'm an abstract interface method")
    )
)
