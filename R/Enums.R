# #' Clase con consantes
# #' @title UMLR
# #' @docType class
# #' @description  Constantes para usar
# #'
# .UMLRDefs = R6::R6Class("_UMLRDefs_",
#     public = list(
#          basic       =   0   # Solo los datos publicos propios y heredados
#         ,simple      =   1   # Solo los datos publicos propios
#         ,complete    =   2   # Info privada
#         ,parent      =   4   # Padre si existe
#         ,composition =   8   # Clases usadas si existen
#         ,simple2     =  16   #
#         ,complete2   =  32
#         ,parents     =  64   # Parents
#         ,sons        = 128
#     )
# )

.RUMLENUM =  function(...) {
    ## EDIT: use solution provided in comments to capture the arguments
    values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)

    #stopifnot(identical(unique(values), values))
    val = as.integer(values)
    names(val) = names(values)
    #res <- setNames(seq_along(values), values)
    res <- as.environment(as.list(val))
    lockEnvironment(res, bindings = TRUE)
    res
}
# |7|6|5|4|3|2|1|0|  |7|6|5|4|3|2|1|0|
# |Include| show  |  |Include| show  |
UMLShow = .RUMLENUM(
     basic         =   0   # Solo los datos publicos propios y heredados
    ,simple        =   1   # Solo los datos publicos propios
    ,complete      =   2   # Info privada
    ,superClasses  =  16   # Padre si existe
    ,subClasses    =  32   # Clases usadas si existen
    ,classSimple   = 256   #
    ,classComplete = 512
)

ClassType = .RUMLENUM(
     unknow     = 0
    ,class      = 1
    ,superClass = 2
    ,subClass   = 4
)
