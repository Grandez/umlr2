#' La clase para interactuar con plantuml
#' @title "R6 Package"
#' @name "R6 Package"
#' @rdname R6Package
#' @docType class
#' @description  La descripcion.
RComponent = R6::R6Class("RCOMPONENT"
    ,inherit      = UMLR2Base
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        #' @field name Nombre de la clase
         name = NULL
        ,type = 0
        ,class = 0
        ,initialize = function(class, name, type) {
            super$initialize()
            self$class = class
            self$name  = name
            self$type  = type
        }
        ,addType = function(type) {
            self$type = bitwOr(self$type, type)
        }
        ,definition = function() {
            msg$error("E910", "definition")
        }


    )
    ,private = list(
        toName  = function(value, enum)  { names(which(enum == value))[1] }
       ,toValue = function(name,  enum)  { eval(parse(text=paste0("enum$", name))) }
    )
)
