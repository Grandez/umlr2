#' La clase para interactuar con plantuml
#' @title "R6 Package"
#' @name "R6 Package"
#' @rdname R6Package
#' @docType class
#' @description  La descripcion.
RMethod = R6::R6Class("RMETHOD"
    ,inherit      = RComposite
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        #' @description Crea una instancia de la clase
        #' @param name  Nombre de la clase
        #' @param generator Objeto generados
        #' @param detail Nivel de detalle generado
        #' @param deep   Nivel de profundidad
        #' @param type   Tipo de clase
        initialize = function(name, visibility) {
            super$initialize(ObjType$Method, name, visbility)
        }
    )
)
