#' La clase para interactuar con plantuml
#' @title "R6 Package"
#' @name "R6 Package"
#' @rdname R6Package
#' @docType class
#' @description  La descripcion.
RComponent = R6::R6Class("RCOMPONENT"
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        #' @field name Nombre de la clase
        name = NULL

    )
)
