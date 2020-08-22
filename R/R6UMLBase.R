#' La clase base de las clasespublicas
#' @title UMLBASE
#' @name UMLBASE
#' @rdname UMLBASE
#' @docType class
#' @description  Clase base/Interfaz de las clases visibles
UMLBASE = R6::R6Class("R6UMLBASE"
    ,inherit      = UMLR2BASE
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public       = list(
        #' @description Constructor
        #' @param ... Parametros de inicializacion
        initialize = function(...) {
            super$initialize(...)
            private$.plant  = PLANTUML$new(...)
            private$.parser = PARSER$new(NULL, 0, 0)
            private$.files  = Files$new(getConfig())
        }
        #' @description Agrega un conjunto de estilos a aplicar
        #' @param template la referencia de los estilos
        #'        Puede ser:
        #'        - El nombre del fichero proporcionado por el paquete
        #'        - Un fichero con extension accesible
        #'        - Datos de definicion
        #' @param replace Si TRUE elimina los ficheros  existentes
        ,addStyle = function(template = NULL, replace = FALSE) {
            .plant$addTemplate(template, replace, "style")
            invisible(self)
        }
        #' @description Agrega una parte de definicion del diagrama al inicio
        #' @param template la referencia de los estilos
        #' @param replace Si TRUE elimina los ficheros  existentes
        ,addHeader = function(template = NULL, replace = FALSE) {
            .plant$addTemplate(template, replace, "header")
        }
        #' @description Agrega una parte de definicion del diagrama al final
        #' @param template la referencia de los estilos
        #' @param replace Si TRUE elimina los ficheros  existentes
        ,addFooter = function(template = NULL, replace = FALSE) {
            .plant$addTemplate(template, replace, "footer")
        }
    )
    ,private = list(
         .plant  = NULL
        ,.parser = NULL
        ,.files  = NULL
    )
)
