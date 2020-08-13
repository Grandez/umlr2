#' La clase para interactuar con plantuml
#' @title R6UMLR
#' @docType class
#' @description  La descripcion.
#  Que opciones:
#  basico - Solo la clase y lo publico
#  simple - con accesors
#  private - rpivado y public
#
# por bits
#  UMLR$accesors + UMLR$private
#' @export
UMLR = R6::R6Class("UMLR", inherit = PLANTUML,
     public = list(
         detail = 254

         #####
         #' @description Crea una instancia de la clase
         #' @param ...  named values para definir la configuración
         #' @return La instancia del objeto
         #'
         #' @examples
         #' plant = UMLR$new()
         #' plant = UMLR$new(jvm='java')
         #' plant = UMLR$new(c(jvm='java', plantuml='plantuml.jar'))
        ,initialize         = function( ...) {
             super$initialize(...)
         }
         ,finalize = function() {
             super$finalize()
             message("Cleaning up R6UML")
         }
         #' @description Genera un diagrama a partir de la instancia del objeto
         #' @param object Instancia de objeto S4 o R6
         #' @param detail Instancia de objeto S4 o R6
         #' @param deep   Nivel de profundidad de analisis de la clase
         #' @details
         #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
         #'     - El fichero con la imagen no se almacena en el sistema de archivos
         ,plotClass             = function (object, detail=14, deep = 0) {
              uml = self$umlClass(object, detail, deep)
              imgFile = private$makeImage(uml)
              knitr::include_graphics(normalizePath(imgFile))
         }
         ,umlClass             = function (object, detail=14, deep = 0) {
             detail = sum(detail)
             private$maxDeep = private$.setMaxDeep(detail, deep)
             private$hecho = list()
             private$pend = NULL
             private$obj  = object
             uml = NULL
             if (isS4(object))      uml = private$parseS4(object, detail)
             if (R6::is.R6(object)) uml = ParserR6$new(object, detail)$parse()
             if (is.null(uml)) warning("'object' is not an instance of S4 or R6 Classes")
             names(uml) = NULL
             uml
         }
         ,header = function(header) {
           private$.header = header
         }
)
,private = list(
     .header = c("hide empty members", "hide empty fields")
    ,hecho = list()  # Para obtener NA la primera vez
    ,generators = NULL
    ,maxDeep = 0
    ,pend  = NULL
    ,obj   = FALSE

    ,parseS4 = function (object, full=FALSE, deep = FALSE) {
    }
    # ,getAttrs  = function (className, visibility) {
    #     data = list(fields = NULL, methods = NULL)
    #     lFields  = eval(parse(text=paste0(className, "$", visibility, "_fields")))
    #     lMethods = eval(parse(text=paste0(className, "$", visibility, "_methods")))
    #     list(fields=names(lFields), methods=names(lMethods))
    # }
    , .setMaxDeep = function(detail, deep) {
        res = ifelse(detail < UMLType$parents, 0, deep)
        if (bitwAnd(detail, UMLType$parents)    > 0 && res == 0) res = 1
        if (bitwAnd(detail, UMLType$subclasses) > 0 && res == 0) res = 1
        res
    }
  )
)

