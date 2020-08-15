#' La clase para interactuar con plantuml
#' @title UMLR
#' @docType class
#' @name R6UMLR
#' @description  La descripcion.
#' @export
UMLR = R6::R6Class("R6UMLR", inherit = PLANTUML,
     public = list(
         #' @description Crea una instancia de la clase
         #' @param ...  named values para definir la configuración
         #' @return La instancia del objeto
         #'
         #' @examples
         #' plant = UMLR$new()
         #' plant = UMLR$new(jvm='java')
         #' plant = UMLR$new(c(jvm='java', plantuml='plantuml.jar'))
          initialize     = function( ...) { super$initialize(...) }
          #' @description destructor de la clase
         ,finalize      = function()     { super$finalize()      }
         #' @description Genera un diagrama a partir de la instancia del objeto
         #' @param object Instancia de objeto S4 o R6
         #' @param detail Nivel de detalle
         #' @param deep   Nivel de profundidad de analisis de la clase
         #' @details
         #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
         #'     - El fichero con la imagen no se almacena en el sistema de archivos
         ,plotClass             = function (object, detail=UMLShow$simple, deep = 1) {
              uml = self$umlClass(object, detail, deep)
              imgFile = private$makeImage(uml)
              knitr::include_graphics(normalizePath(imgFile))
         }
         #' @description Genera la definicion del diagrama a partir de la instancia del objeto
         #' @param object Instancia de objeto
         #' @param detail Nivel de detalle
         #' @param deep   Nivel de profundidad de analisis de la clase
         #' @details
         #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
         #'     - El fichero con la imagen no se almacena en el sistema de archivos
         ,umlClass             = function (object, detail=UMLShow$simple, deep = 1) {
             detail = sum(detail)
             # private$maxDeep = private$.setMaxDeep(detail, deep)
             uml = NULL
             if (isS4(object))      uml = ParserS4$new(object, detail, deep)$parse()
             if (R6::is.R6(object)) uml = ParserR6$new(object, detail, deep)$parse()
             if (is.null(uml)) warning("'object' is not an instance of S4 or R6 Classes")
             names(uml) = NULL
             uml
         }
#         ,plotPackage = function(object, detail=UMLShow$simple, deep = 1) {
#
#          }
           #' @description Agrega definiciones de PlantUML al inicio del documento
           #' @param header definiciones de PlantUML
         ,header = function(header) {
             private$.header = header
             invisible(self)
         }
)
,private = list(
     .header = c("hide empty members", "hide empty fields")
#    ,generators = NULL
#    ,maxDeep = 0
    # ,getAttrs  = function (className, visibility) {
    #     data = list(fields = NULL, methods = NULL)
    #     lFields  = eval(parse(text=paste0(className, "$", visibility, "_fields")))
    #     lMethods = eval(parse(text=paste0(className, "$", visibility, "_methods")))
    #     list(fields=names(lFields), methods=names(lMethods))
    # }
  )
)

