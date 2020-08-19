#' Genera un diagrama UML
#' @title UML
#' @docType class
#' @import R6
#' @export
library(R6)
UML = R6::R6Class("R6UML", inherit = PLANTUML,
   public = list(
       #' @description Crea una instancia de la clase
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       #'
       #' @examples
       #' uml = UML$new()
       #' uml = UML$new(jvm='java')
       #' uml = UML$new(c(jvm='java', plantuml='plantuml.jar'))
        initialize         = function( ...) {
            super$initialize(...)
            private$files = Files$new()
        }
        #' @description Destructor de la clase
       ,finalize = function() {
           super$finalize()
       }
       #' @description Genera un diagrama a partir de la definición pasada
       #' Si no es para guardar no es necesario generar el fichero
       #' el jar debe leer el fichero
       #' chequear la cabecera
       #' grabar el temporal
       #' @details
       #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
       #'     - El fichero con la imagen no se almacena en el sistema de archivos
       #' @param data  Definición del diagrama o fichero con la misma
       #' @param name  Si los datos son inline, los almacena previamente
       #' @param force Fuerza a generar el diagrama aunque no haya cambiado
       ,plot               = function(data=NULL, name=NULL, force=NULL) {
           imgFile = private$makeImage(data,name,force)
           knitr::include_graphics(imgFile)
       }
       #' @description Genera un link al fichero de imagen con el diagrama
       #' @seealso [plot()] para generacion en linea
       #' @param data  Definicion del diagrama o fichero con la misma
       #' @param caption Titulo de la imagen
       #' @param force Fuerza a generar el diagrama aunque no haya cambiado
       #' @return la cadena del link
       ,link              = function(data = NULL, name=NULL, caption = NULL, force = NULL) {
           imgFile = private$makeImage(data, name, force)
           target  = private$files$makeRelativeTo(imgFile, private$cfg$getOutputDir())
           paste0('[![', caption, "](", target, " \'", caption, "\')](https://127.0.0.1)")
       }
       #' @description Almacena la definicion del diagrama en el sistema de archivos
       #' @param data  Definicion del diagrama o fichero con la misma
       #' @param force Ignora la cache
       #' @return fullpath del fichero
       ,save               = function(data=NULL, force=NULL) {
           private$makeImage(data, self$getType())
       }
       #' @description Carga un fichero de definicion de diagrama
       #' @param fileName  Path al fichero con la definicion
       #' @return una clase S3PlantUML
       ,load             = function(fileName=NULL) {
           if (is.nul(fileName))        private$plantErr("E101", fileName)
           if (!file.exists(fileName))  private$plantErr("E101", fileName)
           tryCatch({
               data = readLines(fileName)
               structure(data, class = "S3PlantUML")
           },error = function (e) {
               private$plantErr("E102", fileName)
           }
           )
           names(data) = strsplit(fileName, ".", fixed = TRUE)[[1]]
           private$removeUmlTags(data)
       }
       #' @description Unifica diagramas
       #' @param ... diagramas a unificar
       #' @return una clase S3PlantUML
       ,merge             = function(...) {
       }
       #' @description convierte un fichero de diagramas PlantUML en un objeto
       #' @param diagram el fichero con la definicion del diagrama
       #' @return el objeto
       ,make             = function(diagramm) {
       }

   )
   ,private = list(nada=NULL
       ,makeImage = function(data, name, force) {
           private$files$setConfig(private$cfg)
           rc = private$files$prepareData(data, name, ifelse(is.null(force), private$cfg$getForce(), force))
           if (!rc) return (private$files$getImageFile())

           dataDef = private$files$getDefinition(data)
           imgFile = self$makeImage(dataDef)
           private$files$saveFile(imgFile)
       }
       ,removeUmlTags   = function (data) {
           gsub("@startuml | @enduml", "", data, fixed=TRUE)
        }
    )
)
