#' La clase para interactuar con plantuml
#' @title UML
#' @name UML
#' @rdname UML
#' @docType class
#' @description  La descripcion.
#' @export
UML = R6::R6Class("R6UML", inherit = UMLR2BASE, portable = FALSE, lock_objects = TRUE
   ,public = list(
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
            files = Files$new()
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
           imgFile = makeImage(data,name,force)
           knitr::include_graphics(imgFile)
       }
       #' @description Genera un link al fichero de imagen con el diagrama
       #' @seealso [plot()] para generacion en linea
       #' @param data  Definicion del diagrama o fichero con la misma
       #' @param name  Nombre del fichero si se quiere guardar
       #' @param caption Titulo de la imagen
       #' @param force Fuerza a generar el diagrama aunque no haya cambiado
       #' @return la cadena del link
       ,link              = function(data = NULL, caption = NULL, name = NULL, force = NULL) {
           if (is.null(name)) msg$err("R296", "name")
           target = makeImage(data, name, force)
           #target  = files$makeRelativeTo(imgFile, cfg$getOutputDir())
           paste0('[![', caption, "](", target, " \'", caption, "\')](https://127.0.0.1)")
       }
       #' @description Almacena la definicion del diagrama en el sistema de archivos
       #' @param data  Definicion del diagrama o fichero con la misma
       #' @param name  Nombre del fichero si los datos son en linea
       #' @param force Ignora la cache
       #' @return Path del fichero
       ,save               = function(data=NULL, name=NULL, force=NULL) {
           makeImage(data, name, force)
       }
       #' @description Carga un fichero de definicion de diagrama
       #' @param fileName  Path al fichero con la definicion
       #' @return una clase S3PlantUML
       ,load             = function(fileName=NULL) {
           if (is.nul(fileName) || !file.exists(fileName)) plantErr("E101", fileName)
           tryCatch({
               data = readLines(fileName)
               structure(data, class = S3Class)
           },error = function (e) {
               msg.err("E102", fileName)
           }
           )
           names(data) = strsplit(fileName, ".", fixed = TRUE)[[1]]
           removeUmlTags(data)
       }
       #' @description Unifica diagramas
       #' @param ... diagramas a unificar
       #' @return una clase S3PlantUML
       ,merge             = function(...) {
       }
       #' @description convierte un fichero de diagramas PlantUML en un objeto
       #' @param diagram el fichero con la definicion del diagrama
       #' @return el objeto
       ,make             = function(diagram) {
       }

   )
   ,private = list(nada=NULL
       ,files = NULL
       ,makeImage = function(data, name, force) {
           files$setConfig(cfg)
           rc = files$prepareData(data, name, ifelse(is.null(force), cfg$getForce(), force))
           if (!rc) return (files$getImageFile())

           dataDef = files$getDefinition(data)
           imgFile = plant$genDiagram(dataDef)
           files$saveFile(imgFile)
       }
    )
)
