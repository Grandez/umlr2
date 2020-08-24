#' La clase para interactuar con plantuml
#' @title UML
#' @name UML
#' @rdname UML
#' @docType class
#' @description  La descripcion.
#' @export
UMLR = R6::R6Class("R6UMLR"
   ,inherit       = UMLR2
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
       #' @description Crea una instancia de la clase
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
        initialize         = function( ...) {
            super$initialize(...)
            #files = Files$new()
            private$container = RCONTAINER$new()
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
               uml     = umlClass(object, detail, deep)
               imgFile = .plant$genDiagram(uml, .parser$getSummary())
               knitr::include_graphics(normalizePath(imgFile))
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
       ,addClass         = function (cls, detail=NULL, deep = NULL) {
           if (missing(cls))                            msg$err("E001", "cls")
           if (!is.null(detail) && !is.numeric(detail)) msg$err("E002", "detail")
           if (!is.null(deep)   && !is.numeric(deep))   msg$err("E002", "deep")
           addClasses(cls, detail=detail, deep=deep)
       }
       ,addClasses       = function (..., detail=NULL, deep=NULL) {
           objs  = lapply(list(...), function(obj) PARSER$parse(obj, detail, deep))
           container$add(objs)
       }
       ,addPackage         = function (name, ..., style=NULL) {
           if (missing(name))                           msg$err("E001", "name")
           if (!is.null(style) && !is.numeric(style))   msg$err("E002", "style")
           if ("RPACKAGE" %in% class(name)) {
               container$add(name)
           }
           else {
               container$add(RPackage$new(name, ..., style))
           }
       }
       ,addNamespace         = function (name, ..., style=NULL) {
           if (missing(name))                           msg$err("E001", "name")
           if (!is.null(style) && !is.numeric(style))   msg$err("E002", "style")
           if ("RNAMESPACE" %in% class(name)) {
               container$add(name)
           }
           else {
               container$add(RNamespace$new(name, ..., style))
           }
       }

   )
   ,private = list(nada=NULL
       ,files = NULL
       ,container = NULL
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
