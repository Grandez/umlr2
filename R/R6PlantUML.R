#' La clase para interactuar con plantuml
#' @title PlantUML
#' @name PLANTUML
#' @docType class
#' @description  La descripcion.
PLANTUML = R6::R6Class("R6PLANTUML"
   ,inherit      = UMLR2Base
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,active = list(
     #' #' @field config Devuelve el objeto CONFIG
     #'  config = function(value) {
     #'     if (!missing(value)) msg$err("R012", "config")
     #'     cfg
     #'  }
    )
   ,public = list(
       #' @description Crea una instancia de la clase PLANTUML
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       initialize         = function( ...) {
          #if (substr(as.character(sys.call(-1))[1], 1, 8) == "PLANTUML") msg$err("E900")
          super$initialize(...)
          private$.tpl = TEMPLATE$new()
       }
       #' @description destructor de la instancia
      ,finalize = function() {
          files = gsub("\\..+$", "\\.\\*", umlFiles)
          lista = list.files(tempdir(), files, full.names=TRUE)
          suppressWarnings(file.remove(lista))
      }
      #' @description Obtiene la definicion del diagrama
      #' @param data    Datos a procesar
      #' @param summary Informacion resumida de los objetos analizados
      ,getDiagram = function(data, summary) {
          header  = .tpl$getHeaders()
          footer  = .tpl$getFooters()
          umlData = c(header, data, footer)
          # processVariables(umlData, summary)
          if (length(grep("@startuml", umlData, fixed = TRUE)) == 0) {
              umlData = c("@startuml \n", umlData, "\n@enduml \n")
          }
          paste(umlData,collapse="\n")
      }
      #' @description Crea un fichero temporal con el diagrama
      #'              generado a partir de los datos pasados
      #' @param data  Un objeto S3PlantUML o un vector de lineas interpretables por PlantUML
      #' @param summary Informacion resumida de los objetos analizados#'
      #' @return El nombre del fichero con el diagrama
      ,genDiagram      = function(data, summary) {
          umlData = getDiagram(data, summary)
          umlFile = prepareFile(umlData)
          callPlantUML(umlFile)
          gsub("uml$", "png", umlFile)
      }
      #' @description Inserta un nuevo fichero de plantilla de estilos a la lista
      #' @param template  El nombre del fichero
      #' @param replace   Si TRUE elimina los existentes
      #' @param type      Tipo de plantilla a agregar
      ,addTemplate = function(template = NULL, replace = FALSE, type) {
           if(!is.null(template)) .tpl$add(template, replace, type)
           invisible(self)
      }
   )
   ,private = list(
       umlFiles  = list()      # Files generated
      ,.tpl  = NULL
      ,callPlantUML     = function(umlFile) {
          # Si la llamada es correcta no informa status
          oldwd  = getwd()
          setwd("c:/tmp")
          res = suppressWarnings( system2( cfg$getJVM()
                                          ,c("-jar", cfg$getPlantUML()
                                          ,"-tpng"
                                          ,"-verbose"
                                          ,umlFile), stdout=TRUE, stderr=TRUE)
          )
          msg$setExtErr(res)
          setwd(oldwd)
          rc = ifelse(is.null(attr(res, "status")), 0, attr(res, "status"))
          if (rc != 0) msg$err(rc, res[grepl("Error", res)])
      }
      ,prepareFile      = function(data) {
           tryCatch({
                 fileName = tempfile(fileext=".uml")
                 writeLines(data, fileName)
                 umlFiles[length(umlFiles) + 1] = basename(fileName)
                 return (fileName)
              }
             ,error = function(e) {
                 msg$err("E110")
             }
           )
      }
      ,processVariables = function(umlData, summary) {
         vars = VARS$new()
         data = processVar(umlData, summary[["subclasses"]], vars$subClass)
         data = processVar(umlData, summary[["superclasses"]], vars$superClass)
         data = processVar(umlData, summary[["classes"]], vars$class)
         data[grep(vars$pattern, data, invert=TRUE)]
      }
      ,processVar = function(umlData, classes, tag) {
         if (is.null(classes)) return (umlData)
         c(sapply(classes, function(x) gsub(tag, x, umlData, ignore.case=TRUE)))
      }

      ,removeUmlTags   = function (data) {
         gsub("@startuml | @enduml", "", data, fixed=TRUE)
      }

   )
)
