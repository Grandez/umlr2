#' La clase para interactuar con plantuml
#' @title PlantUML
#' @name R6PLANTUML
#' @aliases PLANTUML
#' @docType class
#' @description  La descripcion.
PLANTUML = R6::R6Class("R6PLANTUML", inherit = UMLR2BASE,
   active = list(
     #' @field config Devuelve el objeto CONFIG
      config = function(value) {
         if (!missing(value)) private$msg$err("R012", "config")
         private$cfg
      }
    )
   ,public = list(
       #' @description Crea una instancia de la clase PLANTUML
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       initialize         = function( ...) {
          if (substr(as.character(sys.call(-1))[1], 1, 8) == "PLANTUML") private$msg$err("E900")
          super$initialize(...)
       }
       #' @description destructor de la isntancia
      ,finalize = function() {
          files = gsub("\\..+$", "\\.\\*", private$umlFiles)
          lista = list.files(tempdir(), files, full.names=TRUE)
          suppressWarnings(file.remove(lista))
      }
      #' @description Crea un fichero temporal con el diagrama
      #'              generado a partir de los datos pasados
      #' @param data  Un objeto S3PlantUML o un vector de lineas interpretables por PlantUML
      #' @return El nombre del fichero con el diagrama
      ,genDiagram      = function(data, summary) {
          header = private$tpl$generate(summary)
          umlData = c(header, data)
          if (length(grep("@startuml", umlData, fixed = TRUE)) == 0) {
             umlData = c("@startuml \n", umlData, "\n@enduml \n")
          }
          umlFile = private$prepareFile(umlData)
          private$callPlantUML(umlFile)
          gsub("uml$", "png", umlFile)
      }
      #' @description Inserta un nuevo fichero de plantilla a la lista
      #' @param template  El nombre del fichero
      #' @param force     Si TRUE elimina los existentes
      ,addTemplate = function(template = NULL, replace = FALSE) {
           if(!is.null(template)) private$tpl$dd(template)
           invisible(self)
      }

   )
   ,private = list(
       umlFiles  = list()      # Files generated
      ,res    = NULL         # Messages from plantuml
      ,tpl    = TEMPLATE$new()
      ,callPlantUML     = function(umlFile) {
          # Si la llamada es correcta no informa status
          private$res = suppressWarnings( system2( private$cfg$getJVM()
                                          ,c("-jar", private$cfg$getPlantUML()
                                          ,"-tpng"
                                          ,"-verbose"
                                          ,umlFile), stdout=TRUE, stderr=TRUE)
          )
          private$msg$setExtErr(private$res)
          rc = ifelse(is.null(attr(private$res, "status")), 0, attr(private$res, "status"))
          if (rc != 0) private$msg$err("E001", newCode=rc)
      }
      ,prepareFile      = function(data) {
           tryCatch({
                 fileName = tempfile(fileext=".uml")
                 writeLines(data, fileName)
                 private$umlFiles[length(private$umlFiles) + 1] = basename(fileName)
                 return (fileName)
              }
             ,error = function(e) {
                 private$msg$err("E110")
             }
           )
      }
      ,removeUmlTags   = function (data) {
         gsub("@startuml | @enduml", "", data, fixed=TRUE)
      }

   )
)
