#' La clase para interactuar con plantuml
#' @title PlantUML
#' @name R6PLANTUML
#' @aliases PLANTUML
#' @docType class
#' @description  La descripcion.
PLANTUML = R6::R6Class("R6PLANTUML", inherit = UMLR2Base,
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
          if (substr(as.character(sys.call(-1))[1], 1, 6) == "PLANTUML") private$msg$err("E900")
          parms = unlist(list(...))
          if (sum(names(parms) == "") > 0) private$msg$err("R103")
          private$cfg = CONFIG$new(parms)
       }
       #' @description destructor de la isntancia
      ,finalize = function() {
          files = gsub("\\..+$", "\\.\\*", private$files)
          lista = list.files(tempdir(), files, full.names=TRUE)
          suppressWarnings(file.remove(lista))
      }
      #' @description Cambia los datos de configuracion de la instancia
      #' @param ...  named values para definir la configuracion
      #' @return La instancia del objeto
      ,setConfig         = function(...) {
          private$cfg$setConfig(...)
          invisible(self)
       }
      #' @description Obtiene los datos de configuracion
      #' @return Una lista con los datos de configuracion
      ,getConfig         = function() { private$cfg }
   )
   ,private = list(
       inline = FALSE        # Mark data as inline
      ,files  = NULL         # Files generated
      ,res    = NULL         # Messages from plantuml
      ,force  = FALSE        # Recreate diagrams
      ,cfg    = NULL
      ,testData="@startuml
                    [*]-->STATE1
                 @enduml"
      ,restoreForce     = function() {
         # if (!is.null(private$oldForce)) {
         #     self$force = private$oldForce
         #     private$oldForce = NULL
         # }
      }
      ,makeImage        = function(data, force=FALSE) {
        private$dataInline(data)
        umlFile = private$mountInputFile(data)
        imgFile = gsub("\\..+$", paste0(".png"), umlFile)

        # Verificar que el fichero o los datos no han cambiado
        if (!force) {
          if (!private$inline) {
            if (!private$fileChanged(umlFile, imgFile)) return (imgFile)
          }
          else {
            # Buscar el MD5
          }
          # Como verificar los temporales
          #JGG else {
          #JGG   if (!private$fileTempChanged(data)) return (private$mountOutputFile(data))
          #JGG }
        }

        private$callPlantUML(umlFile)
        imgFile
      }
      ,callPlantUML     = function(umlFile) {
          f = umlFile
          cfg = private$cfg
          if (!private$inline) {
              f = file.path(tempdir(), basename(umlFile))
              file.copy(umlFile, f)
          }
          private$files = c(private$files, basename(f))

          # Si la llamada es correcta no informa status
          private$res = suppressWarnings( system2( cfg$getJVM()
                                          ,c("-jar", cfg$getPlantUML()
                                          ,"-tpng"
                                          ,"-verbose"
                                          ,f), stdout=TRUE, stderr=TRUE)
          )
          rc = ifelse(is.null(attr(private$res, "status")), 0, attr(private$res, "status"))
          if (rc != 0) private$msg$err("E001", newCode=rc)
      }
      ,dataInline       = function(data) {
          private$inline = FALSE
          if (is.list(data) || length(data) > 1)  {
              private$inline = TRUE
          }
          else {
              words = strsplit(data, " ")
              if (length(words[[1]]) > 1) private$inline = TRUE
              words = strsplit(data, "\n")
              if (length(words[[1]]) > 1) private$inline = TRUE
          }
      }
      ,mountInputFile   = function(data) {
        if (private$inline) {
          inFile = basename(tempfile("pumld"))
          if (!is.null(names(data))) inFile = paste0("pumln", names(data)[1])
          return (private$prepareFile(data, file.path(tempdir(), paste0(inFile, ".uml"))))
        }

        inFile = data
        # Check for absolute path in nix and windows
        if (substring(inFile, 1,1) == "/") return(data)
        if (nchar(inFile) > 1 && substring(inFile, 2,2) == ":") return(data)

        if (length(grep(".", inFile, fixed=TRUE))== 0 ) {
          inFile = paste(inFile, private$cfg$getExt(), sep=".")
        }

        inFile = file.path(private$cfg$getInputDir(), inFile)
        if (!file.exists(inFile)) private$msg$err("R205")
        inFile
      }
      # ,mountOutputFile  = function(umlFile, ext) {
      #     ff = strsplit(basename(umlFile), ".", fixed=TRUE)[[1]][1]
      #     file.path(self$getOutputDir(TRUE), paste0(ff,".",ext))
      # }
      ,prepareFile      = function(data, fileName) {
          txt = data
          if (is.list(data)) txt = unlist(data)
          if (length(txt) > 1) txt = paste(txt, collapse="\n")
          if (length(grep("@startuml", txt, fixed = TRUE)) == 0) {
              txt = paste("@startuml \n", txt, "\n@enduml \n")
          }
          writeLines(txt, fileName)
          fileName
      }

      ,fileChanged      = function(umlFile, imgFile) {
        if (!file.exists(imgFile)) return (TRUE)
        file.info(umlFile)$mtime > file.info(imgFile)$mtime
      }
   )
)
