library(magick)
## #' A wrapper for PlantUML diagrams
## #'
## #' @docType class
## #' @export
## #' @format An \code{R6Class} generator object
## #'
## #' @section Methods:
## #' \describe{
## #'   \itemize{\code{initialize(...)}}{
## #'      Crea un objeto PLANTUML
## #'      Acepta un conjunto de parametros nombre=valor_cadena donde nombre
## #'      identifica un parametro de configuracion
## #'   }
## #'   \itemize{\code{plot(data[, type])}{
## #'      Dibuja el diagrama
## #'      data es la definicion del diagrama via fichero o en linea
## #'      type define el formato del grafico, por defecto "png"
## #'   }
## #'   \itemize{\code{file(data, caption[, force])}{
## #'      Devuelve la referencia a la imagen almacenada en un fichero
## #'      data es la definicion del diagrama via fichero
## #'      caption El mensaje a poner en el fichero
## #'      force   ver caching
## #'   }
## #'   \itemize{\code{image(data)}{
## #'      Devuelve un objeto con la imagen generada
## #'      data es la definicion del diagrama via fichero o en linea
## #'   }
## #'   \itemize{\code{checkInstallation([verbose, first])}{
## #'      Verifica si la instalacion de los componentes es correcta
## #'      verbose Muestra mensajes de progreso
## #'      first   Si establecido, el proceso se detiene en el primer error
## #'   }
## #'
## #'
## #'
## #' }
PLANTUML = R6::R6Class("PLANTUML",
   public = list(
       force = FALSE
      ,initialize = function( ...) {
        browser()
        parms = unlist(list(...))
        if (sum(names(parms) == "") > 0) private$plantErr("R103")
        self$setConfig(parms)
      }
      ,plot  = function(data=NULL, type="png") {
          image = private$makeImage(data, type)
          plot(as.raster(image))
      }
      ,file             = function(data=NULL, caption=NULL, force=NULL) {
          makeFile = TRUE
          if (is.null(self$getOutputDir())) private$plantErr("R202")
          if (private$dataInline(data))     private$plantErr("R204")
          if (!is.null(force) && !is.logical(force)) private$plantErr("R203")

          type    = self$getType()
          umlFile = private$mountInputFile(data)
          imgFile = private$mountOutputFile(umlFile, self$getOutputDir(), type)

          if (!is.null(force) && !force)  {
              makeFile = private$fileChanged(umlFile, imgFile)
          }
          outFile = file.path(self$getOutputDir(), basename(imgFile))
          if (makeFile) private$callPlantUML(umlFile, dirname(imgFile), type)
          paste0('![', caption, '](', outFile, ')')
      }
      ,image  = function(data=NULL) {
          private$makeImage(data, self$getType())
      }
      ,checkInstallation = function(verbose=TRUE, first=FALSE) {
          rc = TRUE
          if (verbose) message(private$msgErr["I001"], appendLF = FALSE)
          rp = private$checkJVM()
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)
          if (verbose) message(private$msgErr["I002"], appendLF = FALSE)
          rp = private$checkDOT()
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)
          if (verbose) message(private$msgErr["I003"], appendLF = FALSE)
          rp = file.exists(self$getPlantUML())
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (rc) {
              if (verbose) message(private$msgErr["I004"], appendLF = FALSE)
              rp = private$checkExecution()
              if (verbose) message(ifelse (rp, "OK", "KO"))
              rc = rc && rp
              if (first && !rc) return(rc)
              if (verbose) message(private$msgErr["I005"], appendLF = FALSE)
              rp = private$checkEnvironment()
              if (verbose) message(ifelse (rp, "OK", "KO"))
          }
          rc && rp
      }
      ,setConfig = function(values) {
         flags = names(values) %in% names(private$cfg)
         noFlags = names(values)[!flags]
         if (length(noFlags) > 0) private$plantErr("R104", noFlags)
         nm = names(values) # tipo
         eval(parse(text=paste0("private$cfg[\"", nm, "\"] = values[\"", nm, "\"]")))
      }
      ,getConfig         = function() { private$cfg }
      # Getters and setters
      ,getJVM       = function() private$cfg[["jvm"]]
      ,getPlantUML  = function() private$cfg[["plantuml"]]
      ,getInputDir  = function() private$cfg[["inputDir"]]
      ,getOutputDir = function() private$cfg[["outputDir"]]
      ,getExt       = function() private$cfg[["ext"]]
      ,getType      = function() private$cfg[["type"]]
      ,getCharset   = function() private$cfg[["charset"]]
      ,setJVM       = function(value) { private$cfg["jvm"]      = private$checkString(value); invisible(self) }
      ,setPlantUML  = function(value) { private$cfg["plantuml"] = private$checkString(value); invisible(self) }
      ,setInputDir  = function(value) { private$cfg["inputDir"] = private$checkString(value); invisible(self) }
      ,SetExt       = function(value) { private$cfg["ext"]      = private$checkString(value); invisible(self) }
      ,setType      = function(value) { private$cfg["type"]     = private$checkString(value); invisible(self) }
      ,setCharset   = function(value) { private$cfg["charset"]  = private$checkString(value); invisible(self) }
      ,setOutputDir = function(value) {
         if (is.null(value)) {
           private$cfg["outputDir"]
         }
        else {
           private$cfg["outputDir"] = private$checkString(value)
        }
        invisible(self)
      }
      # Usado para lanzar excepciones desde S3
      ,launchException = function(code) {
        private$plantErr(code, stack = 2)
      }
   )
   ,private = list(
       config = NULL
      ,oldForce = NULL
      ,tempFile = FALSE
      ,cfg=list( jvm       = "java"
                 ,plantuml  = "plantuml.jar"
                 ,ext       = "uml"
                 ,type      = "png"
                 ,charset   = "utf-8"
                 ,inputDir  = "."
                 ,outputDir = NULL
      )
      ,msgErr = c(
         R001="Invalid value for %s"
        ,R002="Invalid directory: %s"
        ,R003="Invalid parameter: %s"
        ,R005="Invalid flag: %s"
        ,R006="Invalid or missing value"
        ,R101="JVM not found: %s"
        ,R102="Component not found: %s"
        ,R103="All values must be named"
        ,R104="Incorrect configuration names: %s"
        ,R310="Flags must be character"
        ,R311="Invalid flags: %s"
        ,R201="No info provided for diagram"
        ,R202="outputDir must be set"
        ,R203="force must be TRUE or FALSE"
        ,R204="This method requires a file"
        ,R205="Input file not found: %s"
        ,I001="Checking JVM machine \t"
        ,I002="Checking Graphviz \t"
        ,I003="Checking plantuml.jar \t"
        ,I004="Checking execution \t"
        ,I005="Checking environment \t"
        ,E001="Error generating diagram: %d"
      )
      ,testData="@startuml
                    [*]-->STATE1
                 @enduml"
      ,plantErr = function(code, ..., newCode=0,stack=0) {
        msg = sprintf(private$msgErr[code], ...)
        if (newCode != 0) code = sprintf("E%03d", newCode)
        text = sprintf("\n\nPU%s - %s", code, msg)
        c <- errorCondition(text, class=c("plantUMLErr", "error"),
                            call = sys.call(-(1 + stack), ...))
        stop(c)
      }
      ,makeImage        = function(data,type) {
          umlFile = private$mountInputFile(data)
          imgFile = private$mountOutputFile(umlFile, NULL, type)
          rc      = private$callPlantUML(umlFile, dirname(imgFile), type)
          if (rc != 0) {
              private$restoreForce()
              private$plantErr("E001", newCode=rc)
          }
          image = magick::image_read(imgFile)
          file.remove(imgFile)
          if (private$tempFile) {
              file.remove(umlFile)
              private$tempFile = FALSE
          }
          image
      }
      ,callPlantUML     = function(umlFile, outDir, type) {
          # Si la llamada es correcta no informa status
          res = system2( self$getJVM()
                        ,c("-jar", self$getPlantUML()
                        ,paste0("-t", type)
                        ,"-o", normalizePath(outDir)
                        ,umlFile), stdout=TRUE, stderr=TRUE)
          ifelse(is.null(attr(res, "status")), 0, attr(res, "status"))
      }
      ,mountInputFile   = function(data) {
          private$tempFile = FALSE
          if (is.null(data)) private$plantErr("R201", stack=1)
          if (private$dataInline(data)) {
              private$tempFile = TRUE
              tmpFile = paste0(tempfile(), ".uml")
              write(data, tmpFile)
              return(tmpFile)
          }

          # Check for absolute path in nix and windows
          if (substring(data, 1,1) == "/") return(data)
          if (nchar(data) > 1 && substring(data, 2,1) == ":")
              return(data)

          tmpFile = data
          if (length(strsplit(tmpFile, ".",fixed=TRUE)[[1]]) == 1 ) {
              tmpFile = paste(tmpFile, self$getExt(), sep=".")
          }

          fullFile = file.path(self$getInputDir(), tmpFile)
          if (!file.exists(fullFile)) private$plantErr("R205", stack=1)
          fullFile
      }
      ,mountOutputFile  = function(umlFile, odir, ext) {
          ff = strsplit(basename(umlFile), ".", fixed=TRUE)[[1]][1]
          oDir = ifelse(is.null(odir), tempdir(), self$getOutputDir())
          file.path(oDir, paste0(ff,".",ext))
      }
      ,dataInline       = function(data) {
         words = strsplit(data, " ")
         if (length(words[[1]]) > 1) return (TRUE)
         words = strsplit(data, "\n")
         if (length(words[[1]]) > 1) return (TRUE)
         FALSE
      }
      ,fileChanged = function(umlFile, imgFile) {
          if (!file.exists(imgFile)) return (TRUE)
          file.info(umlFile)$ctime > file.info(imgFile)$ctime
      }

      ,checkJVM         = function() {
        rc = suppressWarnings(system2( self$getJVM()
                                       ,"-version"
                                       ,stdout=FALSE
                                       ,stderr=FALSE))
        ifelse(rc == 0, TRUE, FALSE)
      }
      ,checkDOT         = function() {
          ifelse (.Platform$OS.type == "windows"
                  ,private$checkDOTWin()
                  ,private$checkDOTUnix())
      }
      ,checkDOTWin      = function() {
          # Hay que procesar ProgramFiles y x86por separado,
          # por que los directorios pueden tener los mismos nombres

          if (private$checkDOTWinPF("ProgramFiles"))     return (TRUE)
          if (private$checkDOTWinPF("ProgramFiles(x86")) return (TRUE)
          file.exists(Sys.getenv("GRAPHVIZ_DOT"))
      }
      ,checkDOTWinPF    = function(programfiles) {
          pf = Sys.getenv(programfiles)
          dirs = list.files(pf)
          dirs = dirs[grepl("Graphviz.*", dirs, ignore.case = TRUE)]
          if (length(dirs) > 0) {
              dirs = paste(pf, dirs, "bin\\dot.exe", sep="\\")
              files = sapply(dirs, file.exists)
              if (sum(files) > 0) return (TRUE)
          }
          FALSE
      }
      ,checkDOTUnix     = function() {
          if (file.exists("/usr/local/bin/dot")) return (TRUE)
          if (file.exists("/usr/bin/dot"))       return (TRUE)
          file.exists(Sys.getenv("GRAPHVIZ_DOT"))
      }
      ,checkExecution   = function() {
          res = system2( self$getJVM()
                        ,c("-jar", self$getPlantUML(), "-testdot")
                        ,stdout=TRUE)
          if (length(res) != 2) return(FALSE)
          grepl("seems OK", res[2])
      }
      ,checkEnvironment = function() {
          file = basename(tempfile())
          link=NULL
          tryCatch ({
             link = self$image(private$testData)
             file.remove(strsplit(link, "[()]")[[1]][2])
           }
          ,error = function(e) return (FALSE)
          )
          TRUE
      }
      ,restoreForce     = function() {
         if (!is.null(private$oldForce)) {
             self$force = private$oldForce
             private$oldForce = NULL
         }
      }
      ,checkString     = function(value) {
        if (missing(value) || is.null(value))
          private$plantErr("R006", stack=1)
        if (!is.character(value) || length(value) != 1)
          private$plantErr("R006", stack=1)
        value
      }

   )
)
