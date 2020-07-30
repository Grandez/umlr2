library(magick)
#' An example R6 class
#'
#' @docType class
#' @export
#' @format An \code{R6Class} generator object
#' @keywords "plantuml"
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(...)}}{
#'      Crea un objeto PLANTUML
#'      Acepta un conjunto de parametros nombre=valor_cadena donde nombre
#'      identifica un parametro de configuracion
#'   }
#' }
#'  An example R6 class
#'
PLANTUML = R6::R6Class("PLANTUML",
                       public = list(
                         force = FALSE
                         ,initialize = function( ...) {
                           parms = unlist(list(...))
                           if (sum(names(parms) == "") > 0) private$plantErr("R103")
                           self$setConfig(parms)
                         }
                         #' @description plot the diagram
                         #' @param data una definicion del diagrama o un fichero con la misma
                         #' @param force Ver  \code{vignette("Caching diagrams")}
                         #'
                         #' @details
                         #'     La funcion identifica el tipo de informcion pasada en base a la
                         #'     siguiente logica:
                         #'   \enumerate{
                         #'      \item Si data empieza por "/" o "x:" se considera un fichero absoluto
                         #'      \item Si data contiene espacios o retornos de carro se considera dato
                         #'      \item Si no se considera un fichero
                         #'   }
                         #'
                         #' @examples
                         #' \dontrun {
                         #'    plant = PLANTUML$new()
                         #'    data="@startuml
                         #'                [*]-->STATE1
                         #'              @enduml
                         #'             "
                         #'    plant$plot(data)
                         #'    plant$plot("fileName")
                         #'    plant$plot("/home/absolute_file_name")
                         #' }
                         ,plot  = function(data=NULL, force=NULL) {
                           imgFile = private$genDiagram(data)
                           image = magick::image_read(imgFile)
                           file.remove(imgFile)
                           plot(as.raster(image))
                         }
                         #' @description Genera un link a la imagen en fichero
                         #' @inheritParams plot
                         #' @param caption El texto del link
                         ,image             = function(data=NULL, caption=NULL, force=NULL) {
                           if (is.null(private$config$getoutputDir())) {
                             private$plantErr("R202")
                           }
                           if (!is.null(force)) {
                             private$oldForce = self$force
                             self$force = force
                           }
                           outFile = private$genDiagram(data)
                           paste0('![', caption, '](', outFile, ')')
                         }
                         # Generar todos los diagramas en el directorio
                         ,bulk              = function() {
                           if (is.null(private$config$getoutputDir())) {
                             private$plantErr("R202")
                           }
                         }
                         ,setConfig = function(values) {
                           flags = names(values) %in% names(private$cfg)
                           noFlags = names(values)[!flags]
                           if (length(noFlags) > 0) private$plantErr("R104", noFlags)
                           nm = names(values) # tipo
                           eval(parse(text=paste0("private$cfg[\"", nm, "\"] = values[\"", nm, "\"]")))
                         }
                         ,getConfig         = function() { private$cfg }
                         ,checkInstallation = function(verbose=TRUE) {
                           rc = TRUE
                           if (verbose) message(private$msgErr["I001"], appendLF = FALSE)
                           rp = private$checkJVM()
                           if (verbose) message(ifelse (rp, "OK", "KO"))
                           rc = rc && rp
                           if (verbose) message(private$msgErr["I002"], appendLF = FALSE)
                           rp = private$checkDOT()
                           if (verbose) message(ifelse (rp, "OK", "KO"))
                           rc = rc && rp
                           if (verbose) message(private$msgErr["I003"], appendLF = FALSE)
                           rp = file.exists(private$config$getPlantUML())
                           if (verbose) message(ifelse (rp, "OK", "KO"))
                           rc = rc && rp

                           if (rc) {
                             if (verbose) message(private$msgErr["I004"], appendLF = FALSE)
                             rp = private$checkExecution()
                             if (verbose) message(ifelse (rp, "OK", "KO"))
                             if (verbose) message(private$msgErr["I005"], appendLF = FALSE)
                             rp = private$checkEnvironment()
                             if (verbose) message(ifelse (rp, "OK", "KO"))
                           }
                           rc && rp
                         }
                         # Setters and getters
                         # Getters and setters
                         ,getJVM       = function() private$cfg["jvm"]
                         ,getPlantUML  = function() private$cfg["plantuml"]
                         ,getInputDir  = function() private$cfg["inputDir"]
                         ,getOutputDir = function() private$cfg["outputDir"]
                         ,getExt       = function() private$cfg["ext"]
                         ,getType      = function() private$cfg["type"]
                         ,getCharset   = function() private$cfg["charset"]
                         ,setJVM       = function(value) { private$check(value); private$cfg["jvm"]       = value }
                         ,setPlantUML  = function(value) { private$check(value); private$cfg["plantuml"] = value }
                         ,setInputDir  = function(value) { private$check(value); private$cfg["inputDir"] = value }
                         ,setOutputDir = function(value) { private$check(value); private$cfg["outputDir"] = value }
                         ,SetExt       = function(value) { private$check(value); private$cfg["ext"]      = value }
                         ,setType      = function(value) { private$check(value); private$cfg["type"]   = value }
                         ,setCharset   = function(value) { private$check(value); private$cfg["charset"]   = value }
                       )
                       ,private = list(
                         config = NULL
                         ,oldForce = NULL
                         ,cfg=list( jvm       = "java"
                                    ,plantuml  = "plantuml.jar"
                                    ,ext       = "uml"
                                    ,type      = "png"
                                    ,charset   = "utf-8"
                                    ,inputDir  = NULL
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
                           ,I001="Checking JVM machine \t"
                           ,I002="Checking Graphviz \t"
                           ,I003="Checking plantuml.jar \t"
                           ,I004="Checking execution \t"
                           ,I005="Checking environment \t"
                           ,E001="Error generating diagram: %d"
                         )
                         ,plantErr = function(code, ..., newCode=0,stack=0) {
                           msg = sprintf(private$msgErr[code], ...)
                           if (newCode != 0) code = sprintf("E%03d", newCode)
                           text = sprintf("\nPU%s - %s", code, msg)
                           c <- errorCondition(text, class=c("plantUMLErr", "error"),
                                               call = sys.call(-(1 + stack), ...))
                           stop(c)
                         }

                         ,genDiagram       = function(data) {
                           temporal=FALSE
                           if (is.null(data)) private$plantErr("R201")
                           if (private$dataInline(data)) {
                             umlFile = private$prepareTempFile(data)
                             temporal=TRUE
                           }
                           else {
                             umlFile = private$mountInputFile(data)
                             if (!self$force) {

                             }
                           }

                           rc = private$callPlantUML(umlFile)
                           if (rc != 0) {
                             private$restoreForce()
                             private$plantErr("E001", rc)
                           }
                           if (temporal) file.remove(umlFile)
                           private$mountOutputFile(umlFile)
                         }
                         ,callPlantUML     = function(umlFile) {
                           cfg = private$config
                           # Si correcto no devuelve nada
                           # attr(res, "status")
                           res = system2( cfg$getJVM()
                                          ,c("-jar", cfg$getPlantUML()
                                             ,paste0("-t", cfg$getType())
                                             ,"-verbose"
                                             ,"-o", normalizePath(cfg$getOutputDir())
                                             ,umlFile), stdout=TRUE, stderr=TRUE)
                           ifelse(is.null(attr(res, "status")), 0, attr(res, "status"))
                         }
                         ,validate         = function(parm, name) {
                           if (!is.null(parm) && (!is.character(parm) || length(parm) != 1)) {
                             private$plantErr("R001", name)
                           }
                         }
                         ,validateDir      = function(dir, txt) {
                           if (!dir.exists(dir)) private$plantErr("R002", txt)
                         }
                         ,checkJVM         = function() {
                           rc = suppressWarnings(system2( private$config$getJVM()
                                                          ,"-version"
                                                          ,stdout=FALSE
                                                          ,stderr=FALSE))
                           ifelse(rc == 0, TRUE, FALSE)
                         }
                         ,checkDOT         = function() {
                           res = FALSE
                           if (.Platform$OS.type == "windows") {
                             res = private$checkDOTWin()
                           }
                           else {
                             res = private$checkDOTUnix()
                           }
                           res
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
                           res = system2( private$config$getJVM()
                                          ,c("-jar", private$config$getPlantUML(), "-testdot")
                                          ,stdout=TRUE)
                           if (length(res) != 2) return(FALSE)
                           grepl("seems OK", res[2])
                         }
                         ,checkEnvironment = function() {
                           testData="@startuml
                        [*]-->STATE1
                        @enduml
                       "
                           file = basename(tempfile())
                           link=NULL
                           tryCatch ({
                             link = self$image(testData)
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
                         ,dataInline       = function(data) {
                           words = strsplit(data, " ")
                           if (length(words[[1]]) > 1) return (TRUE)
                           words = strsplit(data, "\n")
                           if (length(words[[1]]) > 1) return (TRUE)
                           FALSE
                         }
                         ,mountInputFile   = function(filename) {
                           # Check for absolute
                           if (substring(filename, 1,1) == "/") return(filename)
                           if (nchar(filename) > 1 && substring(filename, 2,1) == ":")
                             return(filename)

                           tmpFile = filename
                           if (which(tmpFile, ".") == 0 ) {
                             tmpFile = paste(tmpFile, private$config$getExt(), sep=".")
                           }

                           file.path(private$config$getInDir(), tmpFile)
                         }
                         ,mountOutputFile  = function(umlFile) {
                           ff = strsplit(basename(umlFile), ".", fixed=TRUE)[[1]][1]

                           # hasExtension?
                           if (length(strsplit(basename(filename), ".", fixed=TRUE)[[1]]) == 1) {
                             ff = paste(ff, private$config$getType(), sep=".")
                           }
                           oDir = private$config$getOutputDir()
                           if (!is.null(oDir) && oDir != ".") ff = file.path(oDir, ff)
                           # Poner el directorio
                           ff
                         }
                         ,prepareTempFile  = function(data) {
                           tmpFile = paste0(tempfile(), ".uml")
                           write(data, tmpFile)
                           tmpFile
                         }
                         ,check     = function(value) {
                           if (missing(value) || is.null(value))
                             private$plantErr("R006", stack=1)
                           if (!is.character(value) || length(value) != 1)
                             private$plantErr("R006", stack=1)
                         }

                       )
)
