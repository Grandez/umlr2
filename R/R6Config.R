#' La clase para interactuar con plantuml
#' @name Config
#' @title R6Config
#' @rdname Config
#' @docType class
#' @description  La descripcion.
#' @export
CONFIG = R6::R6Class("R6CONFIG"
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
       #' @description Crea una instancia de la clase
       #' @details     Esta clase no puede ser instanciada
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       initialize         = function( ...) {
#          if (substr(as.character(sys.call(-1))[1], 1, 6) == "CONFIG") msg$err("E900", "CONFIG")
         # msg = umlr2.env$UMLR2Msg
          setConfig(...)
      }
      #' @description
      #'     Verifica la corrección de los datos de configuración de la clase.
      #'     Se recomienda su uso en desarrollo para verificar que los valores son correctos
      #'     El que indique que la configuracion es correcta no implica que el sistema funcione
      #' @param verbose  Si TRUE muestra informacion de progreso por la consola
      #' @param first    Si TRUE se detiene en el primer error
      #' @return TRUE si la configuracion es correcta
      #'         FALSE en caso contrario
      ,checkConfiguration = function(verbose=TRUE, first=FALSE) {
          txt = ""
          rc = TRUE
          if (verbose) message(msg$msg("I010"))
          if (verbose) message(msg$msg("I011"), appendLF = FALSE)
          rp = (nchar(self$getJVM()) != 0)
          if (verbose) message(paste(msg$ok(rp), "-", self$getJVM()))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I012"), appendLF = FALSE)
          rp = (nchar(self$getPlantUML()) != 0)
          if (verbose) message(paste(msg$ok(rp), "-", self$getPlantUML()))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I013"), appendLF = FALSE)
          ext = self$getExt()
          rp = (nchar(ext) != 0)
          if (rp) rp = (substring(ext, 1, 1) != '.')
          if (verbose) message(paste(msg$ok(rp), "-", self$getExt()))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I014"), appendLF = FALSE)
          rp = (self$getType() %in% private$types)
          if (verbose) message(paste(msg$ok(rp), "-", self$getType()))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I015"), appendLF = FALSE)
          rp = (nchar(self$getCharset()) != 0)
          if (verbose) message(paste(msg$ok(rp), "-", self$getCharset()))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I016"), appendLF = FALSE)
          dd = self$getInputDir()
          txt = ifelse(is.null(dd) || nchar(dd) == 0, "Not set", dd)
          rp  = ifelse(is.null(dd) || nchar(dd) == 0, TRUE, dir.exists(dd))

          if (verbose) message(paste(msg$ok(rp), "-", txt))
          txt = ""
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I017"), appendLF = FALSE)
          dd = self$getOutputDir()
          txt = ifelse(is.null(dd) || nchar(dd) == 0, "Not set", dd)
          rp = ifelse(is.null(dd) || nchar(dd) == 0, TRUE, dir.exists(dd))
          if (verbose) message(paste(msg$ok(rp), "-", txt))
          return (rc && rp)
      }
      #' @description Verifica la correccion de la instalacion. Es decir, si todos los
      #' componentes estan correctamente configurados y pueden ser ejecutados
      #' @note
      #' Se recomienda su uso en desarrollo para verificar que los valores son correctos
      #' @param verbose  Si TRUE muestra informacion de progreso por la consola
      #' @param first    Si TRUE se detiene en el primer error
      #' @return TRUE si la configuracion es correcta
      #'         FALSE en caso contrario
      ,checkInstallation = function(verbose=TRUE, first=FALSE) {
          rc = self$checkConfiguration(verbose, first)
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I001"), appendLF = FALSE)
          rp = private$checkJVM()
          if (verbose) message(msg$ok(rp))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I002"), appendLF = FALSE)
          rp = private$checkDOT()
          if (verbose) message(msg$ok(rp))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(msg$msg("I003"), appendLF = FALSE)
          rp = file.exists(self$getPlantUML())
          if (verbose) message(msg$ok(rp))
          rc = rc && rp
          if (first && !rc) return(rc)

          # Si ha fallado alguno anterior, fallaran estos
          if (rc) {
              if (verbose) message(msg$msg("I004"), appendLF = FALSE)
              rp = private$checkExecution()
              if (verbose) message(msg$ok(rp))
              rc = rc && rp
              if (first && !rc) return(rc)
              if (verbose) message(msg$msg("I005"), appendLF = FALSE)
              rp = private$checkEnvironment()
              if (verbose) message(msg$ok(rp))
          }
          rc && rp
      }
      #' @description Cambia los datos de configuracion de la instancia
      #' @param ...  named values para definir la configuracion
      #' @return La instancia del objeto
      ,setConfig         = function(...) {
          values = unlist(list(...))
          if (length(values) == 0) return(invisible(self))
          if (sum(names(values) == "") > 0) msg$err("E004")
          if (!is.na(values["config"])) {
              from = values["config"]
              private$cfg = from$getConfig()
              pos = match("config", names(values))
              values = values[-pos]
         }
         if (length(values) == 0) return()
         flags = names(values) %in% names(private$cfg)
         noFlags = names(values)[!flags]
         if (length(noFlags) > 0) msg$err("E104", noFlags)

         nm = names(values) # tipo
         eval(parse(text=paste0("private$cfg[\"", nm, "\"] = values[\"", nm, "\"]")))
         if (!is.na(values["type"])) private$checkType(values["type"])
         invisible(self)
      }
      #' @description Obtiene los datos de configuracion
      #' @return Una lista con los datos de configuracion
      ,getConfig         = function() { cfg }
      #####################################################
      # Getters and setters
      #####################################################

      #' @description si activo no hace cache
      #' @return El estado del flag
      ,getForce      = function() force
      #' @description Devuelve el nombre de la maquina virtual java
      #' @return El nombre del ejecutable de la maquina virtual java
      ,getJVM       = function() cfg[["jvm"]]
      #' @description Devuelve la ubicacion del archivo jar plantuml.jar
      #' @family setters y getters
      #' @return La ubicacion del archivo jar plantuml.jar
      ,getPlantUML  = function() cfg[["plantuml"]]
      #' @description Devuelve la ubicacion del directorio por defecto de las definiciones de diagramas
      #' @param real Si TRUE devuelve el valor real del directorio, si no el indicado (puede ser NULL)
      #' @return La ubicacion del directorio por defecto de las definiciones de diagramas
      ,getInputDir  = function(real = FALSE) {
           if (nchar(private$cfg[["inputDir"]]) > 0) return(cfg[["inputDir"]])
           if (real) return(tempdir)
           NULL
      }
      #' @description Devuelve la ubicacion del directorio por defecto de las imagenes de diagramas
      #' @param real Si TRUE devuelve el valor real del directorio, si no el indicado (puede ser NULL)
      #' @return La ubicacion del directorio por defecto de las imagenes de diagramas
      ,getOutputDir  = function(real = FALSE) {
          if (nchar(private$cfg[["outputDir"]]) > 0) return(cfg[["outputDir"]])
          if (real) return(tempdir)
          NULL
      }
      #' @description Devuelve la extensión por defecto para almacenar los ficheros de diagramas
      #' @family setters y getters
      #' @return La extensión por defecto para almacenar los ficheros de diagramas
      ,getExt       = function() cfg[["ext"]]
      #' @description Devuelve el tipo de grafico por defecto
      #' @family setters y getters
      #' @return El tipo de grafico por defecto
      ,getType      = function() cfg[["type"]]
      #' @description Devuelve el juego de caracteres por defecto
      #' @family setters y getters
      #' @return El juego de caracteres por defecto
      ,getCharset   = function() cfg[["charset"]]
      ,getDetail    = function() cfg[["detail"]]
      ,getDeep      = function() cfg[["deep"]]
      #' @description Establece la maquina virtual Java a utilizar
      #' @param value nombre de la maquina virtual
      ,setJVM       = function(value) { private$cfg["jvm"]      = checkString(value); invisible(self) }
      #' @description Establece el paquete plantuml.jar a utilizar
      #' @param value el paquete plantuml.jar a utilizar
      ,setPlantUML  = function(value) { private$cfg["plantuml"] = checkString(value); invisible(self) }
      #' @description Establece la extension por defecto para los ficheros de definicion de diagramas
      #'              Se ignora si el fichero en si mismo tiene extension
      #' @param value El paquete plantuml.jar a utilizar
      ,setExt       = function(value) { private$cfg["ext"]      = checkString(value); invisible(self) }
      #' @description Establece el juego de caracteres por defecto
      #' @param value El juego de caracteres por defecto
      ,setCharset   = function(value) { private$cfg["charset"]  = checkString(value); invisible(self) }
      #' @description Establece el tipo de grafico por defecto
      #'              Valores permitidos: png, svg, png
      #' @param value El tipo de grafico por defecto
      ,setType      = function(value) {
        checkString(value)
        checkType(value)
        private$cfg["type"] = value
        invisible(self)
      }
      #' @description Establece directorio por defecto donde buscar las definiciones de diagramas
      #'              Puede ser relativo o absoluto
      #' @param value El directorio por defecto donde buscar los ficheros de definiciones
      ,setInputDir  = function(value=NULL) {
          private$cfg["inputDir"] = ifelse(is.null(value), "", checkString(value))
          invisible(self)
      }
      #' @description Establece directorio donde guardar los diagramas
      #'              Puede ser relativo o absoluto
      #' @param value El directorio donde guardar los ficheros de definiciones
      ,setOutputDir  = function(value=NULL) {
        private$cfg["outputDir"] = ifelse(is.null(value), "", checkString(value))
        invisible(self)
      }
      ,setDetail  = function(value=NULL) {
        private$cfg["detail"] = checkNumber(value)
        invisible(self)
      }
      ,setDeep  = function(value=NULL) {
        private$cfg["deep"] = checkNumber(value)
        invisible(self)
      }
   )
   ,private = list(
       cfg=list( jvm        = "java"
                 ,plantuml  = "c:/SDK/plantuml/plantuml.jar"
                 ,ext       = "uml"
                 ,type      = "png"
                 ,charset   = "utf-8"
                 ,inputDir  = ""    # Evitar Nulos
                 ,outputDir = ""    # Evitar Nulos
                 ,detail    = 1
                 ,deep      = 1
      )
      ,force = FALSE
      ,types  = c("png", "jpg", "svg")
      ,msg = UMLR2MSG$new()
      ,copyConfig = function(config) {
        private$cfg = config$getConfig()
      }
      ,checkType        = function (type) {
        if (!(type %in% private$types)) msg$err("R107")
      }
      #############################################################
      ### Checkers
      #############################################################
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
          writeLines(c("Test 1", "Test 2"), file)
          if (!file.exists(file)) return (FALSE)
          file.remove(file)
        }
        ,error = function(e) return (FALSE)
        )
        TRUE
      }
      ,checkString      = function(value) {
          if (missing(value) || is.null(value))           msg$err("E006")
          if (!is.character(value) || length(value) != 1) msg$err("E006")
          trimmed = gsub("[[:space:]]", "", value)
          if (nchar(trimmed) == 0)                        msg$err("E006")
           trimmed
      }
      ,checkNumber      = function(value) {
          if (missing(value) || is.null(value)) msg$err("E006")
          if (!is.numeric(value))               msg$err("E006")
      }

   )
)
