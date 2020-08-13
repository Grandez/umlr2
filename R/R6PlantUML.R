#' La clase para interactuar con plantuml
#' @title R6PlantUML
#' @docType class
#' @description  La descripcion.
#'

# 1.- Recibe los datos
# 2.1.- Si es un fichero
#       2.1.1 chequea en inputDir y outputDir
#       2.1.2 Si generar
#             Copiar al temporal
#             Ejecutar
#             Copiar al destino
#             borrar
# 2.2 - Si es inline
#       2.2.1 - Tiene nombre? Puede repetirse
#               2.2.1.1 Verificar el md5 en temp
PLANTUML = R6::R6Class("R6PLANTUML",
   public = list(
       #' @description Crea una instancia de la clase PLANTUML
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       #'
       #' @examples
       #' plant = PLANTUML$new()
       #' plant = PLANTUML$new(jvm='java')
       #' plant = PLANTUML$new(c(jvm='java', plantuml='plantuml.jar'))
       initialize         = function( ...) {
          if (substr(as.character(sys.call(-1))[1], 1, 6) == "PLANTUML") private$plantErr("E900")
          parms = unlist(list(...))
          if (sum(names(parms) == "") > 0) private$plantErr("R103")
          self$setConfig(parms)
      }
      ,finalize = function() {
          message("Cleaning up clase base")
          files = gsub("\\..+$", "\\.\\*", private$files)
          lista = list.files(tempdir(), files, full.names=TRUE)
          file.remove(lista)
      }
      #' @description Si es posible, convierte los datos pasados en una clase PLANTUML
      #'              Usado desde la funcion generica as.plantuml. **No invocar directamente**
      #' @param umlData  Definicion del diagrama en formato texto
      #' @return una clase S3PlantUML
      ,asS3PlantUML       = function(umlData = NULL, name=NULL) {
          if (is.null(umlData)) private$plantErr("R010")
          txt = umlData
          if (is.list(txt)) txt = unlist(txt)
          if (!is.character(txt)) private$plantErr("R011")
          txt = private$removeUmlTags(txt)
          # if (length(txt) > 1) txt = paste(txt, collapse="\n")
          # if (length(grep("@startuml", txt, fixed = TRUE)) == 0) {
          #     txt = paste("@startuml \n", txt, "\n@enduml \n")
          # }
          if (length(names(umlData)) > 0) names(txt) = name
          if (!is.null(name))             names(txt) = name
          structure(txt, class="S3PlantUML")
      }
      #' @description
      #'     Verifica la corrección de los datos de configuración de la clase.
      #'     Se recomienda su uso en desarrollo para verificar que los valores son correctos
      #'     El que indique que la configuracion es correcta no implica que el sistema funcione
      #' @seealso [checkInstallation()] para un chequeo completo
      #' @family verificacion
      #' @param verbose  Si TRUE muestra informacion de progreso por la consola
      #' @param first    Si TRUE se detiene en el primer error
      #' @return TRUE si la configuracion es correcta
      #'         FALSE en caso contrario
      ,checkConfiguration = function(verbose=TRUE, first=FALSE) {
          rc = TRUE
          if (verbose) message(private$msgErr["I010"])
          if (verbose) message(private$msgErr["I011"], appendLF = FALSE)
          rp = (nchar(self$getJVM()) != 0)
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(private$msgErr["I012"], appendLF = FALSE)
          rp = (nchar(self$getPlantUML()) != 0)
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(private$msgErr["I013"], appendLF = FALSE)
          ext = self$getExt()
          rp = (nchar(ext) != 0)
          if (rp) rp = (substring(ext, 1, 1) != '.')
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(private$msgErr["I014"], appendLF = FALSE)
          ext = self$getExt()
          rp = (self$getType() %in% private$types)
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(private$msgErr["I015"], appendLF = FALSE)
          rp = (nchar(self$getCharset()) != 0)
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(private$msgErr["I016"], appendLF = FALSE)
          dd = self$getUmlDir()
          rp = (!is.null(dd) && dir.exists(dd))
          if (verbose) message(ifelse (rp, "OK", "KO"))
          rc = rc && rp
          if (first && !rc) return(rc)

          if (verbose) message(private$msgErr["I017"], appendLF = FALSE)
          dd = self$getOutputDir()
          rp = ifelse(is.null(dd), TRUE, dir.exists(dd))
          if (verbose) message(ifelse (rp, "OK", "KO"))
          return (rc && rp)
      }
      #' @description Verifica la correccion de la instalacion. Es decir, si todos los
      #' componentes estan correctamente configurados y pueden ser ejecutados
      #' @family verificacion
      #' @note
      #' Se recomienda su uso en desarrollo para verificar que los valores son correctos
      #' @param verbose  Si TRUE muestra informacion de progreso por la consola
      #' @param first    Si TRUE se detiene en el primer error
      #' @return TRUE si la configuracion es correcta
      #'         FALSE en caso contrario
      ,checkInstallation = function(verbose=TRUE, first=FALSE) {
          rc = self$checkConfiguration(verbose, first)
          if (first && !rc) return(rc)

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

          # Si ha fallado alguno anterior, fallaran estos
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
      #' @description Cambia los datos de configuracion de la instancia
      #' @family setters y getters
      #' @param ...  named values para definir la configuracion
      #' @return La instancia del objeto
      ,setConfig         = function(...) {
         values = unlist(list(...))
         if (length(values) == 0) return()
         if (!is.character(values[1])) private$plantErr("R106")
         parms = names(values)
         if (sum(parms == "") > 0) private$plantErr("R105")
         flags = names(values) %in% names(private$cfg)
         noFlags = names(values)[!flags]
         if (length(noFlags) > 0) private$plantErr("R104", noFlags)

         nm = names(values) # tipo
         eval(parse(text=paste0("private$cfg[\"", nm, "\"] = values[\"", nm, "\"]")))
         if (!is.na(values["type"])) private$checkType(values["type"])
         invisible(self)
      }
      #' @description Obtiene los datos de configuracion
      #' @family setters y getters
      #' @return Una lista con los datos de configuracion
      ,getConfig         = function() { private$cfg }

      #####################################################
      # Getters and setters
      #####################################################

      #' @description si activo no hace cache
      #' @return El estado del flag
      ,getForce      = function() private$force
      #' @description Devuelve el nombre de la maquina virtual java
      #' @return El nombre del ejecutable de la maquina virtual java
      ,getJVM       = function() private$cfg[["jvm"]]
      #' @description Devuelve la ubicacion del archivo jar plantuml.jar
      #' @family setters y getters
      #' @return La ubicacion del archivo jar plantuml.jar
      ,getPlantUML  = function() private$cfg[["plantuml"]]
      #' @description Devuelve la ubicacion del directorio por defecto de las definiciones de diagramas
      #' @return La ubicacion del directorio por defecto de las definiciones de diagramas
      ,getInputDir  = function(real = FALSE) {
           if (is.null(private$cfg[["inputDir"]]) && real) return(tempdir)
           private$cfg[["umlDir"]]
      }
      #' @description Devuelve la ubicacion del directorio por defecto de las imagenes de diagramas
      #' @return La ubicacion del directorio por defecto de las imagenes de diagramas
      ,getOutputDir  = function(real = FALSE) {
          if (is.null(private$cfg[["outputDir"]]) && real) return(tempdir)
          private$cfg[["outputDir"]]
      }
      #' @description Devuelve la extensión por defecto para almacenar los ficheros de diagramas
      #' @family setters y getters
      #' @return La extensión por defecto para almacenar los ficheros de diagramas
      ,getExt       = function() private$cfg[["ext"]]
      #' @description Devuelve el tipo de grafico por defecto
      #' @family setters y getters
      #' @return El tipo de grafico por defecto
      ,getType      = function() private$cfg[["type"]]
      #' @description Devuelve el juego de caracteres por defecto
      #' @family setters y getters
      #' @return El juego de caracteres por defecto
      ,getCharset   = function() private$cfg[["charset"]]
      ,setForce     = function(value) {
          private$force = as.logical(value)
          invisible(self)
      }
      #' @description Establece la maquina virtual Java a utilizar
      #' @param value nombre de la maquina virtual
      ,setJVM       = function(value) { private$cfg["jvm"]      = private$checkString(value); invisible(self) }
      #' @description Establece el paquete plantuml.jar a utilizar
      #' @param value el paquete plantuml.jar a utilizar
      ,setPlantUML  = function(value) { private$cfg["plantuml"] = private$checkString(value); invisible(self) }
      #' @description Establece la extension por defecto para los ficheros de definicion de diagramas
      #'              Se ignora si el fichero en si mismo tiene extension
      #' @param value El paquete plantuml.jar a utilizar
      ,setExt       = function(value) { private$cfg["ext"]      = private$checkString(value); invisible(self) }
      #' @description Establece el juego de caracteres por defecto
      #' @param value El juego de caracteres por defecto
      ,setCharset   = function(value) { private$cfg["charset"]  = private$checkString(value); invisible(self) }
      #' @description Establece el tipo de grafico por defecto
      #'              Valores permitidos: png, svg, png
      #' @param value El tipo de grafico por defecto
      ,setType      = function(value) {
        private$checkString(value)
        private$checkType(value)
        private$cfg["type"] = value
        invisible(self)
      }
      #' @description Establece directorio por defecto donde buscar las definiciones de diagramas
      #'              Puede ser relativo o absoluto
      #' @param value El directorio por defecto donde buscar los ficheros de definiciones
      ,setInputDir  = function(value=NULL) {
          private$cfg["inputDir"] = ifelse(is.null(value), NULL, private$checkString(value))
          invisible(self)
      }
      #' @description Establece directorio donde guardar los diagramas
      #'              Puede ser relativo o absoluto
      #' @param value El directorio donde guardar los ficheros de definiciones
      ,setOutputDir  = function(value=NULL) {
        private$cfg["outputDir"] = ifelse(is.null(value), NULL, private$checkString(value))
        invisible(self)
      }
   )
   ,private = list(
       inline = FALSE        # Mark data as inline
      ,files  = NULL         # Files generated
      ,res    = NULL         # Messages from plantuml
      ,force  = FALSE        # Recreate diagrams
      ,cfg=list( jvm        = "java"
                 ,plantuml  = "plantuml.jar"
                 ,ext       = "uml"
                 ,type      = "png"
                 ,charset   = "utf-8"
                 ,inputDir  = NULL
                 ,outputDir = NULL
      )
      ,types  = c("png", "jpg", "svg")
      ,msgErr = c(
         R001="Invalid value for %s"
        ,R002="Invalid directory: %s"
        ,R003="Invalid parameter: %s"
        ,R005="Invalid flag: %s"
        ,R006="Invalid or missing value"
        ,R010="Invalid call. Parameter missing"
        ,R011="Data provided can not be casted to S3PlantUML class"
        ,R101="JVM not found: %s"
        ,R102="Component not found: %s"
        ,R103="All values must be named"
        ,R104="Incorrect configuration names: %s"
        ,R105="All configuration parameters must be named"
        ,R106="COnfiguration parameters must be an string"
        ,R107="Invalid graphic format. Only png, jpg or svg are allowed"
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
        ,I010="Checking configuration values:"
        ,I011="\tJVM\t\t\t"
        ,I012="\tPlantUML\t\t"
        ,I013="\tInput extension\t\t"
        ,I014="\tGraph type\t\t"
        ,I015="\tCharset\t\t\t"
        ,I016="\tInput directory\t\t"
        ,I017="\tOutput directory\t"
        ,E001="Error generating diagram"
        ,E101="Invalid file name: %s"
        ,E102="Reading file name: %s"
        ,E900="This class is abstract"
      )
      ,testData="@startuml
                    [*]-->STATE1
                 @enduml"
      ,restoreForce     = function() {
         # if (!is.null(private$oldForce)) {
         #     self$force = private$oldForce
         #     private$oldForce = NULL
         # }
      }
      ,checkType        = function (type) {
        if (!(type %in% private$types)) private$plantErr("R107")
      }
      ,parseS4 = function (object, full=FALSE, deep = FALSE) {
      }
      ,parseR6 = function (object, full=FALSE, deep = FALSE) {
           #uml = ""
           lista = list()
           defs = private$getGenerators()  # Generadores
           classes = class(object)
           classes = classes[classes != "R6"]
           inherits = private$getExtends(classes, defs, deep)
           classBase = private$UMLClass(classes[1], full, defs)
           if (deep && length(classes) > 1) {
               lista = sapply(classes[-1], function(x) private$UMLClass(x, full, defs))
           }
           #uml = c(uml, paste("class", class(object)[1], "{"))
           #data = capture.output(object)
           #data = trimws(data)
           c(classBase, unlist(lista), inherits)
      }
      ,getGenerators = function () {
          vars = ls(globalenv())
          classes = lapply(vars, function(x) eval(parse(text=paste0("class(",x,")"))))
          names(classes) = vars
          classes = unlist(classes)
          c2 = classes[classes == "R6ClassGenerator"]
          # Obtenemos la clase que genera
          c2 = sapply(names(c2), function(x) eval(parse(text=paste0(x, "$classname"))), USE.NAMES=T)
          # Cambiamos nombres por valores
          nm = names(c2)
          names(nm) = c2
          nm
      }
      ,getExtends        = function (classes, defs, deep) {
          if (length(classes) == 1) return ("")
          cNames = defs[classes]
          extend = c(paste(cNames[classes[2]], "<|--", cNames[classes[1]]))
          if (deep && length(classes) > 2) {
             rr = sapply(seq(2, length(classes) - 1), function(x)
                    c(paste(cNames[classes[x+1]], "<|--", cNames[classes[x]])))
          }
          # if (!deep && length(classes) > 1) return (c(paste(classes[1], "<|--", classes[2])))
          # for (i in 1:length(classes) - 1) {
          #     extend = c(extend, paste(classes[i], "<|--", classes[i+1]))
          # }
          extend
      }
      ,UMLClass          = function (className, full, generators) {
          attrPrivate = NULL
          uml = c(paste("class", generators[className], "<<", className, ">> {"))
          attrPublic  = private$getAttrs(generators[className], "public")
          if (full) attrPrivate  = private$getAttrs(generators[className], "private")
          if (!is.null(attrPublic$fields))   uml = c(uml, paste("+{field}",  attrPublic$fields))
          if (!is.null(attrPrivate$fields))  uml = c(uml, paste("-{field}",  attrPrivate$fields))
          if (!is.null(attrPublic$methods))  uml = c(uml, paste("+{method}", attrPublic$methods))
          if (!is.null(attrPrivate$methods)) uml = c(uml, paste("-{method}", attrPrivate$methods))
          c(uml, "}")
      }
      ,getAttrs  = function (className, visibility) {
          data = list(fields = NULL, methods = NULL)
          lFields  = eval(parse(text=paste0(className, "$", visibility, "_fields")))
          lMethods = eval(parse(text=paste0(className, "$", visibility, "_methods")))
          list(fields=names(lFields), methods=names(lMethods))
      }
      ,parseR62          = function (object, full=FALSE, deep = FALSE) {
          priv = ""
          pub = ""
          uml = ""
          data = capture.output(object)
          data = trimws(data)
          inherit = grep("Inherits[ \t]*", data, ignore.case = TRUE, value = TRUE)
          if (length(inherit) > 0) {
              uml = private$parseR6Parent(inherit)
          }
          uml = c(uml, paste("class", class(object)[1], "{"))
          idxPublic  = grep("Public:" , data, ignore.case = TRUE)
          idxPrivate = grep("Private:", data, ignore.case = TRUE)
          if (idxPublic != 0) {
              max = ifelse(idxPrivate > 0, idxPrivate - 1, length(data))
              pub = private$extract(data[c((idxPublic + 1):max)],"+")
          }
          if (idxPrivate != 0 && full) {
              priv = "__ Private Data __"
              priv = c(priv, private$extract(data[c((idxPrivate + 1):length(data))],"-"))
          }
          uml = c(uml, pub, priv)
          structure(c(uml, "}"), class="S3PlantUML")
      }
      ,extract          = function(data, type) {
          attrs = grep("function", data, fixed=TRUE, value=TRUE, invert=TRUE)
          attrs = gsub(":.*", "", attrs)

          defs = grep("function", data, fixed=TRUE, value=TRUE)
          defs = gsub(": function", "", defs)
          c(paste0(type, attrs), "--", paste0(type, defs))
      }
      ,parseR6Parent    = function(txtParent) {
          parent = trimws(gsub(">", "", gsub(".*<", "", txtParent)))
          c(paste("class", parent, "{"), "}")
          vars = ls()
          classes = lapply(vars, function(x) eval(parse(text=paste0("class(",x,")"))))
          names(classes) = vars

      }
      ##############################################################################
      ##### CHECKED
      ##############################################################################
      ,plantErr         = function(code, ..., newCode=0) {
        if (newCode != 0) {
          text = sprintf("PUMLE%03d - %s", newCode, private$msgErr[code])
        }
        else {
          msg = sprintf(private$msgErr[code], ...)
          text = sprintf("PUML%s - %s", code, msg)
        }
        c <- errorCondition(text, class=c("plantUMLErr", "error"))
        stop(c)
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
          if (!private$inline) {
              f = file.path(tempdir(), basename(umlFile))
              file.copy(umlFile, f)
          }
          private$files = c(private$files, basename(f))
          # Si la llamada es correcta no informa status
          private$res = suppressWarnings( system2( self$getJVM()
                                          ,c("-jar", self$getPlantUML()
                                          ,"-tpng"
                                          ,"-verbose"
                                          ,f), stdout=TRUE, stderr=TRUE)
          )
          rc = ifelse(is.null(attr(private$res, "status")), 0, attr(private$res, "status"))
          if (rc != 0) private$plantErr("E001", newCode=rc)
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
          inFile = paste(inFile, self$getExt(), sep=".")
        }

        inFile = file.path(self$getInputDir(), inFile)
        if (!file.exists(inFile)) private$plantErr("R205")
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
      ,removeUmlTags   = function (data) {
          gsub("@startuml | @enduml", "", data, fixed=TRUE)
          # browser()
          # # Es mas rapido obtener el indice que hacer una sustitucion
          # pos = grep("@startuml", data)
          # if (pos > 0) data[pos] = ""
          # pos = grep("@enduml", data)
          # if (pos > 0) data[pos] = ""
          # data
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
          link = self$image(private$testData)
          file.remove(strsplit(link, "[()]")[[1]][2])
        }
        ,error = function(e) return (FALSE)
        )
        TRUE
      }
      ,checkString      = function(value) {
          if (missing(value) || is.null(value))
              private$plantErr("R006")
          if (!is.character(value) || length(value) != 1)
              private$plantErr("R006")
          trimmed = gsub("[[:space:]]", "", value)
          if (nchar(trimmed) == 0)
              private$plantErr("R006")
           trimmed
      }

   )
)
