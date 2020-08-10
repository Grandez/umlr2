#' La clase para interactuar con plantuml
#' @title R6PlantUML
#' @docType class
#' @description  La descripcion.
#' @import R6
#' @export
# #' @import magick
#'
library(R6)
PLANTUML = R6::R6Class("R6PLANTUML",
   public = list(
       #' @field force Flag para forzar la recreación de diagramas
       force = FALSE

       #####
       #' @description Crea una instancia de la clase PLANTUML
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       #'
       #' @examples
       #' plant = PLANTUML$new()
       #' plant = PLANTUML$new(jvm='java')
       #' plant = PLANTUML$new(c(jvm='java', plantuml='plantuml.jar'))
      ,initialize         = function( ...) {
          parms = unlist(list(...))
          if (sum(names(parms) == "") > 0) private$plantErr("R103")
          self$setConfig(parms)
      }
      ,finalize = function() {
          message("Cleaning up ")
      }
      #' @description Genera un diagrama a partir de la definición pasada
      #' @param data  Definición del diagrama o fichero con la misma
      #' @details
      #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
      #'     - El fichero con la imagen no se almacena en el sistema de archivos
      ,plot               = function(data=NULL) {
          imgFile = private$makeImage(data, self$getType())
          knitr::include_graphics(imgFile)
      }
      #' @description Genera un link al fichero de imagen con el diagrama
      #' @seealso [plot()] para generacion en linea
      #' @param data  Definicion del diagrama o fichero con la misma
      #' @param caption Titulo de la imagen
      #' @return la cadena del link
      ,link              = function(data = NULL, caption = NULL) {
        imgFile = private$makeImage(data, type)
        target  = file.path(self$getOutputDir(), basename(imgFile))
        paste0('[![', caption, "](", target, " \'", caption, "\')](https://127.0.0.1)")
      }
      #' @description Almacena el fichero de la imagen en el sistema de archivos
      #' @seealso [plot()] para generacion en linea
      #' @param data  Definicion del diagrama o fichero con la misma
      #' @param caption Titulo de la imagen
      #' @param force   Fuerza a regenerar el fichero
      #' @return la cadena del link
      ,file               = function(data=NULL, caption=NULL, force=NULL) {
        private$makeImage(data, self$getType())
      }
      #' @description Carga un fichero de definicion de diagrama como clase S3PlantUML
      #' @family generics
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

      #' @description Devuelve un vector con el código de definición del diagrama
      #' @param data  Definición del diagrama o fichero con la misma
      #' @return el codigo de definición del diagrama
      ,getDefinition = function(data=NULL) {
          if (private$dataInline(data)) {
              def = private$prepareData(data)
              return (private$removeUmlTags(data))
          }
          self$load(data)
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
      #' @description Genera el diagrama de la clase pasada
      ,plotClass             = function (object, full=FALSE, deep = FALSE) {
          self$plot(self$umlClass(object,full, deep))
      }
      ,umlClass             = function (object, full=FALSE, deep = FALSE) {
          if (isS4(object))            return (private$parseS4(object, full, deep))
          if ("R6" %in% class(object)) return (private$parseR6(object, full, deep))
          warning("'object' is not an instance of S4 or R6 Classes"))
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

      #' @description Devuelve el nombre de la maquina virtual java
      #' @family setters y getters
      #' @return El nombre del ejecutable de la maquina virtual java
      ,getJVM       = function() private$cfg[["jvm"]]
      #' @description Devuelve la ubicacion del archivo jar plantuml.jar
      #' @family setters y getters
      #' @return La ubicacion del archivo jar plantuml.jar
      ,getPlantUML  = function() private$cfg[["plantuml"]]
      #' @description Devuelve la ubicacion del directorio por defecto de las definiciones de diagramas
      #' @family setters y getters
      #' @return La ubicacion del directorio por defecto de las definiciones de diagramas
      ,getUmlDir  = function(real = FALSE) {
          if (!is.null(private$cfg[["umlDir"]]) || !real) return (private$cfg[["umlDir"]])
          tempdir()
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
      #' @description Establece directorio por defecto donde buscar los ficheros de diagramas
      #'              Puede ser relativo o absoluto
      #' @param value El directorio por defecto donde buscar los ficheros de diagramas
      ,setUmlDir  = function(value=NULL) {
          private$cfg["umlDir"] = ifelse(is.null(value), NULL, private$checkString(value))
          invisible(self)
       }
   )
   ,private = list(
       cfg=list( jvm       = "java"
                 ,plantuml = "plantuml.jar"
                 ,ext      = "uml"
                 ,type     = "png"
                 ,charset  = "utf-8"
                 ,umldir   = NULL
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
      )
      ,testData="@startuml
                    [*]-->STATE1
                 @enduml"
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
      ,makeImage        = function(data,type) {
          umlFile = private$mountInputFile(data)
          imgFile = gsub("\\..+$", paste0(".", type), umlFile)
          private$callPlantUML(umlFile, type)
          imgFile
      }
      ,callPlantUML     = function(umlFile, type) {
          # Si la llamada es correcta no informa status
          res = suppressWarnings( system2( self$getJVM()
                        ,c("-jar", self$getPlantUML()
                        ,paste0("-t", type)
                        # ,"-o", normalizePath(outDir)
                        ,umlFile), stdout=TRUE, stderr=TRUE)
                )
          rc = ifelse(is.null(attr(res, "status")), 0, attr(res, "status"))
          if (rc != 0) {
              private$plantErr("E001", newCode=rc)
          }
      }
      ,mountInputFile   = function(data) {
          inDir = self$getUmlDir(TRUE)

          if (is.null(data)) private$plantErr("R201")
          if (private$dataInline(data)) {
              if (!is.null(names(data))) inFile = names(data)
              if ( is.null(names(data))) inFile = basename(tempfile("pumld"))
              inFile = file.path(inDir, inFile)
              return (private$prepareFile(data, paste0(inFile, ".", self$getExt())))
          }

          inFile = data
          # Check for absolute path in nix and windows
          if (substring(inFile, 1,1) == "/") return(data)
          if (nchar(inFile) > 1 && substring(inFile, 2,2) == ":") return(data)

          if (length(strsplit(inFile, ".",fixed=TRUE)[[1]]) == 1 ) {
              inFile = paste(inFile, self$getExt(), sep=".")
          }

          inFile = file.path(inDir, inFile)
          if (!file.exists(inFile)) private$plantErr("R205")
          inFile
      }
      # ,mountOutputFile  = function(umlFile, ext) {
      #     ff = strsplit(basename(umlFile), ".", fixed=TRUE)[[1]][1]
      #     file.path(self$getOutputDir(TRUE), paste0(ff,".",ext))
      # }
      ,dataInline       = function(data) {
         if (is.list(data))    return (TRUE)
         if (length(data) > 1) return (TRUE)
         words = strsplit(data, " ")
         if (length(words[[1]]) > 1) return (TRUE)
         words = strsplit(data, "\n")
         if (length(words[[1]]) > 1) return (TRUE)
         FALSE
      }
      ,prepareFile      = function(data, fileName) {
          txt = data
          if(is.list(data)) txt = unlist(data)
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
         # if (!is.null(private$oldForce)) {
         #     self$force = private$oldForce
         #     private$oldForce = NULL
         # }
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
      ,removeUmlTags   = function (data) {
         # Es mas rapido obtener el indice que hacer una sustitucion
         pos = grep("@startuml", data)
         if (pos > 0) data[pos] = ""
         pos = grep("@enduml", data)
         if (pos > 0) data[pos] = ""
         data
      }
   )
)
