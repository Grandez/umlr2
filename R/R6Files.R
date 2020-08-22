#' Gestiona los ficheros del sistema de archivos
#' @title Files
#' @name Files
#' @rdname Files
#' @docType class
#' @description  Clase de gestion de la persistencia
Files = R6::R6Class("R6Files"
    ,inherit      = UMLR2BASE
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        #' @field file Fullpath del fichero a procesar
        file = NULL
        #' @description Establece la referencia al objeto UMLR2::Config en uso
        #' @param config  El objeto UMLR2::Config
       ,setConfig = function(config) {
           private$cfg = config
           invisible(self)
        }
       #' @description Crea una instancia de la clase
       #' @param config Objeto UMLR2::Config en uso
       ,initialize         = function(config) {
           #if (substr(as.character(sys.call(-1))[1], 1, 5) == "Files") private$msg$err("E900", "Files")
           if (!missing(config)) private$cfg = config
       }
       #' @description TO DO
       #' @param data  Datos
       ,getDefinition = function(data) {
           if (is.null(private$inFile)) {
               def = data
               if (is.list(def)) def = unlist(def)
               if (length(def) == 1) def = unlist(strsplit(def, "\n"))
           }
           else {
               def = readLines(private$inFile)
           }
           attr(def, "class") = private$S3Class
           def
       }
       #' @description Verifica los datos y prepara los ficheros
       #' @param data  Definición del diagrama o fichero con la misma
       #' @param name  Si los datos son inline, los almacena previamente
       #' @param force Fuerza a generar el diagrama aunque no haya cambiado
       #' @return TRUE si es necesario generar la imagen
       #'         FALSE si no
       ,prepareData   = function(data, name, force) {
           self$file = NULL
           private$mountFileNames(data,name)
           #if (force) return (private$createTempFile(data, name))
           if (private$inline(data)) return (private$prepareInlineData(data,name))
#           if (!private$matchFile(data)) private$createTempFile(data)
           if (force) return (TRUE)
           # Aqui el fichero bueno
           TRUE
       }
       #' @description TO DO
       #' @param fileName  Nombre del fichero
       ,saveFile = function(fileName) {
          if (is.null(private$inFile)) return (fileName)
           tryCatch( file.rename(fileName, private$outFile)
                    ,error = function(e) private$msg$err("E111", data= e)
           )
           private$outFile
       }
       #' @description Genera un fichero temporal con los datos pasados
       #' @param data  Informacion a grabar
       #' @param ext   Extension a utilizar
       #' @return La ruta al fichero generado
       ,tempFile = function(data, ext="tmp") {
           fileName = tempfile(pattern="uml", fileext=paste0(".", ext))
           writeLines(data,fileName)
           fileName
       }
       #' @description Convierte un path absoluto en relativo a dirbase
       #' @param fullFile  Ruta completa al fichero
       #' @param dirBase   Directorio a usar como base
       #' @return El nombre del fichero relativo a dirBase
       ,relativeTo = function(fullFile, dirBase) {
        #
           if (is.null(dirbase) || nchar(dirbase) == 0) return (fullFille)
           fullDir = normalizePath(dirbase)
           gsub(fullDir, fullFile, fixed=TRUE)
       }
       #' @description Obtiene el nombre del fichero de imagen
       #' @return La ruta al fichero de imagen
       ,getImageFile = function() { private$outFile }
       #' @description Obtiene un fichero del paquete
       #' @param name  Nombre del fichero
       #' @param ext   Extension
       #' @return Path del fichero
       ,getPackageFile = function(name, ext) {
           root = ifelse(interactive(), "inst/extdata", "extdata")
           fName = name
           if (nchar(ext) > 0) fName = paste(fName, ext, sep=".")
           system.file("extdata", fName, package = .Options$pkgName)
       }
      #' @description Obtiene los datos de configuracion
      #' @return Una lista con los datos de configuracion
      ,getConfig         = function() { private$cfg }
      #' @description Chequea si data es un nombre de fichero o son datos
      #' @param data Datos a evaluar
      #' @return TRUE si son datos
      #'         FALSE si es un nombre de fichero
      ,inline       = function(data) {
          if (is.list(data) || length(data) > 1)     return (TRUE)
          if (length(strsplit(data, " ")[[1]]) > 1)  return (TRUE)
          if (length(strsplit(data, "\n")[[1]]) > 1) return (TRUE)
          FALSE
      }
      #' @description Chequea si un fichero tiene extension
      #' @param file  Fichero
      #' @return TRUE si tiene extension
      #'         FALSE en caso contrario
      ,hasExtension = function(file) grepl("\\..+$", file)
   )
   ,private = list(x=NA
      ,cfg = NULL
      ,inFile = NULL
      ,outFile = NULL
      ,makeObject = function(data) {
         txt = data
         if (is.list(data)) txt = unlist(data)
         attr(txt, "class") = private$S3Class
         txt
      }
      ,prepareInlineData = function(data, fileName) {

          if (is.null(fileName)) {
              private$inFile = NULL
              private$outFile = NULL
              return (private$createTempFile(data))
          }
          if (nchar(private$cfg$getInputDir()) == 0) msg.err("E200")
          f = private$getFullPath(fileName, private$cfg$getInputDir(), private$cfg$getExt())

          if (!file.exists(f) || !private$matchMD5(data, f)) {
              return (private$writeFile(data, f))
          }

          fOut = private$getFullPath(filename, private$cfg$getOutputDir(), private$cfg$getType())
          if (private$matchFiles(private$inFile, fOut)) private$createTempFile(data)
      }
      ,matchFiles = function(fileIn, fileOut) {
          if (!file.exists(fileIn))  return (FALSE)
          if (!file.exists(fileOut)) return (FALSE)
          fileIn.info(fileIn)$mtime > fileOut.info(fileOut)$mtime
      }
      ,mountFileNames = function (data,name) {
          fileName = data
          if(private$inline(data)) fileName = name

          if (is.null(fileName)) {
            private$inFile = NULL
          }
          else {
            private$inFile = private$getFullPath(fileName, private$cfg$getInputDir(), private$cfg$getExt())
            fileName = gsub("\\..+$", "", fileName)
            private$outFile = private$getFullPath(fileName, private$cfg$getOutputDir(), private$cfg$getType())
          }
      }
      ,getFullPath = function(filename, dirname, ext) {
          fullPath = filename
          if (!private$isFullPath(filename)) fullPath = file.path(dirname, filename)
          if (length(grep(".", fullPath, fixed=TRUE))== 0 ) fullPath = paste(fullPath, ext, sep=".")
          fullPath
      }
      ,isFullPath = function (filename) {
         # Check for absolute path in nix and windows
         if (substring(filename, 1,1) == "/") return (TRUE)
         if (nchar(filename) > 1 && substring(filename, 2,2) == ":") return(TRUE)
         FALSE
      }
      ,writeFile = function(data, fileName) {
          tryCatch(writeLines(data,fileName)
            ,error = function(e) {
              private$msg$err("E111",e)
            }
          )
          TRUE
      }
      ,createTempFile = function(data) {
           tryCatch({
                self$file = tempfile(fileext=".uml")
                if (is.null(private$inFile)) {
                    writeLines(paste(data, collapse="\n"), self$file)
                }
                else {
                    file.copy(private$inFile, self$file)
                }
             }, error = function(e) {
                msg.err("E110")
             }
           )
           TRUE
      }
      ,matchMD5 = function (data, fName) {
         md5File = private$changeExt(fName, "md5")
         if (!file.exists(md5File)) return (FALSE)
         md5in = private$calcMD5(data)
         md5f = readLines(md5File)
         (md5in == md5f)
     }
      ,changeExt = function(fName, newExt) {
          rr = strsplit(fName, "\\.")
          if (length(rr[[1]]) >  1) rr[[1]][length(rr[[1]])] = newExt
          if (length(rr[[1]]) == 1) rr[[1]][2] = newExt
          paste(rr[[1]], collapse=".")
       }
   )
)
