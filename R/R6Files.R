#' Gestiona los ficheros del sistema de archivos
#' @title Files
#' @name R6Files
#' @aliases Files
#' @docType class
#' @description  La descripcion.
Files = R6::R6Class("R6Files", inherit = UMLR2BASE,
    public = list(
       #' @field file Fullpath del fichero a procesar
        file = NULL
       ,setConfig = function(config) {
           private$cfg = config
           invisible(self)
        }
       #' @description Crea una instancia de la clase
       #' @param ...  named values para definir la configuración
       #' @return La instancia del objeto
       ,initialize         = function(config) {
           #if (substr(as.character(sys.call(-1))[1], 1, 5) == "Files") private$msg$err("E900", "Files")
           if (!missing(config)) private$cfg = config
       }
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
       ,saveFile = function(fileName) {
          if (is.null(private$inFile)) return (fileName)
           tryCatch( file.rename(fileName, private$outFile)
                    ,error = function(e) private$msg$err("E111", data= e)
           )
           private$outFile
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
       ,getImageFile = function() { private$outFile }
      #' @description Obtiene los datos de configuracion
      #' @return Una lista con los datos de configuracion
      ,getConfig         = function() { private$cfg }
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
      ,inline       = function(data) {
          if (is.list(data) || length(data) > 1)     return (TRUE)
          if (length(strsplit(data, " ")[[1]]) > 1)  return (TRUE)
          if (length(strsplit(data, "\n")[[1]]) > 1) return (TRUE)
          FALSE
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
