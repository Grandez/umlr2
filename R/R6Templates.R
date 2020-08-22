#' La clase para interactuar con plantuml
#' @title Templates
#' @name Templates
#' @rdname Templates
#' @docType class
#' @description  La descripcion.
TEMPLATE = R6::R6Class("R6TEMPLATE"
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        #' @description Constructor de la instancia
        #' @param template El nombre del fichero a agregar
        #' @param type El tipo del fichero
        initialize = function(template, type) {
           if (!missing(template)) add(template, TRUE, type)
           private$msg = UMLR2MSG$new()
        }
        #' @description Inserta un fichero de estilos a la lista de estilos
        #' @param template El nombre del fichero a agregar
        #' @param replace Si TRUE elimina los ficheros de estilos anteriores
        #' @param type El tipo del fichero
        ,add = function (template, replace=FALSE, type) {
            ext = switch(type, "style"= "sty", "header" = "tpl", "footer"="tpl", "inc")
            tplFile = private$checkFileClass(template, ext)
            target = switch(type, "style"= "styFiles", "header" = "headFiles", "footer"="footFiles", "incFiles")
            rvalue = ifelse(replace, paste0("list('", tplFile, "')"), paste0("c(private$", target, ", '", tplFile, "')"))
            eval(parse(text=paste0("private$",target, "=", rvalue)))
            invisible(self)
        }
        #' @description Obtiene la informacion que se debe insertar al inicio de la definicion del diagrama
        ,getHeaders    = function() {
            data = c()
            if(length(styFiles)  > 0) data = c(unlist(lapply(styFiles,  function(styFile)  readLines(styFile))))
            if(length(headFiles) > 0) data = c(unlist(lapply(headFiles, function(headFile) readLines(headFile))))
            data
        }
        #' @description Obtiene la informacion que se debe insertar al final de la definicion del diagrama
        ,getFooters    = function() {
            data = c()
            if(length(footFiles)  > 0) data = c(unlist(lapply(footFiles,  function(footFile)  readLines(footFile))))
            data
        }
   )
   ,private = list(
       styFiles  = list()
      ,headFiles = list()
      ,footFiles = list()
      ,incFiles  = list()
      ,msg       = NULL
      ,checkFileClass = function(style, type) {
         files = Files$new()
         ff = style
         if (files$inline(style))        ff = files$tempFile(style, type)
         if (!files$hasExtension(style)) {
             ff = files$getPackageFile(style, type)
             if (nchar(ff) == 0) msg$warning("W510", style)
         }
         ff
      }
   )
)
