#' La clase para interactuar con plantuml
#' @title UMLR
#' @docType class
#' @name UMLR
#' @description  La descripcion.
#' @export
library(R6)
UMLR = R6Class("R6UMLR", inherit = PLANTUML,
     active = list(
         #' @field config Devuelve el objeto CONFIG
         config = function(value) {
           if (!missing(value)) private$msg$err("R012", "config")
               super$config
           }
     )
    ,public = list(
         #' @description Crea una instancia de la clase
         #' @param ...  named values para definir la configuración
         #' @return La instancia del objeto
         #'
         #' @examples
         #' plant = UMLR$new()
         #' plant = UMLR$new(jvm='java')
         #' plant = UMLR$new(c(jvm='java', plantuml='plantuml.jar'))
          initialize     = function( ...) { super$initialize(...) }
          #' @description destructor de la clase
         ,finalize      = function()     { super$finalize()      }
         #' @description Genera un diagrama a partir de la instancia del objeto
         #' @param object Instancia de objeto S4 o R6
         #' @param detail Nivel de detalle
         #' @param deep   Nivel de profundidad de analisis de la clase
         #' @details
         #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
         #'     - El fichero con la imagen no se almacena en el sistema de archivos
         ,plotClass             = function (object, detail=UMLShow$simple, deep = 1) {
              uml = self$umlClass(object, detail, deep)
              imgFile = private$makeImage(uml)
              knitr::include_graphics(normalizePath(imgFile))
         }
         #' @description Genera la definicion del diagrama a partir de la instancia del objeto
         #' @param object Instancia de objeto
         #' @param detail Nivel de detalle
         #' @param deep   Nivel de profundidad de analisis de la clase
         #' @details
         #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
         #'     - El fichero con la imagen no se almacena en el sistema de archivos
         ,umlClass             = function (object, detail=UMLShow$simple, deep = 1) {
             objs = private$parse(object, detail, deep)
             parser = Parser$new(NULL, 0, 0)
             parser$setObjects(objs)
             parser$generateDefinition
         }
           #' @description Agrega definiciones de PlantUML al inicio del documento
           #' @param header definiciones de PlantUML
         ,header = function(header) {
             private$.header = header
             invisible(self)
         }
)
,private = list(
     .header = c("hide empty members", "hide empty fields")
     ,mergeObjs = list()
     ,parse  = function(object, detail, deep) {
         objs = lapply(object, function(obj) private$parseObject(obj,detail,deep))
         private$merge(objs)
     }
     ,parseObject = function(object, detail, deep) {
       obj = NULL
       detail = sum(detail)

       if (isS4(object))      obj = ParserS4$new(object, detail, deep)
       if (R6::is.R6(object)) obj = ParserR6$new(object, detail, deep)
       if (is.null(obj)) {
           warning("'object' is not an instance of S4 or R6 Classes")
           return (NULL)
       }
       obj$parse()
     }
     ,merge = function(objs) {
         result = objs[[1]]
         names(result) = names(objs[[1]])
         if (length(objs) == 1) return (result)
         for (idx in 2:length(objs)) {
            classesL = result
            classesR = objs[[idx]]
            namesL = sort(names(result))
            namesR = sort(names(objs[[idx]]))
            idxL = idxR = 1
            private$mergeObjs = list()  # Evitar las copias

            while (idxL <= length(namesL) && idxR <= length(namesR)) {
               if (namesL[idxL] > namesR[idxR]) {
                   private$add(classesR, namesR[idxR])
                   idxR = idxR + 1
                }
                else if (namesL[idxL] < namesR[idxR]) {
                    private$add(classesL, namesL[idxL])
                    idxL = idxL + 1
                 }
                 else if (namesL[idxL] == namesR[idxR]) {
                    if (classesL[[namesL[idxL]]]$detail >= classesR[[namesR[idxR]]]$detail) {
                        private$add(classesL, namesL[idxL])
                    }
                    else {
                       private$add(classesR, namesR[idxR])
                    }
                    idx1 = idx1 + 1
                    idx2 = idx2 + 1
                }
            }
            nobjs = private$add(classesL, tail(namesL, idxL * -1 ))
            nobjs = private$add(classesR, tail(namesR, idxR * -1 ))
            result = private$mergeObjs
         }
     }
    ,add = function (objs, names) {
        if (length(names) == 0) return()
        for (name in names) {
          nm = names(private$mergeObjs)
          private$mergeObjs[length(private$mergeObjs) + 1] = objs[name]
          nm = c(nm, name)
          names(private$mergeObjs) = nm
        }
    }
#    ,generators = NULL
#    ,maxDeep = 0
    # ,getAttrs  = function (className, visibility) {
    #     data = list(fields = NULL, methods = NULL)
    #     lFields  = eval(parse(text=paste0(className, "$", visibility, "_fields")))
    #     lMethods = eval(parse(text=paste0(className, "$", visibility, "_methods")))
    #     list(fields=names(lFields), methods=names(lMethods))
    # }
  )
)

