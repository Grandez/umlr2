#' La clase para interactuar con plantuml
#' @title UMLR
#' @name UMLR
#' @rdname UMLR
#' @docType class
#' @description  La descripcion.
#' @export
UMLR = R6::R6Class("R6UMLR"
     ,inherit      = UMLBASE
     ,portable     = FALSE
     ,lock_objects = TRUE
     ,lock_class   = TRUE
     ,active = list(
         #' @field config Devuelve el objeto CONFIG
         config = function(value) {
            if (!missing(value)) msg$err("R012", "config")
            super$config
         }
     )
    ,public = list(
          #' @description Crea una instancia de la clase
          #' @param ...  named values para definir la configuración
          #' @return La instancia del objeto
           initialize     = function( ...) {
              super$initialize(...)
           }
           #' @description destructor de la clase
          ,finalize      = function()     { super$finalize()      }
          #' @description Obtiene la definicion del diagrama
          #' @param object Instancia de objeto S4 o R6
          #' @param detail Nivel de detalle
          #' @param deep   Nivel de profundidad de analisis de la clase
          ,getSource = function (object, detail=UMLShow$simple, deep = 100) { # deep = .Machine$integer.max) {
             uml = umlClass(object, detail, deep)
             getDiagram(uml, .parser$getSummary())
          }
          #' @description Genera un diagrama a partir de la instancia del objeto
          #' @param object Instancia de objeto S4 o R6
          #' @param detail Nivel de detalle
          #' @param deep   Nivel de profundidad de analisis de la clase
          #' @details
          #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
          #'     - El fichero con la imagen no se almacena en el sistema de archivos
          ,plotClass             = function (object, detail=UMLShow$simple, deep = .Machine$integer.max) {
               uml     = umlClass(object, detail, deep)
               imgFile = .plant$genDiagram(uml, .parser$getSummary())
               knitr::include_graphics(normalizePath(imgFile))
          }
          #' @description Genera la definicion del diagrama a partir de la instancia del objeto
          #' @param object Instancia de objeto
          #' @param detail Nivel de detalle
          #' @param deep   Nivel de profundidad de analisis de la clase
          #' @details
          #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
          #'     - El fichero con la imagen no se almacena en el sistema de archivos
          ,umlClass             = function (object, detail=UMLShow$simple, deep = 100) {
              checkObject(object)
              objs = parse(object, detail, deep)
              .parser$setObjects(objs)
              .parser$generateDefinition()
          }
            #' @description Agrega definiciones de PlantUML al inicio del documento
            #' @param header definiciones de PlantUML
          ,header = function(header) {
              .header = header
              invisible(self)
          }
 )
 ,private = list(
       .plant = NULL
      ,.header = c("hide empty members", "hide empty fields")
      ,.parser = NULL
      ,parse   = function(object, detail, deep) {
          objs = list()
          if (!is.list(object)) objs = list(parseObject(object, detail, deep))
          else                  objs = lapply(object, function(obj) parseObject(obj,detail,deep))
          merge(objs)
      }
      ,parseObject = function(object, detail, deep) {
          obj = NULL
          detail = sum(detail)
          parser = getParser(object, detail, deep)
          if (!is.null(parser)) obj = parser$parse()
          obj
      }
      ,getParser = function(object, detail, deep) {
           if (isS4(object))      return (ParserS4$new(object, detail, deep))
           if (R6::is.R6(object)) return (ParserR6$new(object, detail, deep))
           if (is.environment(object)) {
               if (isS4(object$self))      return (ParserS4$new(object$self, detail, deep))
               if (R6::is.R6(object$self)) return (ParserR6$new(object$self, detail, deep))
           }
           msg$warning("E810")
           NULL
        }
      ,merge = function(objs) {
          mergeClasses(objs)
      }
      ,mergeClasses = function (objs) {
           result = objs[[1]]
           names(result) = names(objs[[1]])
           if (length(objs) == 1) return (result)
           for (idx in 2:length(objs)) {
                classesL = result
                classesR = objs[[idx]]
                namesL = sort(names(result))
                namesR = sort(names(classesR))
                sizeL = length(namesL)
                sizeR = length(namesR)
                selL = seq(1,sizeL)
                selR = seq(1,sizeR)
                idxL = idxR = 1

                while (idxL <= sizeL && idxR <= sizeR) {
                       if      (namesL[idxL] > namesR[idxR]) idxR = idxR + 1
                       else if (namesL[idxL] < namesR[idxR]) idxL = idxL + 1
                            else { detL = classesL[[namesL[idxL]]]$detail # Iguales
                                   detR = classesR[[namesR[idxR]]]$detail
                                   if (detL >= detR) selR[idxR] = 0
                                   else              selL[idxL] = 0
                                   idxL = idxL + 1
                                   idxR = idxR + 1
                                 }
               }
               result = c(classesL[namesL[selL]], classesR[namesR[selR]])
               names(result) =  c(namesL[selL], namesR[selR])
          }
          result
      }
      ,checkObject = function(object) {
           # Si es un unico objeto deberia tener class informado
           # si no es un objeto, deberia tener una longitud mayor que 1
           if (length(object) == 1) {
               if (!is.object(object)) msg$err("R020", as.character(sys.call(-2))[2])
           }
      }
      # ,merge = function(objs) {
      #   result = objs[[1]]
      #   names(result) = names(objs[[1]])
      #   if (length(objs) == 1) return (result)
      #   for (idx in 2:length(objs)) {
      #     classesL = result
      #     classesR = objs[[idx]]
      #     namesL = sort(names(result))
      #     namesR = sort(names(objs[[idx]]))
      #     idxL = idxR = 1
      #     private$mergeObjs = list()  # Evitar las copias
      #
      #     while (idxL <= length(namesL) && idxR <= length(namesR)) {
      #       if (namesL[idxL] > namesR[idxR]) {
      #         private$add(classesR, namesR[idxR])
      #         idxR = idxR + 1
      #       }
      #       else if (namesL[idxL] < namesR[idxR]) {
      #         private$add(classesL, namesL[idxL])
      #         idxL = idxL + 1
      #       }
      #       else if (namesL[idxL] == namesR[idxR]) {
      #         if (classesL[[namesL[idxL]]]$detail >= classesR[[namesR[idxR]]]$detail) {
      #           private$add(classesL, namesL[idxL])
      #         }
      #         else {
      #           private$add(classesR, namesR[idxR])
      #         }
      #         idx1 = idx1 + 1
      #         idx2 = idx2 + 1
      #       }
      #     }
      #     nobjs = private$add(classesL, tail(namesL, idxL * -1 ))
      #     nobjs = private$add(classesR, tail(namesR, idxR * -1 ))
      #     result = private$mergeObjs
      #   }
      # }

     ,add = function (objs, names) {
         if (length(names) == 0) return()
         for (name in names) {
           nm = names(mergeObjs)
           mergeObjs[length(mergeObjs) + 1] = objs[name]
           nm = c(nm, name)
           names(mergeObjs) = nm
         }
     }
   )
)

