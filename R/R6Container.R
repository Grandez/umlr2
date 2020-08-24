#' La clase para interactuar con plantuml
#' @title "R6 Package"
#' @name "R6 Package"
#' @rdname R6Package
#' @docType class
#' @description  Contiene todos los objetos resultantes del analisis
RCONTAINER = R6::R6Class("R6RCONTAINER"
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(void0=NULL
        #' @description Incluye los objetos analizados
        #' @param objects objetos analizados
       ,add = function(objects) {
           for (objs in objects) {
                for (obj in objs) {
                     if (length(.objects) == 0) {
                         private$.objects = list(obj)
                         names(.objects) = obj$name
                     }
                     else {
                       nm = names(.objects)
                       private$.objects[length(.objects) + 1] = obj
                       names(private$.objects) = c(nm, obj$name)
                     }
                }
           }
           invisible(self)
       }
       ,add2package = function(pkg, cls) {
           o = getObject(cls, TRUE)
           p = getObject(pkg, FALSE)
           if (is.null(p)) {
               if (is.character(pkg)) p = RPackage$new(pkg)
               if (isClass(pkg))      p = pkg
               add(p)
           }
           if (is.null(o)) o = cls
           p$add(o)
       }
       #' @description Genera la definicion del diagrama
       #' @return La definicion del diagrama
       ,definition = function() {
           rename()
           uml=c()
           for (pkg in getPackages()) uml = c(uml, pkg$definition())
           objs = mergeClasses()
           for (cls in objs)  uml = c(uml, cls$definition())
           parents = lapply(seq(1,length(objs)), function(x) objs[[x]]$getParentsRelation())
           sons    = lapply(seq(1,length(objs)), function(x) objs[[x]]$getSubclassesRelation())
           layers = generateLayers()
           c(uml, layers, unique(unlist(parents)), unique(unlist(sons)))
       }

      ,getPackages = function(negated = FALSE) { extract("RPackage", negated) }
      ,getClasses = function(negated = FALSE)  { extract("RClass", negated) }
      ,getRelations = function() {

      }
      ,getNotes = function() {

      }
    )
    ,private = list(void1=NULL
      ,.objects = list()
      ,rename = function() {
        nm = lapply(.objects, function(o) o$name)
        names(private$.objects) = nm
      }
      ,extract = function(type, negated) {
        sel = unlist(lapply(.objects, function(obj) type %in% class(obj)))
        if (negated) sel = !sel
        objs = .objects[sel]
        names(objs) = names(.objects)[sel]
        objs
      }
      ,getObject = function(obj, remove = FALSE) {
         o = NULL
         if (is.character(obj)) o = .objects[o]
         if (isClass(obj))      o = obj
         if (remove && !is.null(o)) private$.objects[o$name] = NULL
         o
      }
      ,mergeClasses = function() {
        classes = getClasses()
        res = merge(classes)
        remove(getClasses(TRUE))
        add(res)
        res
      }
      ,merge = function (objs) {
        browser()
        if (length(objs) == 1) return (objs)
        result = objs[[1]]
        names(result) = names(objs[[1]])

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

    )
)
