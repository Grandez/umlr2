#' La clase para interactuar con plantuml
#' @title "R6 Package"
#' @name "R6 Package"
#' @rdname R6Package
#' @docType class
#' @description  Contiene todos los objetos resultantes del analisis
library(dplyr)
RCONTAINER = R6::R6Class("R6RCONTAINER"
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        setMainClass = function(mainClass) {
           clsName = NULL
           if (is.character(mainClass)) clsName = mainClass
           if (is.object(mainClass))    clsName = mainClass$name
           private$vMain[length(vMain) + 1] = clsName
        }
        #' @description Incluye los objetos analizados
        #' @param objects objetos analizados
       ,add = function(objects, force=TRUE) {
           #if (!force && !is.null(.objects[objects$name])) return (invisible(self))

           private$prepared = FALSE
           if (is.object(objects)) addObject(objects)
          else                     apply(objects, function(x) addObject(x))
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
       ,getDefinition = function(deep = .Machine$integer.max) {
           private$deep = deep
           if (!prepared) prepare()
           preprocess()
           uml=generateHeaders()

           for (pkg in getPackages()) uml = c(uml, pkg$definition())
           uml = c(uml, generateClasses())
           uml = c(uml, generateRelations())
           unlist(uml)
       }
      ,getSummary = function() {
          clss         = getClasses()
          classes      = unlist(sapply(clss, function(x) if (x$isClass())      x$.generator))
          subclasses   = unlist(sapply(clss, function(x) if (x$isSubclass())   x$.generator))
          superclasses = unlist(sapply(clss, function(x) if (x$isSuperclass()) x$.generator))
          list(classes=classes, subclasses=subclasses, superclasses=superclasses)
      }
      ,getPackages  = function(negated = FALSE) { extract("RPACKAGE",  negated) }
      ,getClasses   = function(negated = FALSE) { extract("RCLASS",    negated) }
      ,getRelations = function(negated = FALSE) { extract("RRELATION", negated) }
      ,getNotes = function() { }
    )
    ,private = list(
       prepared = FALSE
      ,deep = .Machine$integer.max
      ,.objects = list()
      ,vMain = c()
      ,dfRels  = NULL
      ,dfClss  = NULL
      ,dfObjs  = NULL
      ,prepare = function() {
        private$prepared = TRUE
          makeDataPointers()
          extractClasses()
          setMainClasses()
          extractRelations()
          setDeeps()
      }
      ,makeDataPointers = function() {
           cls =  sapply(.objects, function(obj) obj$class, USE.NAMES = FALSE)
           name = sapply(.objects, function(obj) obj$name,  USE.NAMES = FALSE)
           pos = seq(1, length(cls))
           # Todos los objetos
           private$dfObjs = data.frame(cls=cls,name=name, pos=pos, row.names = NULL)
      }
      ,extractClasses = function() {
           private$dfClss = dfObjs[dfObjs$cls == ObjType$class,]
           rr = lapply(dfClss$pos, function(pos) {obj = .objects[[pos]]
                                             c(type=obj$type, detail=obj$detail, deep=obj$deep, source=obj$source)})
           private$dfClss = cbind(dfClss, type=sapply(rr, function(x) x["type"]))
           private$dfClss = cbind(dfClss, detail=sapply(rr, function(x) x["detail"]))
           private$dfClss = cbind(dfClss, deep=sapply(rr, function(x) x["deep"]))
           private$dfClss = cbind(dfClss, source=sapply(rr, function(x) x["source"]))

           private$dfClss = dfClss[with(dfClss, order(name, -detail)), ] %>%
                            distinct(name, .keep_all = TRUE)

      }
      ,extractRelations = function() {
           private$dfRels = dfObjs[dfObjs$cls == ObjType$relation,]
           data = unlist(strsplit(dfRels$name, "-"))
           len = length(data) / 2
           ffrom = data[1:len]
           tto   = data[(len+1):length(data)]
           private$dfRels = cbind(dfRels, from=ffrom, to=tto, deep=.Machine$integer.max - 1)
           private$dfRels = dfRels[with(dfRels, order(name)), ] %>%
                            distinct(name, .keep_all = TRUE)
      }
      ,preprocess = function() {
      }
      ,setMainClasses = function() {
          for (mainClass in vMain) {
             obj = .objects[[mainClass]]
             if (!is.null(obj)) {
                 obj$addType(ClassType$main)
                 obj$setDeep(0)
                 dfClss[dfClss$name == obj$name, "deep"] = 0
             }
             else {
               self$add(RClass$new(mainClass, type=ClassType$main, deep=0))
             }
          }

      }
      ,setDeeps = function() {
          sel = private$dfRels$from %in% vMain
          setDeepsRec(sel, 1)
      }
      ,setDeepsRec = function(sel, deep) {
          if (sum(sel) > 0) {
              idx = which(sel == TRUE)
              dfRels[sel,"deep"] = ifelse(dfRels[sel,"deep"] > deep, deep, dfRels[sel,"deep"])
              nextRel = dfRels[sel, "to"]
              selC = dfClss$name %in% nextRel
              dfClss[selC,"deep"] = ifelse(dfClss[selC,"deep"] > deep, deep, dfClss[selC,"deep"])
              selR = private$dfRels$from %in% nextRel
              setDeepsRec(selR, deep + 1)
          }
      }
      ,rename = function() {
        nm = lapply(.objects, function(o) o$name)
        names(private$.objects) = nm
      }
      ,generateHeaders = function() {
        c()
      }
      ,generateClasses = function() {
          uml = c()
          # classes = mergeClasses()
          if (nrow(dfClss) > 0) {
              classes = .objects[dfClss[dfClss$deep <= deep,"pos"]]
              for (cls in classes)  uml = c(uml, cls$definition())
              parents = lapply(seq(1,length(classes)), function(x) classes[[x]]$getParentsRelation())
              uml     = c(uml, unique(unlist(parents)))
              sons    = lapply(seq(1,length(classes)), function(x) classes[[x]]$getSubclassesRelation())
              uml     = c(uml, unique(unlist(sons)))
          }
          uml
      }
      ,generateRelations = function() {
          uml = NULL
          if (nrow(dfRels) > 0) {
              uml = lapply(.objects[dfRels$pos], function(x) x$definition())
           }
           uml
      }
      ,generateLayers = function(classes) {
          l = lapply(seq(1,length(classes)), function(x) classes[[x]]$deep)
          names(l) = names(classes)
          levels = unlist(l)
          ll = sapply(seq(0,max(levels)), function(x) levels[levels == x], USE.NAMES = TRUE)
          # Solo cuando hay mas de una clase
          ll = ll[sapply(ll, function(x) length(x) > 1)]
          classes = lapply(ll, function (x) paste("   class ", names(x), collapse="\n"))
          layers = lapply(classes, function(x) paste("together {\n", x, "\n}\n", collapse="\n"))
          unlist(layers)
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
        if (length(classes) == 0) return(NULL)
        res = merge(classes)
        toDelete =  getClasses(TRUE)
        if (length(toDelete) > 0) remove(toDelete)
        add(res)
        res
      }
      ,merge = function (objs) {
        if (length(objs) == 1) return (objs)
        result = list(objs[[1]])
        names(result) = result[[1]]$name
        max = length(objs)
        for (idx in 2:max) {
          classesL = result
          classesR = objs[idx:max]
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
      ,addObject = function (obj) {
          idx = 0
          if (length(.objects) == 0) {
              private$.objects = list(obj)
              names(private$.objects) = obj$name
           }
           else {
              nm = names(.objects)
              idx = length(.objects)
              private$.objects[[idx + 1]] = obj
              names(private$.objects) = c(nm, obj$name)
           }
           if (class(obj)[1] == "RRELATION") {
               self$add(RClass$new(obj$from, type=ClassType$named), force=FALSE)
               self$add(RClass$new(obj$to,   type=ClassType$named), force=FALSE)
           # reg = list(obj$from, obj$to, obj$type, 0, pos)
           # names(reg) = colnames(dfRel)
           # private$dfRel = rbind(dfRel, reg)
         }

           # add2df(obj, idx + 1)
           # add2main(obj)
       }
      ,add2df = function(obj, pos) {
         cls = class(obj)[1]
         if (cls == "RRELATION") {
           reg = list(obj$from, obj$to, obj$type, 0, pos)
           names(reg) = colnames(dfRel)
           private$dfRel = rbind(dfRel, reg)
         }
         if (cls == "RCLASS") {
           reg = list(obj$name, obj$detail, obj$type, obj$deep, pos)
           names(reg) = colnames(dfClass)
           private$dfClass = rbind(dfClass, reg)
         }
      }
      ,add2main = function(cls) {
         if (class(cls)[1] == "RCLASS") {
             if (cls$isMain()) {
                 private$vMain = c(private$vMain, cls$name)
                 private$vMain = unique(private$vMain)
             }
         }
      }
    )
)
