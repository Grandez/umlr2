#' La clase para interactuar con plantuml
#' @title R6UMLR
#' @docType class
#' @description  La descripcion.
#  Que opciones:
#  basico - Solo la clase y lo publico
#  simple - con accesors
#  private - rpivado y public
#
# por bits
#  UMLR$accesors + UMLR$private
#' @export
UMLR = R6::R6Class("UMLR", inherit = PLANTUML,
     public = list(
         detail = 254

         #####
         #' @description Crea una instancia de la clase
         #' @param ...  named values para definir la configuración
         #' @return La instancia del objeto
         #'
         #' @examples
         #' plant = UMLR$new()
         #' plant = UMLR$new(jvm='java')
         #' plant = UMLR$new(c(jvm='java', plantuml='plantuml.jar'))
        ,initialize         = function( ...) {
             super$initialize(...)
         }
         ,finalize = function() {
             super$finalize()
             message("Cleaning up R6UML")
         }
         #' @description Genera un diagrama a partir de la instancia del objeto
         #' @param object Instancia de objeto S4 o R6
         #' @param detail Instancia de objeto S4 o R6
         #' @param deep   Nivel de profundidad de analisis de la clase
         #' @details
         #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
         #'     - El fichero con la imagen no se almacena en el sistema de archivos
         ,plotClass             = function (object, detail=14, deep = 0) {
              uml = self$umlClass(object, detail, deep)
              imgFile = private$makeImage(uml)
              knitr::include_graphics(normalizePath(imgFile))
         }
         ,umlClass             = function (object, detail=14, deep = 0) {
             detail = sum(detail)
             private$maxDeep = private$.setMaxDeep(detail, deep)
             private$hecho = list()
             private$pend = NULL
             private$obj  = object
             uml = NULL
             if (isS4(object))      uml = private$parseS4(object, detail)
             if (R6::is.R6(object)) uml = private$parseR6(object, detail)
             if (is.null(uml)) warning("'object' is not an instance of S4 or R6 Classes")
             names(uml) = NULL
             uml
         }
         ,header = function(header) {
           private$.header = header
         }
)
,private = list(
     .header = c("hide empty members", "hide empty fields")
    ,hecho = list()  # Para obtener NA la primera vez
    ,generators = NULL
    ,maxDeep = 0
    ,pend  = NULL
    ,obj   = FALSE

    ,parseS4 = function (object, full=FALSE, deep = FALSE) {
    }
    ,parseR6   = function (object, detail) {
        private$.getGenerators()  # Generadores

        c0 = class(object)[1]
        p = R6CLASS$new(private$generators[c0], c0, 0)
        private$pend = list(p)
        names(private$pend) = p$name
        private$processR6Pending(detail)
        private$generateDefinition(detail)
     }
    ,processR6Pending = function (detail) {
        # La primera clase es la objetivo
        # las siguientes son padres e hijos
        det = detail
        items = 0

        while (length(private$pend) > 0) {
          if (private$parseR6Class(det)) {
              private$hecho[[length(private$hecho) + 1]] = private$pend[[1]]
              nm = names(private$hecho)
              if (is.null(nm)) nm = c("")   # Primera vez
              nm[length(nm)] = names(private$pend)[1]
              names(private$hecho) = nm

              items = items + 1
              if (items == 1) {
                det1 = bitwAnd(det, UMLType$simple + UMLType$complete)
                if (bitwAnd(det, UMLType$classSimple)   > 0) det1 = bitwOr(det1,UMLType$simple)
                if (bitwAnd(det, UMLType$classComplete) > 0) det1 = bitwOr(det1,UMLType$complete)
                det = det1
              }
          }
          private$pend[[1]] = NULL
        }
     }
    ,parseR6Class = function (detail) {
         cls = private$pend[[1]]
         # Los siguientes estan al mismo nivel o superior
         if (cls$deep > private$maxDeep) {
             private$pend = NULL
             return(FALSE)
         }

         if (cls$name %in% names(private$hecho)) return (FALSE)

         # Basico, obtener la informacion directamente de la clase
         if (bitwAnd(detail,1) == 0) return (private$parseOutput(cls))

        # Clase heredada. R6 no tiene multiple herencia
         if (cls$deep < private$maxDeep) {
             g = eval(parse(text=paste0(cls$name, "$get_inherit()")))
             if (!is.null(g)) {
                 p = R6CLASS$new(private$generators[g$classname], g$classname, cls$deep + 1)
                 private$addClass(p)
                 cls$addParent(p)
             }
         }
         # Fields & methods
         private$.addAttributes(cls, TRUE)

         if (bitwAnd(detail,2) > 0) private$.addAttributes(cls, FALSE)

         private$.getComposition(cls)
         private$.getAggregation(cls)
         TRUE
    }
    ,.getGenerators = function () {
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
        private$generators = nm
    }
    ,generateDefinition = function(detail) {
      # Al menos hay uno
      classes = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getClassDefinition(detail))
      parents = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getParentsRelation())
      sons    = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getSubclassesRelation())

      c(private$.header, unlist(classes), unlist(parents), unlist(sons))
    }
    ,.addAttributes      = function(cls, public) {
         vis = ifelse(public, "public", "private")
         cls$addFields  (eval(parse(text=paste0(cls$name, "$", vis, "_fields"))),  public)
         cls$addMethods (eval(parse(text=paste0(cls$name, "$", vis, "_methods"))), public)
         if (public) cls$addBindings(eval(parse(text=paste0(cls$name, "$active"))))
    }
    ,addClass = function(cls) {
       if (!is.list(cls)) cls = list(cls)
       sapply(cls, function(x) {
                     nm = names(private$pend)
                     private$pend[[length(private$pend) + 1]] = x
                     names(private$pend) = c(nm, x$name)
       }, USE.NAMES=TRUE)
    }
    ,getAttrs  = function (className, visibility) {
        data = list(fields = NULL, methods = NULL)
        lFields  = eval(parse(text=paste0(className, "$", visibility, "_fields")))
        lMethods = eval(parse(text=paste0(className, "$", visibility, "_methods")))
        list(fields=names(lFields), methods=names(lMethods))
    }
    ,.getComposition = function(cls) {
        # Campos
        fields = eval(parse(text=paste0(cls$name, "$public_fields")))
        fields = c(fields, eval(parse(text=paste0(cls$name, "$private_fields"))))

        # Si tiene inicializador a una clase, esta es un environment
        classes = names(grep("<environment>", fixed=TRUE, fields, value=T))
        clases = lapply(fields[classes], class)

        # Cogemos los primeros
        if (length(clases) > 0) {
          hijos = unique(unlist(lapply(clases, function(x) x[[1]])))
          hijos = lapply(hijos, function(x) R6CLASS$new(private$generators[hijos], x, cls$deep + 1))
          names(hijos) = sapply(hijos, function(x) x$name)
          cls$addComposition(hijos)
          private$addClass(hijos)
        }

        hijos = private$parseFunction(cls$name, "public_methods[\"initialize\"]")
        if (length(hijos) > 0) {
            hijos = unique(hijos)
            pos =
            hijos = lapply(hijos, function(x) { pos = which(private$generators == x)
                                                R6CLASS$new(x, names(private$generators)[pos], cls$deep + 1)})
            names(hijos) = sapply(hijos, function(x) x$name)
            cls$addComposition(hijos)
            private$addClass(hijos)
        }
    }
    ,.getAggregation = function(cls) {
        # Campos
        funcs = eval(parse(text=paste0(cls$name, "$public_methods")))
        funcs["initialize"] = NULL
        funcs["clone"] = NULL

        hijos1 = sapply(names(funcs), function(x) private$parseFunction(cls$name, paste0("public_methods[\"",x,"\"]")))

        funcs = eval(parse(text=paste0(cls$name, "$private_methods")))
        hijos2 = sapply(names(funcs), function(x) private$parseFunction(cls$name, paste0("private_methods[\"",x,"\"]")))
        hijos = c(Filter(Negate(is.null),hijos1), Filter(Negate(is.null),hijos2))

        if (length(hijos) > 0) {
            hijos = unique(hijos)
            hijos = lapply(hijos, function(x) { pos = which(private$generators == x)
                                                R6CLASS$new(x, names(private$generators)[pos], cls$deep + 1)})

            names(hijos) = sapply(hijos, function(x) x$name)
            cls$addAggregation(hijos)
            private$addClass(hijos)
        }
    }
    ,parseFunction = function(clsName, func) {

        init = eval(parse(text=paste0(clsName, "$", func)))
        if (is.na(names(init[1]))) return(NULL)

        out = capture.output(init[[1]])
        pat = "[a-zA-Z0-9_\\.]+\\$new[ ]*\\("
        txt = grep(pat, out, value=TRUE)

        # Aqui el initialize
        if (length(txt) == 0) return(NULL)

        sapply(txt, function(x) { m = gregexpr(pat,x)
                                  t = substring(x,m[[1]],m[[1]]+attr(m[[1]],'match.length')-1)
                                  gsub("\\$.*","",t)
                                }, USE.NAMES=F)
    }
    ,parseOutput = function(cls) {
       if (cls$deep > 0) return (TRUE)
       data = trimws(capture.output(private$obj))
       idx = grep("Public:", data, ignore.case=T)
       idxBeg = ifelse (length(idx) > 0, idx[1] + 1, 0)
       idx = grep("Private:", data, ignore.case=T)
       idxEnd = ifelse(length(idx) > 0, idx[1] - 1, length(data))
       if (idxBeg == 0) return
       data = data[idxBeg:idxEnd]
       mask = grepl("function[ ]*\\(", data)
       if (sum(mask) > 0)  cls$addMethods(private$.clearOutput(data[mask]), TRUE)
       if (sum(!mask) == 0) return (TRUE)
       fields = data[!mask]
       mask = grepl("active binding", fields)
       cls$addBindings(private$.clearOutput(fields[mask]))
       cls$addFields  (private$.clearOutput(fields[!mask]), TRUE)
       TRUE
    }
    ,.clearOutput = function(data) {
        values = gsub(":.*", "", data)
        names(values) = values
        as.list(values)
    }
    , .setMaxDeep = function(detail, deep) {
        res = ifelse(detail < UMLType$parents, 0, deep)
        if (bitwAnd(detail, UMLType$parents)    > 0 && res == 0) res = 1
        if (bitwAnd(detail, UMLType$subclasses) > 0 && res == 0) res = 1
        res
    }
  )
)

