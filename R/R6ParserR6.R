#' La clase para interactuar con plantuml
#' @title R6Parser
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
ParserR6 = R6::R6Class("R6PARSERR6", inherit = Parser,
   public = list(
       initialize = function(object, detail) {
          private$detail = detail
          private$object = object
          private$.getGenerators()  # Generadores
       }
      ,parse = function() {
          c0 = class(private$object)[1]
          p = R6CLASS$new(private$generators[c0], c0, 0)
          private$pend = list(p)
          names(private$pend) = p$name
          private$process()
          private$generateDefinition()

      }
   )
   ,private = list(
       object = NULL
      ,detail = NULL
      ,generators = NULL
      ,pend  = NULL
      ,hecho = list()  # Para obtener NA la primera vez
      ,maxDeep = 0
      ,process = function () {
         det = private$detail

         while (length(private$pend) > 0) {
            if (private$parseClass()) {
               private$hecho[[length(private$hecho) + 1]] = private$pend[[1]]
               nm = names(private$hecho)
               if (is.null(nm)) nm = c("")   # Primera vez
               nm[length(nm)] = names(private$pend)[1]
               names(private$hecho) = nm
               if (length(private$hecho) > 0) {
                  det1 = bitwAnd(det, UMLType$simple + UMLType$complete)
                  if (bitwAnd(det, UMLType$classSimple)   > 0) det1 = bitwOr(det1,UMLType$simple)
                  if (bitwAnd(det, UMLType$classComplete) > 0) det1 = bitwOr(det1,UMLType$complete)
                  det = det1
               }
            }
            private$pend[[1]] = NULL
         }
      }
      ,parseClass = function () {
         cls = private$pend[[1]]
         # Los siguientes estan al mismo nivel o superior
         if (cls$deep > private$maxDeep) {
            private$pend = NULL
            return(FALSE)
         }

         if (cls$name %in% names(private$hecho)) return (FALSE)

         # Basico, obtener la informacion directamente de la clase
         if (bitwAnd(private$detail,1) == 0) return (private$parseOutput(cls))

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
         private$addAttributes(cls, TRUE)

         if (bitwAnd(private$detail,2) > 0) private$addAttributes(cls, FALSE)

         private$.getComposition(cls)
         private$.getAggregation(cls)
         TRUE
      }
      ,generateDefinition = function() {
         # Al menos hay uno
         classes = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getClassDefinition(private$detail))
         parents = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getParentsRelation())
         sons    = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getSubclassesRelation())

         c(private$.header, unlist(classes), unlist(parents), unlist(sons))
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
      ,addClass = function(cls) {
         if (!is.list(cls)) cls = list(cls)
         sapply(cls, function(x) {
            nm = names(private$pend)
            private$pend[[length(private$pend) + 1]] = x
            names(private$pend) = c(nm, x$name)
         }, USE.NAMES=TRUE)
      }
      ,addAttributes      = function(cls, public) {
         vis = ifelse(public, "public", "private")
         cls$addFields  (eval(parse(text=paste0(cls$name, "$", vis, "_fields"))),  public)
         cls$addMethods (eval(parse(text=paste0(cls$name, "$", vis, "_methods"))), public)
         if (public) cls$addBindings(eval(parse(text=paste0(cls$name, "$active"))))
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
      ,.clearOutput = function(data) {
         values = gsub(":.*", "", data)
         names(values) = values
         as.list(values)
      }

   )
)
