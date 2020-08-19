#' Parser de objetos de tipo R6
#' @title ParserR6
#' @name ParserR6
#' @rdname ParserR6
#' @docType class
#' @aliases ParserR6
ParserR6 = R6::R6Class("R6PARSERR6", inherit = Parser,
   public = list(
      #' @description Crea una instancia de la clase
      #' @details **Esta clase no se puede instanciar**
      #' @param object Instancia de objeto a analizar
      #' @param detail Nivel de detalle segun UMLShow
      #' @param deep   Nivel de profundidad del analisis
      #' @return La instancia del objeto
       initialize = function(object, detail, deep) {
          super$initialize(object, detail, deep)
          private$.getGenerators()  # Generadores
       }
       #' @description Ejecuta el analisis del objeto
       #' @return La definiciondel diagrama
      ,parse = function() {
          c0 = class(private$object)[1]
          p = R6CLASS$new(private$generators[c0], c0, 0)
          private$pend = list(p)
          names(private$pend) = p$name
          private$process()
          private$hecho
          #private$generateDefinition()
      }
   )
   ,private = list(
       generators = NULL
      ,process = function () {

         while (length(private$pend) > 0) {
            if (private$objSeen == 1) private$setFlags(FALSE)
            processed = private$parseClass()
            private$incObjects(processed)
            if (processed) {
               private$hecho[[length(private$hecho) + 1]] = private$pend[[1]]
               nm = names(private$hecho)
               if (is.null(nm)) nm = c("")   # Primera vez
               nm[length(nm)] = names(private$pend)[1]
               names(private$hecho) = nm
            }
            private$pend[[1]] = NULL
         }
      }
      ,parseClass = function () {
          cls = private$pend[[1]]

          if (cls$deep > private$maxDeep) return(FALSE)
          if (cls$name %in% names(private$hecho)) return (FALSE)

          if (private$isBasic()) return (private$parseOutput(cls))

          # Clase heredada. R6 no tiene multiple herencia
          if (cls$deep < private$maxDeep) {
              g = eval(parse(text=paste0(cls$name, "$get_inherit()")))
              if (!is.null(g) && private$incSuperClass()) {
                  p = R6CLASS$new(private$generators[g$classname], g$classname, cls$deep + 1)
                  private$addClass(p)
                  cls$addParent(p)
              }
          }
          # Fields & methods
          private$addAttributes(cls, TRUE)

          if (private$isComplete()) private$addAttributes(cls, FALSE)

          if (private$incSubClass()) {
             private$.getComposition(cls)
             private$.getAggregation(cls)
          }
          TRUE
      }
      ,parseOutput = function(cls) {
         if (cls$deep > 0) return (TRUE)
         data = trimws(capture.output(private$object))
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
         browser
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
            cls$addSubclasses(hijos, TRUE)
            private$addClass(hijos)
         }

         hijos = private$parseFunction(cls$name, "public_methods[\"initialize\"]")
         if (length(hijos) > 0) {
            hijos = unique(hijos)
            pos =
               hijos = lapply(hijos, function(x) { pos = which(private$generators == x)
               R6CLASS$new(x, names(private$generators)[pos], cls$deep + 1)})
            names(hijos) = sapply(hijos, function(x) x$name)
            cls$addSubclasses(hijos, TRUE)
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
            cls$addSubclasses(hijos, FALSE)
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
