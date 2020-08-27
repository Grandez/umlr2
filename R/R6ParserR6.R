#' Parser de objetos de tipo R6
#' @title ParserR6
#' @name ParserR6
#' @rdname R6PARSERR6
#' @docType class
ParserR6 = R6::R6Class("PARSERR6"
   ,inherit      = ParserBase
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
      #' @description Crea una instancia de la clase
      #' @details **Esta clase no se puede instanciar**
      #' @param object Instancia de objeto a analizar
      #' @param detail Nivel de detalle segun UMLShow
      #' @param deep   Nivel de profundidad del analisis
      #' @return La instancia del objeto
       initialize = function(object, detail, deep) {
          super$initialize(object, detail, deep)
          private$.generators = getGenerators()  # Generadores
       }
       #' @description Ejecuta el analisis del objeto
       #' @return La definicion del diagrama
      ,parse = function() {
          c0 = class(.object)[1]
          p = RClass$new(.generators[c0], c0, .detail, 0, 1)
          private$.pend = list(p)
          names(private$.pend) = p$name
          process()
      }
   )
   ,private = list(
       .generators = NULL
      ,process = function () {
         while (length(.pend) > 0) {
            if (.objSeen == 1) setFlags(FALSE)
            processed = parseClass()
            incObjects(processed)
            if (processed) {
               private$.hecho[[length(private$.hecho) + 1]] = .pend[[1]]
               nm = names(.hecho)
               if (is.null(nm)) nm = c("")   # Primera vez
               nm[length(nm)] = names(.pend)[1]
               names(.hecho) = nm
            }
            private$.pend[[1]] = NULL
         }
         .hecho
      }
      ,parseClass = function () {          cls = .pend[[1]]

          if (cls$deep > .maxDeep) return(FALSE)
          if (cls$name %in% names(.hecho)) return (FALSE)

          #JGG if (isBasic()) return (parseOutput(cls))
return (parseOutput(cls))
          # Clase heredada. R6 no tiene multiple herencia
          if (cls$deep < .maxDeep) {
              g = eval(base::parse(text=paste0(cls$name, "$get_inherit()")))
              if (!is.null(g) && incSuperClass()) {
                  p = RClass$new(.generators[g$classname], g$classname, .detail, cls$deep + 1, 2)
                  addClass(p)
                  cls$addParent(p)
              }
          }
          # Fields & methods
          addAttributes(cls, TRUE)

          if (isComplete()) addAttributes(cls, FALSE)

          if (incSubClass()) {
             getComposition(cls)
             getAggregation(cls)
          }
          TRUE
      }
      ,parseOutput = function(cls) {
         browser()
         if (cls$deep > 0) return (TRUE)
         data = trimws(capture.output(.object))
         idx = grep("Public:", data, ignore.case=T)
         idxBeg = ifelse (length(idx) > 0, idx[1] + 1, 0)
         idx = grep("Private:", data, ignore.case=T)
         idxEnd = ifelse(length(idx) > 0, idx[1] - 1, length(data))
         if (idxBeg == 0) return
         data = data[idxBeg:idxEnd]
         mask = grepl("function[ ]*\\(", data)
         if (sum(mask) > 0)  cls$addMethods(clearOutput(data[mask]), TRUE)
         if (sum(!mask) == 0) return (TRUE)
         fields = data[!mask]
         mask = grepl("active binding", fields)
         cls$addBindings(clearOutput(fields[mask]))
         cls$addFields  (clearOutput(fields[!mask]), TRUE)
         TRUE
      }
      ,parseFunction = function(clsName, func) {

         init = eval(base::parse(text=paste0(clsName, "$", func)))
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
         sapply(cls, function(x) { nm = names(.pend)
                                   .pend[[length(.pend) + 1]] = x
                                   names(.pend) = c(nm, x$name[1])
                                 }, USE.NAMES=TRUE)
      }
      ,addAttributes      = function(cls, public) {
         vis = ifelse(public, "public", "private")
         cls$addFields  (eval(base::parse(text=paste0(cls$name, "$", vis, "_fields"))),  public)
         cls$addMethods (eval(base::parse(text=paste0(cls$name, "$", vis, "_methods"))), public)
         if (public) cls$addBindings(eval(base::parse(text=paste0(cls$name, "$active"))))
      }
      ,getGenerators = function () {
          pat = "[\\+\\-\\/\\*]"
          vars = ls(globalenv())
          vars = vars[!grepl(pat, vars)] # Quitar operadores sobrecargados
          classes = lapply(vars, function(x) eval(base::parse(text=paste0("class(", x ,")"))))
          names(classes) = vars
          classes = unlist(classes)
          c2 = classes[classes == "R6ClassGenerator"]
          # Obtenemos la clase que genera
          c2 = sapply(names(c2), function(x) eval(base::parse(text=paste0(x, "$classname"))), USE.NAMES=T)
          # Cambiamos nombres por valores
          nm = names(c2)
          names(nm) = c2
          nm
      }
      ,getComposition = function(cls) {
         # Campos
         fields = eval(base::parse(text=paste0(cls$name, "$public_fields")))
         fields = c(fields, eval(base::parse(text=paste0(cls$name, "$private_fields"))))

         # Si tiene inicializador a una clase, esta es un environment
         classes = names(grep("<environment>", fixed=TRUE, fields, value=T))
         clases = lapply(fields[classes], class)

         # Cogemos los primeros
         if (length(clases) > 0) {
            hijos = unique(unlist(lapply(clases, function(x) x[[1]])))
            hijos = lapply(hijos, function(x) RClass$new(.generators[hijos], x, detail, cls$deep + 1, 4))
            names(hijos) = unique(unlist(lapply(hijos, function(x) x$name)))
            cls$addSubclasses(hijos, TRUE)
            addClass(hijos)
         }

         hijos = parseFunction(cls$name, "public_methods[\"initialize\"]")
         if (length(hijos) > 0) {
            hijos = unique(hijos)
            hijos = lapply(hijos, function(x) { pos = which(.generators == x)
                                                RClass$new(x, names(.generators)[pos], detail, cls$deep + 1)
                                               })
            names(hijos) = unlist(lapply(hijos, function(x) x$name))
            cls$addSubclasses(hijos, TRUE)
            addClass(hijos)
         }
      }
      ,getAggregation = function(cls) {
         # Campos
         funcs = eval(base::parse(text=paste0(cls$name, "$public_methods")))
         funcs["initialize"] = NULL
         funcs["clone"] = NULL

         hijos1 = sapply(names(funcs), function(x) parseFunction(cls$name, paste0("public_methods[\"",x,"\"]")))

         funcs = eval(base::parse(text=paste0(cls$name, "$private_methods")))
         hijos2 = sapply(names(funcs), function(x) parseFunction(cls$name, paste0("private_methods[\"",x,"\"]")))
         hijos = c(Filter(Negate(is.null),hijos1), Filter(Negate(is.null),hijos2))
         if (is.list(hijos)) hijos = unlist(hijos)

         if (length(hijos) > 0) {
            hijos = unique(hijos)
            hijos = lapply(hijos, function(x) { pos = which(.generators == x)
            RClass$new(x, names(.generators)[pos], detail, cls$deep + 1)})
            names(hijos) = unlist(lapply(hijos, function(x) x$name))
            cls$addSubclasses(hijos, FALSE)
            addClass(hijos)
         }
      }
      ,clearOutput = function(data) {
         values = gsub(":.*", "", data)
         names(values) = values
         as.list(values)
      }

   )
)
