#' Clase base de los parsers
#  Es una pseudoclase. Tiene como un miembro estatico
#' @title Parser
#' @name Parser
#' @rdname R6PARSER
#' @docType class
#' @description  Clase base para los parsers
PARSER = R6::R6Class("R6PARSER"
    ,portable     = FALSE
    ,lock_objects = FALSE
    ,lock_class   = FALSE
    ,public  = list(
        #' @description Crea una instancia de la clase
        #' @param object Instancia de objeto a analizar
        #' @param detail Nivel de detalle segun UMLShow
        #' @param deep   Nivel de profundidad del analisis
        initialize = function(object, detail, deep) {
            #if (substr(as.character(sys.call(-1))[1], 1, 6) == "Parser") msg$err("E900", "Parser")
            private$.detail = detail
            private$.object = object
            private$.maxDeep = setMaxDeep(detail, deep)
            private$.hecho = list()
            private$.pend = NULL
            setFlags(TRUE)
        }
        ,print = function() {
            "Soy una definicion de PARSER"
        }
        #' @description Incluye los objetos analizados
        #' @param objects objetos analizados
        ,setObjects = function(objects) {
            private$.hecho = objects
            summary()
            invisible(self)
        }
        #' @description Genera la definicion del diagrama
        #' @return La definicion del diagrama
        ,generateDefinition = function() {
            # Al menos hay uno
            classes = lapply(seq(1,length(.hecho)), function(x) .hecho[[x]]$getClassDefinition(.detail))
            parents = lapply(seq(1,length(.hecho)), function(x) .hecho[[x]]$getParentsRelation())
            sons    = lapply(seq(1,length(.hecho)), function(x) .hecho[[x]]$getSubclassesRelation())
            layers = generateLayers()
            c(unlist(classes), layers, unique(unlist(parents)), unique(unlist(sons)))
        }
        #' @description Obtiene la informacion resumida de los objetos analizados
        ,getSummary = function() summ
        #' @description Interfaz para analizar los objetos
        # ,parse      = function() msg$err("E900", "parse")
    )
    ,private = list(
         .object = NULL
        ,.detail = NULL
        ,.pend    = NULL
        ,.hecho   = list()  # Para obtener NA la primera vez
        ,summ    = list()
        ,.det     = 0       # Nivel de detalle
        ,.inc     = 0       # Cosas a incluir
        ,.maxDeep = 0
        ,.objSeen = 0       # Objetos vistos
        ,.objDone = 0       # Objetos procesados
        ,internalParser = function(obj, detail, deep) {
           checkObject(obj)
           tmpParser = getParser(obj, detail, deep)
           if (is.null(tmpParser)) return (NULL)
           tmpParser$parse(obj, detail, deep)

      }

        ,generateLayers = function() {
            l = lapply(seq(1,length(.hecho)), function(x) .hecho[[x]]$deep)
            names(l) = names(.hecho)
            levels = unlist(l)
            ll = sapply(seq(0,max(levels)), function(x) levels[levels == x], USE.NAMES = TRUE)
            # Solo cuando hay mas de una clase
            ll = ll[sapply(ll, function(x) length(x) > 1)]
            classes = lapply(ll, function (x) paste("   class ", names(x), collapse="\n"))
            layers = lapply(classes, function(x) paste("together {\n", x, "\n}\n", collapse="\n"))
            unlist(layers)

        }
        ,summary = function() {
            classes      = unlist(sapply(.hecho, function(x) if (x$isClass())      x$.generator))
            subclasses   = unlist(sapply(.hecho, function(x) if (x$isSubclass())   x$.generator))
            superclasses = unlist(sapply(.hecho, function(x) if (x$isSuperclass()) x$.generator))
            private$summ = list(classes=classes, subclasses=subclasses, superclasses=superclasses)
        }
        ,checkObject = function(object) {
           # Si es un unico objeto deberia tener class informado
           # si no es un objeto, deberia tener una longitud mayor que 1
           if (length(object) == 1) {
               if (!is.object(object)) msg$err("R020", as.character(sys.call(-2))[2])
           }
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

    )
)
PARSER$parse = function(object, detail=0, deep=0) {
    if (isS4(object))      return (ParserS4$new(object, detail, deep)$parse())
    if (R6::is.R6(object)) return (ParserR6$new(object, detail, deep)$parse())
    if (is.environment(object)) {
        if (isS4(object$self))      return (ParserS4$new(object$self, detail, deep)$parse())
        if (R6::is.R6(object$self)) return (ParserR6$new(object$self, detail, deep)$parse())
     }
     msg$warning("W810")
     NULL
}

PARSER$lock_class = TRUE
PARSER$lock_objects = TRUE
