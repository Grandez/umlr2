#' Clase base de los parsers
#' @title Parser
#' @name Parser
#' @rdname R6PARSER
#' @docType class
#' @description  Clase base para los parsers
PARSER = R6::R6Class("R6PARSER"
    ,inherit      = UMLR2BASE
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
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
        ,parse      = function() msg$err("E900", "parse")
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
        ,setMaxDeep = function(detail, deep) {
         # Ajusta la profundidad
         # 1.- Se empieza en 0 no en 1
         # 2.- Si no hay superclases (padres) la profundidad es 0
            if (deep > 0) deep = deep - 1
            res = ifelse(detail < UMLShow$superClasses, 0, deep)
            if (bitwAnd(detail, UMLShow$superClasses) > 0 && res == 0) res = 1
            if (bitwAnd(detail, UMLShow$subClasses)   > 0 && res == 0) res = 1
            res
        }
        ,isBasic       = function() { .det == 0 }
        ,isSimple      = function() { .det > 0 && bitwAnd(.det, UMLShow$complete) == 0}
        ,isComplete    = function() { bitwAnd(.det, UMLShow$complete)     > 0 }
        ,incSuperClass = function() { bitwAnd(.inc, UMLShow$superClasses) > 0 }
        ,incSubClass   = function() { bitwAnd(.inc, UMLShow$subClasses)   > 0 }
        ,setFlags = function(base) {
            #  Nibbles: 1 = 15, 2 = 240, 3 = 3840
            flags = ifelse(base, 15, 3840)
            .det = bitwAnd(.detail, flags)
            .inc = bitwAnd(.detail, 240)
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
        ,incObjects = function(processed) {
            private$.objSeen = private$.objSeen + 1
            if (processed)     private$.objDone = private$.objDone + 1
        }
        ,summary = function() {
            classes      = unlist(sapply(.hecho, function(x) if (x$isClass())      x$.generator))
            subclasses   = unlist(sapply(.hecho, function(x) if (x$isSubclass())   x$.generator))
            superclasses = unlist(sapply(.hecho, function(x) if (x$isSuperclass()) x$.generator))
            private$summ = list(classes=classes, subclasses=subclasses, superclasses=superclasses)
        }
    )
)
