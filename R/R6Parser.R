#' Clase base de los parsers
#' @title Parser
#' @name Parser
#' @rdname R6PARSER
#' @docType class
#' @description  Clase base para los parsers
PARSER = R6::R6Class("R6PARSER", inherit = UMLR2BASE,
    public  = list(
        #' @description Crea una instancia de la clase
        #' @details **Esta clase no se puede instanciar**
        #' @param object Instancia de objeto a analizar
        #' @param detail Nivel de detalle segun UMLShow
        #' @param deep   Nivel de profundidad del analisis
        #' @return La instancia del objeto
        initialize = function(object, detail, deep) {
            #if (substr(as.character(sys.call(-1))[1], 1, 6) == "Parser") private$msg$err("E900", "Parser")
            private$detail = detail
            private$object = object
            private$maxDeep = private$.setMaxDeep(detail, deep)
            private$hecho = list()
            private$pend = NULL
            private$setFlags(TRUE)
        }
        ,setObjects = function(objects) {
            private$hecho = objects
            private$summary()
            invisible(self)
        }
        ,generateDefinition = function() {
            # Al menos hay uno
            classes = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getClassDefinition(private$detail))
            parents = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getParentsRelation())
            sons    = lapply(seq(1,length(private$hecho)), function(x) private$hecho[[x]]$getSubclassesRelation())

            c(private$.header, unlist(classes), unlist(parents), unlist(sons))
        }
        ,getSummary = function() private$summ
    )
    ,private = list(
         object  = NULL
        ,detail  = NULL
        ,pend    = NULL
        ,hecho   = list()  # Para obtener NA la primera vez
        ,summ    = list()
        ,det     = 0       # Nivel de detalle
        ,inc     = 0       # Cosas a incluir
        ,maxDeep = 0
        ,objSeen = 0       # Objetos vistos
        ,objDone = 0       # Objetos procesados
        ,.setMaxDeep = function(detail, deep) {
         # Ajusta la profundidad
         # 1.- Se empieza en 0 no en 1
         # 2.- Si no hay superclases (padres) la profundidad es 0
            if (deep > 0) deep = deep - 1
            res = ifelse(detail < UMLShow$superClasses, 0, deep)
            if (bitwAnd(detail, UMLShow$superClasses) > 0 && res == 0) res = 1
            if (bitwAnd(detail, UMLShow$subClasses)   > 0 && res == 0) res = 1
            res
        }
        ,isBasic       = function() { private$det == 0 }
        ,isSimple      = function() { private$det > 0 && bitwAnd(private$det, UMLShow$complete) == 0}
        ,isComplete    = function() { bitwAnd(private$det, UMLShow$complete)     > 0 }
        ,incSuperClass = function() { bitwAnd(private$inc, UMLShow$superClasses) > 0 }
        ,incSubClass   = function() { bitwAnd(private$inc, UMLShow$subClasses)   > 0 }
        ,setFlags = function(base) {
            #  Nibbles: 1 = 15, 2 = 240, 3 = 3840
            flags = ifelse(base, 15, 3840)
            private$det = bitwAnd(private$detail, flags)
            private$inc = bitwAnd(private$detail, 240)
        }
        ,incObjects = function(processed) {
            private$objSeen = private$objSeen + 1
            if (processed)    private$objDone = private$objDone + 1
        }
        ,summary = function() {
            classes      = unlist(sapply(private$hecho, function(x) if (x$isClass()) x$generator))
            subclasses   = unlist(sapply(private$hecho, function(x) if (x$isSubclass()) x$generator))
            superclasses = unlist(sapply(private$hecho, function(x) if (x$isSuperclass()) x$generator))
            private$summ = list(classes=classes, subclasses=subclasses, superclasses=superclasses)
        }
    )
)
