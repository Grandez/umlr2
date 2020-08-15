#' Clase base de los parsers
#' @title Parser
#' @name Parser
#' @aliases R6PARSER
#' @docType class
#' @description  Clase base para los parsers
Parser = R6::R6Class("R6PARSER", inherit = UMLR2Base,
    public  = list(
        #' @description Crea una instancia de la clase
        #' @details **Esta clase no se puede instanciar**
        #' @param object Instancia de objeto a analizar
        #' @param detail Nivel de detalle segun UMLShow
        #' @param deep   Nivel de profundidad del analisis
        #' @return La instancia del objeto
        initialize = function(object, detail, deep) {
            if (substr(as.character(sys.call(-1))[1], 1, 6) == "Parser") private$msg$err("E900")
            private$detail = detail
            private$object = object
            private$maxDeep = private$.setMaxDeep(detail, deep)
            private$hecho = list()
            private$pend = NULL
            private$setFlags(TRUE)
        }
    )
    ,private = list(
         object  = NULL
        ,detail  = NULL
        ,pend    = NULL
        ,hecho   = list()  # Para obtener NA la primera vez
        ,det     = 0       # Nivel de detalle
        ,inc     = 0       # Cosas a incluir
        ,maxDeep = 0
        ,objSeen = 0       # Objetos vistos
        ,objDone = 0       # Objetos procesados
        ,.setMaxDeep = function(detail, deep) {
            if (deep > 0) deep = deep -1
            res = ifelse(detail < UMLShow$parents, 0, deep)
            if (bitwAnd(detail, UMLShow$parents)    > 0 && res == 0) res = 1
            if (bitwAnd(detail, UMLShow$subclasses) > 0 && res == 0) res = 1
            res
        }
        ,isBasic       = function() { private$det == 0 }
        ,isSimple      = function() { private$det > 0 && bitwAnd(private$det, UMLShow$complete) == 0}
        ,isComplete    = function() { bitwAnd(private$det, UMLShow$complete)   > 0 }
        ,incSuperClass = function() { bitwAnd(private$inc, UMLShow$parents)    > 0 }
        ,incSubClass   = function() { bitwAnd(private$inc, UMLShow$subclasses) > 0 }
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

    )
)
