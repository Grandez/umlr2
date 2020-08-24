ParserBase = R6::R6Class("R6PARSERBase"
    ,portable     = FALSE
    ,lock_objects = FALSE
    ,lock_class   = FALSE
    ,public = list(
        initialize = function(object, detail, deep) {
            private$.object = object
            private$.detail = ifelse (missing(detail) || is.null(detail), 0, detail)
            private$.maxDeep = setMaxDeep(.detail, deep)
            private$.hecho = list()
            private$.pend = NULL
            setFlags(TRUE)
        }
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
            if (is.null(deep)) deep = 0
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
            private$.det = bitwAnd(.detail, flags)
            private$.inc = bitwAnd(.detail, 240)
        }
        ,incObjects = function(processed) {
            private$.objSeen = private$.objSeen + 1
            if (processed)     private$.objDone = private$.objDone + 1
        }

    )
)
