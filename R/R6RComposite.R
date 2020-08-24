#' Packages y Namespaces son objetos compuestos
#' @title "Clase Composite"
#' @name "RComposite"
#' @rdname RComposite
#' @docType class
#' @description  La descripcion.
RComposite = R6::R6Class("RCOMPOSITE"
    ,inherit      = RComponent
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public = list(
        #' @description Crea una instancia de la clase
        #' @param name  Nombre de la clase
        #' @param generator Objeto generados
        #' @param detail Nivel de detalle generado
        #' @param deep   Nivel de profundidad
        #' @param type   Tipo de clase
        initialize = function(name, ..., style = NULL) {
            self$name      = name
            private$style = style
            add(...)
        }
        ,add = function(...) {
            data = list(...)
            for (obj in list(...)) {
                if (is.character(obj)) private$classNames = c(private$classNames, obj)
            }
        }
        ,getClassNames = function(remove = TRUE) {
            nm = classNames
            if (remove) private$classNames = c()
            nm
        }
        ,definition = function() {
            def = c(paste(lang$package, name, lang$lblock, "\n"))
            for (class in classes) {
                def = c(def, class$definition(), "\n")
            }
            c(def, lang$rblock)
        }

    )
   ,private = list(
       classes = list()
      ,classNames = c()
      ,style = NULL
   )
)
