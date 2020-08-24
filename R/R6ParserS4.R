#' Parser de objetos de tipo S4
#' @title ParserS4
#' @name ParserS4
#' @rdname R6PARSERS4
#' @docType class
PARSERS4 = R6::R6Class("R6PARSERS4", inherit = PARSER, portable = FALSE, lock_objects = TRUE, lock_class = TRUE
   ,public = list(
      #' @description Crea una instancia de la clase
      #' @details **Esta clase no se puede instanciar**
      #' @param object Instancia de objeto a analizar
      #' @param detail Nivel de detalle segun UMLShow
      #' @param deep   Nivel de profundidad del analisis
      #' @return La instancia del objeto
       initialize = function(object, detail, deep) {
          super$initialize(object, detail, deep)
       }
       #' @description Ejecuta el analisis del objeto
       #' @return La definiciondel diagrama
      ,parse = function() {
      }
   )
   ,private = list(
   )
)
