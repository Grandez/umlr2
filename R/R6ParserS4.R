#' La clase para interactuar con plantuml
#' @title ParserS4r
#' @docType class
#' @name ParserS4
ParserS4 = R6::R6Class("R6PARSERS4", inherit = PARSER,
   public = list(
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
