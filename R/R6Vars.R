#' La clase para interactuar con plantuml
#' @title Variables
#' @name R6VARS
#' @aliases VARS
#' @docType class
#' @description  La descripcion.
VARS = R6::R6Class("R6VARSE"
   ,public = list(
        #' @field class Variable que referencia la clase principal
        class      = "__mainClass__"
        #' @field superClass Variable que referencia clases padre (inherit)
       ,superClass = "__superClass__"
       #' @field subClass Variable que referencia subclases (Composcion y agregacion)
       ,subClass   = "__subclass__"
       #' @field pattern Variable que referencia nombres de variables
       ,pattern    = "__[a-zA-Z0-9_]+__"
   )
)
