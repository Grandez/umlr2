#' Clase base para objetos R
#' @title RObject
#' @docType class

RObject = R6::R6Class("R6RObject",
   public = list(
        name=""
       ,type=0
       ,initialize = function(name,type) {
           self$name = name
           self$type = type
       }
   )
)
