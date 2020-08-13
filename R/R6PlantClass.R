#' La clase para gestionar el lenguaje de PlantUML
#' @title R6PlantSyntax
#' @docType class
#' @description  La clase para gestionar el lenguaje de PlantUML
PlantClass = R6::R6Class("R6PlantClass", inherit = PLANTUML,
  public = list(
      initialize = function(name, generator, type) {
          private$name = name
          private$generator = generator
          private$type = type
      }
      ,definition = function() {

      }
  )
  ,private = list(
       name = NULL
      ,generator = NULL
      ,type  = NULL
  )
)
